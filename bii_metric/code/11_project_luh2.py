#!/usr/bin/env python3
"""Project fitted PREDICTS BII response functions onto LUH2 state fractions.

The code intentionally projects the two components in the De Palma tutorial
separately at each 0.25-degree cell and then multiplies them.  It avoids
materialising a multi-million-row land-use CSV by reading the two requested
LUH2 NetCDF slices directly.
"""

from __future__ import annotations

import argparse
import csv
import math
from collections import defaultdict
from pathlib import Path

import numpy as np
from netCDF4 import Dataset


LUH2_CLASSES = {
    "primary_minimal": ("primf", "primn"),
    "secondary": ("secdf", "secdn"),
    "cropland": ("c3ann", "c4ann", "c3per", "c4per", "c3nfx"),
    "pasture": ("pastr", "range"),
    "urban": ("urban",),
}
REQUIRED_CLASSES = tuple(LUH2_CLASSES)
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--historical", type=Path, required=True)
    parser.add_argument("--future", type=Path, required=True)
    parser.add_argument("--static", type=Path, required=True)
    parser.add_argument("--grid-lookup", type=Path, required=True)
    parser.add_argument("--responses", type=Path, required=True)
    parser.add_argument("--published", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument(
        "--years", default="2010,2030",
        help="Comma-separated LUH2 years to project (default: 2010,2030). Years through 2015 use v2h; later years use v2f.",
    )
    parser.add_argument(
        "--output-prefix", default="bii_luh2",
        help="Filename prefix for output tables (default: bii_luh2).",
    )
    return parser.parse_args()


def parse_year_specs(years_text: str):
    try:
        years = [int(value.strip()) for value in years_text.split(",") if value.strip()]
    except ValueError as error:
        raise ValueError("--years must be a comma-separated list of integer years.") from error
    if not years or len(years) != len(set(years)):
        raise ValueError("--years must contain one or more unique years.")
    specs = []
    for year in years:
        if year <= 2015:
            specs.append((year, "historical", "historical"))
        else:
            specs.append((year, "ssp2rcp4p5messageglobiom", "future"))
    return tuple(specs)


def read_csv_rows(path: Path):
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, rows, fieldnames) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def finite_array(variable, index=None) -> np.ndarray:
    data = variable[:] if index is None else variable[index]
    return np.ma.filled(data, np.nan).astype(np.float64, copy=False)


def time_index(dataset: Dataset, year: int) -> int:
    units = str(getattr(dataset.variables["time"], "units", ""))
    if not units.startswith("years since "):
        raise ValueError(f"Unsupported LUH2 time units: {units}")
    # LUH2 uses CF strings such as "years since 850-01-01".
    first_year = int(units.split("years since ", 1)[1].split()[0].split("-", 1)[0])
    index = year - first_year
    if index < 0 or index >= len(dataset.dimensions["time"]):
        raise ValueError(f"Year {year} is not present in {dataset.filepath()}")
    return index


def load_response_models(path: Path):
    rows = read_csv_rows(path)
    models = {}
    for row in rows:
        model = models.setdefault(
            row["model_id"],
            {
                "model_id": row["model_id"],
                "scope_type": row["scope_type"],
                "scope_value": row["scope_value"],
                "responses": {},
            },
        )
        model["responses"][row["pressure_class"]] = (
            float(row["abundance_relative"]),
            float(row["composition_relative"]),
        )
    if "global__all" not in models:
        raise ValueError("The BII response table needs a global__all model for fallback and validation.")
    global_responses = models["global__all"]["responses"]
    missing_global = set(REQUIRED_CLASSES) - set(global_responses)
    if missing_global:
        raise ValueError(f"The global response model lacks LUH2 classes: {sorted(missing_global)}")
    return models, global_responses


def load_lookup(path: Path, n_cells: int):
    iso = np.full(n_cells, "", dtype="<U8")
    country = np.full(n_cells, "", dtype="<U80")
    continent = np.full(n_cells, "", dtype="<U24")
    predicts_region = np.full(n_cells, "", dtype="<U16")
    assignment = np.full(n_cells, "", dtype="<U20")
    for row in read_csv_rows(path):
        grid_id = int(row["grid_id"])
        if grid_id < 0 or grid_id >= n_cells:
            raise ValueError(f"Grid id outside LUH2 domain: {grid_id}")
        iso[grid_id] = row.get("country_iso3", "")
        country[grid_id] = row.get("country", "")
        continent[grid_id] = row.get("continent", "")
        predicts_region[grid_id] = row.get("predicts_region", "")
        assignment[grid_id] = row.get("country_assignment", "")
    return iso, country, continent, predicts_region, assignment


def mean_or_nan(values: np.ndarray, weights: np.ndarray) -> float:
    total_weight = float(weights.sum())
    if total_weight <= 0:
        return math.nan
    return float(np.dot(values, weights) / total_weight)


def aggregate(values, abundance, similarity, weights, groups, labels, base_row):
    """Return area-weighted rows for a categorical group vector."""
    groups = np.asarray(groups)
    keys, inverse = np.unique(groups, return_inverse=True)
    total_weights = np.bincount(inverse, weights=weights)
    counts = np.bincount(inverse)
    bii_sum = np.bincount(inverse, weights=weights * values)
    bounded_sum = np.bincount(inverse, weights=weights * np.clip(values, 0, 1))
    abundance_sum = np.bincount(inverse, weights=weights * abundance)
    similarity_sum = np.bincount(inverse, weights=weights * similarity)
    rows = []
    for index, key in enumerate(keys):
        if not key or total_weights[index] <= 0:
            continue
        row = dict(base_row)
        row.update(labels(str(key)))
        row.update(
            bii=float(bii_sum[index] / total_weights[index]),
            bii_bounded=float(bounded_sum[index] / total_weights[index]),
            relative_abundance=float(abundance_sum[index] / total_weights[index]),
            compositional_similarity=float(similarity_sum[index] / total_weights[index]),
            n_cells=int(counts[index]),
            land_area_km2=float(total_weights[index]),
        )
        rows.append(row)
    return rows


def global_row(values, abundance, similarity, weights, base_row):
    row = dict(base_row)
    row.update(
        bii=mean_or_nan(values, weights),
        bii_bounded=mean_or_nan(np.clip(values, 0, 1), weights),
        relative_abundance=mean_or_nan(abundance, weights),
        compositional_similarity=mean_or_nan(similarity, weights),
        n_cells=int(values.size),
        land_area_km2=float(weights.sum()),
    )
    return row


def correlation(x, y):
    if len(x) < 2 or np.std(x) == 0 or np.std(y) == 0:
        return math.nan
    return float(np.corrcoef(x, y)[0, 1])


def rank(values):
    order = np.argsort(values, kind="mergesort")
    ranks = np.empty(len(values), dtype=float)
    ranks[order] = np.arange(len(values), dtype=float)
    sorted_values = np.asarray(values)[order]
    start = 0
    while start < len(values):
        end = start + 1
        while end < len(values) and sorted_values[end] == sorted_values[start]:
            end += 1
        ranks[order[start:end]] = (start + end - 1) / 2
        start = end
    return ranks


def validation_rows(global_rows, region_rows, country_rows, published_rows):
    published = {}
    published_country = {}
    for row in published_rows:
        if row.get("variable") != "bii":
            continue
        try:
            key = (row["scenario"], int(row["year"]), row["area_code"])
            value = float(row["value"])
            published[key] = value
            # Country area codes include their full UN-region hierarchy, e.g.
            # 001-019-419-005-COL, rather than just 001-COL.
            country_code = row["area_code"].rsplit("-", 1)[-1]
            if len(country_code) == 3:
                published_country[(row["scenario"], int(row["year"]), country_code)] = value
        except (KeyError, ValueError):
            continue

    scenarios = {2010: "historical", 2030: "ssp2rcp4p5messageglobiom"}
    validation = {"global": [], "regions": [], "countries": []}
    for row in global_rows:
        if row["model_id"] != "global__all" or row["scope_type"] != "global":
            continue
        key = (row["scenario"], int(row["year"]), "global")
        if key in published:
            validation["global"].append({
                "scenario": row["scenario"], "year": row["year"], "area_code": "global",
                "modelled_bii": row["bii"], "modelled_bii_bounded": row["bii_bounded"],
                "published_bii": published[key], "difference": row["bii"] - published[key],
            })
    published_region_codes = {
        "Africa": "001-002", "Americas": "001-019", "Asia": "001-142",
        "Europe": "001-150", "Oceania": "001-009",
    }
    for row in region_rows:
        if row["model_id"] != "global__all" or row["scope_type"] != "global":
            continue
        area_code = published_region_codes.get(row["predicts_region"])
        key = (row["scenario"], int(row["year"]), area_code)
        if area_code and key in published:
            validation["regions"].append({
                "scenario": row["scenario"], "year": row["year"], "region": row["predicts_region"],
                "area_code": area_code, "modelled_bii": row["bii"],
                "modelled_bii_bounded": row["bii_bounded"], "published_bii": published[key],
                "difference": row["bii"] - published[key],
            })
    for row in country_rows:
        if row["model_id"] != "global__all" or row["scope_type"] != "global":
            continue
        iso3 = row["country_iso3"]
        if len(iso3) != 3:
            continue
        key = (row["scenario"], int(row["year"]), iso3)
        if key in published_country:
            validation["countries"].append({
                "scenario": row["scenario"], "year": row["year"], "country_iso3": iso3,
                "country": row["country"], "modelled_bii": row["bii"],
                "modelled_bii_bounded": row["bii_bounded"], "published_bii": published_country[key],
                "difference": row["bii"] - published_country[key],
            })

    summaries = []
    for level, rows in validation.items():
        if rows:
            modelled = np.array([row["modelled_bii"] for row in rows])
            published_values = np.array([row["published_bii"] for row in rows])
            differences = modelled - published_values
            summaries.append({
                "comparison": level,
                "n_matched_rows": len(rows),
                "mean_absolute_difference": float(np.mean(np.abs(differences))),
                "mean_signed_difference": float(np.mean(differences)),
                "pearson_correlation": correlation(modelled, published_values),
                "spearman_correlation": correlation(rank(modelled), rank(published_values)),
            })
        else:
            summaries.append({
                "comparison": level, "n_matched_rows": 0,
                "mean_absolute_difference": math.nan, "mean_signed_difference": math.nan,
                "pearson_correlation": math.nan, "spearman_correlation": math.nan,
            })
    return validation, summaries


def paired_report(rows, dimensions):
    """Make a compact 2010/2030 table for the requested reporting level."""
    grouped = defaultdict(dict)
    for row in rows:
        grouped[tuple(row[dimension] for dimension in dimensions)][int(row["year"])] = row
    report = []
    for key in sorted(grouped):
        by_year = grouped[key]
        if 2010 not in by_year or 2030 not in by_year:
            continue
        baseline, future = by_year[2010], by_year[2030]
        row = {dimension: value for dimension, value in zip(dimensions, key)}
        row.update(
            bii_2010=baseline["bii_bounded"],
            bii_2030=future["bii_bounded"],
            change_2030_minus_2010=future["bii_bounded"] - baseline["bii_bounded"],
            response_mode_2010=baseline["response_mode"],
            response_mode_2030=future["response_mode"],
            fallback_pressure_classes=baseline["fallback_pressure_classes"],
        )
        report.append(row)
    return report


def project_year(dataset_path, year, scenario, static_area, lookup, models, global_responses):
    with Dataset(dataset_path) as dataset:
        index = time_index(dataset, year)
        arrays = {}
        for pressure_class, variables in LUH2_CLASSES.items():
            arrays[pressure_class] = sum(
                finite_array(dataset.variables[variable], index) for variable in variables
            )
    total = sum(arrays.values())
    valid = np.isfinite(total) & (total > 0) & np.isfinite(static_area) & (static_area > 0)
    shares = {name: values[valid] / total[valid] for name, values in arrays.items()}
    share_sum = sum(shares.values())
    if not np.allclose(share_sum, 1, atol=3e-6, rtol=0):
        raise ValueError(f"LUH2 land-use shares do not sum to one for {year}.")

    flat_valid = np.flatnonzero(valid.ravel())
    iso, country, continent, predicts_region, assignment = lookup
    # The reporting domain follows the five PREDICTS/NHM regions. Antarctica and
    # cells without a modern country/continent assignment are not part of BII.
    reporting_domain = predicts_region[flat_valid] != ""
    valid_cells = flat_valid[reporting_domain]
    weights = (static_area.ravel()[valid_cells] * total.ravel()[valid_cells]).astype(np.float64)
    # `shares` has already been restricted to the valid cells, so subset it by
    # the reporting-domain mask rather than by its original grid indices.
    shares = {name: values[reporting_domain] for name, values in shares.items()}
    iso = iso[valid_cells]
    country = country[valid_cells]
    continent = continent[valid_cells]
    predicts_region = predicts_region[valid_cells]
    assignment = assignment[valid_cells]

    common = {
        "data_source": "LUH2 v2",
        "scenario": scenario,
        "year": int(year),
        "landuse_crosswalk": "primf+primn=primary_minimal; secdf+secdn=secondary; crop states=cropland; pastr+range=pasture; urban=urban",
    }
    global_rows, country_rows, continent_rows, published_region_rows, coverage_rows = [], [], [], [], []
    country_names = {key: name for key, name in zip(iso, country) if key}
    continent_names = {key: key for key in np.unique(continent) if key}

    for model in models.values():
        fallback = sorted(set(REQUIRED_CLASSES) - set(model["responses"]))
        response = dict(model["responses"])
        for pressure_class in fallback:
            response[pressure_class] = global_responses[pressure_class]
        abundance_weights = np.array([response[name][0] for name in REQUIRED_CLASSES])
        composition_weights = np.array([response[name][1] for name in REQUIRED_CLASSES])
        stacked_shares = np.vstack([shares[name] for name in REQUIRED_CLASSES])
        abundance = abundance_weights @ stacked_shares
        similarity = composition_weights @ stacked_shares
        bii = abundance * similarity
        scope_mask = np.ones(bii.size, dtype=bool)
        if model["scope_type"] == "region":
            scope_mask = predicts_region == model["scope_value"]
        if not np.any(scope_mask):
            continue
        base = dict(common)
        base.update(
            model_id=model["model_id"], scope_type=model["scope_type"], scope_value=model["scope_value"],
            response_mode="strict" if not fallback else "hybrid_global_fallback",
            fallback_pressure_classes=";".join(fallback),
        )
        coverage_rows.append({
            **base,
            "available_pressure_classes": ";".join(sorted(model["responses"])),
            "n_luh2_cells": int(scope_mask.sum()),
            "n_nearest_boundary_cells": int(np.sum(assignment[scope_mask] == "nearest_boundary")),
        })
        global_rows.append(global_row(bii[scope_mask], abundance[scope_mask], similarity[scope_mask], weights[scope_mask], base))
        country_rows.extend(aggregate(
            bii[scope_mask], abundance[scope_mask], similarity[scope_mask], weights[scope_mask], iso[scope_mask],
            lambda key: {"country_iso3": key, "country": country_names.get(key, "")}, base
        ))
        continent_rows.extend(aggregate(
            bii[scope_mask], abundance[scope_mask], similarity[scope_mask], weights[scope_mask], continent[scope_mask],
            lambda key: {"continent": key}, base
        ))
        published_region_rows.extend(aggregate(
            bii[scope_mask], abundance[scope_mask], similarity[scope_mask], weights[scope_mask], predicts_region[scope_mask],
            lambda key: {"predicts_region": key}, base
        ))
    return global_rows, country_rows, continent_rows, published_region_rows, coverage_rows


def main() -> None:
    args = parse_args()
    year_specs = parse_year_specs(args.years)
    if not args.output_prefix or Path(args.output_prefix).name != args.output_prefix:
        raise ValueError("--output-prefix must be a simple filename prefix, not a path.")
    for path in (args.historical, args.future, args.static, args.grid_lookup, args.responses, args.published):
        if not path.exists():
            raise FileNotFoundError(path)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    with Dataset(args.static) as static:
        cell_area = finite_array(static.variables["carea"])
    n_cells = cell_area.size
    lookup = load_lookup(args.grid_lookup, n_cells)
    models, global_responses = load_response_models(args.responses)

    all_global, all_countries, all_continents, all_regions, all_coverage = [], [], [], [], []
    for year, scenario, source in year_specs:
        source_path = args.historical if source == "historical" else args.future
        results = project_year(source_path, year, scenario, cell_area, lookup, models, global_responses)
        all_global.extend(results[0])
        all_countries.extend(results[1])
        all_continents.extend(results[2])
        all_regions.extend(results[3])
        all_coverage.extend(results[4])

    common_fields = [
        "data_source", "scenario", "year", "model_id", "scope_type", "scope_value", "response_mode",
        "fallback_pressure_classes", "landuse_crosswalk", "bii", "bii_bounded", "relative_abundance",
        "compositional_similarity", "n_cells", "land_area_km2",
    ]
    output = lambda suffix: args.output_dir / f"{args.output_prefix}_{suffix}.csv"
    write_csv(output("global"), all_global, common_fields)
    write_csv(output("countries"), all_countries, common_fields[:9] + ["country_iso3", "country"] + common_fields[9:])
    write_csv(output("continents"), all_continents, common_fields[:9] + ["continent"] + common_fields[9:])
    write_csv(output("published_regions"), all_regions, common_fields[:9] + ["predicts_region"] + common_fields[9:])
    coverage_fields = [
        "data_source", "scenario", "year", "model_id", "scope_type", "scope_value", "response_mode",
        "fallback_pressure_classes", "landuse_crosswalk", "available_pressure_classes", "n_luh2_cells",
        "n_nearest_boundary_cells",
    ]
    write_csv(output("model_coverage"), all_coverage, coverage_fields)
    taxon_rows = [row for row in all_global if row["scope_type"] == "taxon"]
    write_csv(output("taxa"), taxon_rows, common_fields)

    validation, summary = validation_rows(all_global, all_regions, all_countries, read_csv_rows(args.published))
    validation_fields = {
        "global": ["scenario", "year", "area_code", "modelled_bii", "modelled_bii_bounded", "published_bii", "difference"],
        "regions": ["scenario", "year", "region", "area_code", "modelled_bii", "modelled_bii_bounded", "published_bii", "difference"],
        "countries": ["scenario", "year", "country_iso3", "country", "modelled_bii", "modelled_bii_bounded", "published_bii", "difference"],
    }
    for level, rows in validation.items():
        write_csv(output(f"validation_{level}_vs_published"), rows, validation_fields[level])
    write_csv(
        output("validation_summary"), summary,
        ["comparison", "n_matched_rows", "mean_absolute_difference", "mean_signed_difference", "pearson_correlation", "spearman_correlation"],
    )
    # Clean reporting tables: national and continental values use the one
    # global aggregate-taxa response function, while the taxon table uses the
    # respective group models (and retains their explicit fallback labels).
    global_report = paired_report(
        [row for row in all_global if row["model_id"] == "global__all" and row["scope_type"] == "global"],
        ["model_id"],
    )
    published_global = {(row["year"]): row for row in validation["global"]}
    for row in global_report:
        for year in (2010, 2030):
            published_row = published_global.get(year)
            row[f"published_bii_{year}"] = published_row["published_bii"] if published_row else math.nan
            row[f"difference_vs_published_{year}"] = (
                row[f"bii_{year}"] - published_row["published_bii"] if published_row else math.nan
            )
    report_fields = [
        "bii_2010", "bii_2030", "change_2030_minus_2010", "response_mode_2010", "response_mode_2030",
        "fallback_pressure_classes",
    ]
    write_csv(
        output("report_global"), global_report,
        ["model_id"] + report_fields + ["published_bii_2010", "difference_vs_published_2010", "published_bii_2030", "difference_vs_published_2030"],
    )
    national_report = paired_report(
        [row for row in all_countries if row["model_id"] == "global__all" and row["scope_type"] == "global"],
        ["country_iso3", "country"],
    )
    write_csv(output("report_national"), national_report, ["country_iso3", "country"] + report_fields)
    continent_report = paired_report(
        [row for row in all_continents if row["model_id"] == "global__all" and row["scope_type"] == "global"],
        ["continent"],
    )
    write_csv(output("report_continents"), continent_report, ["continent"] + report_fields)
    taxon_report = paired_report(
        [row for row in all_global if row["scope_type"] == "taxon"],
        ["scope_value"],
    )
    write_csv(output("report_taxa"), taxon_report, ["scope_value"] + report_fields)
    metadata = [
        {"key": "historical_file", "value": str(args.historical)},
        {"key": "future_file", "value": str(args.future)},
        {"key": "future_scenario", "value": "LUH2 SSP2-RCP4.5 MESSAGE-GLOBIOM"},
        {"key": "primary_mapping", "value": "primf + primn -> primary_minimal; LUH2 does not resolve use intensity"},
        {"key": "plantation_mapping", "value": "No plantation state in LUH2 v2 state file; no plantation area is imputed"},
        {"key": "area_weight", "value": "carea * total LUH2 land-state fraction (ice/water excluded)"},
        {"key": "country_method", "value": "Natural Earth 1:50m modern country boundaries at LUH2 cell centres; nearest-boundary fallback for coastal cells"},
        {"key": "taxon_missing_classes", "value": "Taxon/region models with unavailable land-use classes use global response fallback and are labelled hybrid_global_fallback"},
    ]
    metadata.append({"key": "projected_years", "value": args.years})
    metadata.append({"key": "output_prefix", "value": args.output_prefix})
    write_csv(output("metadata"), metadata, ["key", "value"])
    print(f"Wrote LUH2 BII tables to {args.output_dir}")


if __name__ == "__main__":
    main()
