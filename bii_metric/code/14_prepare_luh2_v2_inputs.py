#!/usr/bin/env python3
"""Create manageable 0.5-degree LUH2 V2 land-use and population inputs.

The V2 comparison uses the same LUH2 2010 historical and SSP2-RCP4.5 2030
states as V1.  It aggregates the 0.25-degree state fractions to 0.5 degree so
that they align with the public 0.125-degree NCAR SSP population grid while
keeping the R mixed-model projection tractable.  The intensity assignment is
explicitly recorded in the accompanying crosswalk, not inferred from LUH2.
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

import numpy as np
from netCDF4 import Dataset


LUH2_CLASSES = {
    "primary": ("primf", "primn"),
    "secondary": ("secdf", "secdn"),
    "cropland": ("c3ann", "c4ann", "c3per", "c4per", "c3nfx"),
    "pasture": ("pastr", "range"),
    "urban": ("urban",),
}
YEAR_SPECS = (
    (2010, "historical", "historical"),
    (2030, "ssp2rcp4p5messageglobiom", "future"),
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--historical", type=Path, required=True)
    parser.add_argument("--future", type=Path, required=True)
    parser.add_argument("--static", type=Path, required=True)
    parser.add_argument("--grid-lookup", type=Path, required=True)
    parser.add_argument("--population-2010", type=Path, required=True)
    parser.add_argument("--population-2030", type=Path, required=True)
    parser.add_argument("--landuse-output", type=Path, required=True)
    parser.add_argument("--pressure-output", type=Path, required=True)
    return parser.parse_args()


def finite(variable, index=None) -> np.ndarray:
    values = variable[:] if index is None else variable[index]
    return np.ma.filled(np.ma.asarray(values, dtype=np.float64), np.nan)


def time_index(dataset: Dataset, year: int) -> int:
    units = str(getattr(dataset.variables["time"], "units", ""))
    if not units.startswith("years since "):
        raise ValueError(f"Unsupported LUH2 time units: {units}")
    first_year = int(units.split("years since ", 1)[1].split()[0].split("-", 1)[0])
    index = year - first_year
    if index < 0 or index >= len(dataset.dimensions["time"]):
        raise ValueError(f"Year {year} is not present in {dataset.filepath()}")
    return index


def block_sum(values: np.ndarray) -> np.ndarray:
    if values.shape != (720, 1440):
        raise ValueError(f"Expected 0.25-degree LUH2 grid (720, 1440), found {values.shape}")
    return values.reshape(360, 2, 720, 2).sum(axis=(1, 3))


def block_mode(labels: np.ndarray, weights: np.ndarray) -> np.ndarray:
    blocks = labels.reshape(360, 2, 720, 2).transpose(0, 2, 1, 3).reshape(360, 720, 4)
    block_weights = weights.reshape(360, 2, 720, 2).transpose(0, 2, 1, 3).reshape(360, 720, 4)
    winner = block_weights.argmax(axis=2)[..., None]
    chosen = np.take_along_axis(blocks, winner, axis=2)[..., 0]
    # A valid land block should have a label. If its maximum-area 0.25 degree
    # component did not, use any non-empty component before reporting it.
    missing = chosen == ""
    if missing.any():
        for candidate in range(4):
            replacement = blocks[..., candidate]
            chosen[(missing) & (replacement != "")] = replacement[(missing) & (replacement != "")]
    return chosen


def aggregate_population(population_path: Path) -> np.ndarray:
    with Dataset(population_path) as dataset:
        value_names = [name for name in dataset.variables if name not in {"lon", "lat"}]
        if len(value_names) != 1:
            raise ValueError(f"Expected one population variable in {population_path}, found {value_names}")
        counts = finite(dataset.variables[value_names[0]])
        lats = finite(dataset.variables["lat"])
        lons = finite(dataset.variables["lon"])
    if counts.shape != (len(lats), len(lons)):
        raise ValueError(f"Population grid dimensions do not match coordinates in {population_path}")
    rows = np.floor((90 - lats) / 0.5).astype(int)
    cols = np.floor((lons + 180) / 0.5).astype(int)
    if np.any(rows < 0) or np.any(rows >= 360) or np.any(cols < 0) or np.any(cols >= 720):
        raise ValueError(f"Population grid is outside the expected global 0.5-degree domain: {population_path}")
    output = np.zeros((360, 720), dtype=np.float64)
    counts = np.nan_to_num(counts, nan=0.0, posinf=0.0, neginf=0.0)
    for source_row, target_row in enumerate(rows):
        np.add.at(output[target_row], cols, counts[source_row])
    return output


def read_lookup(path: Path) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    country = np.full(720 * 1440, "", dtype="<U8")
    continent = np.full(720 * 1440, "", dtype="<U24")
    region = np.full(720 * 1440, "", dtype="<U16")
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            grid_id = int(row["grid_id"])
            country[grid_id] = row.get("country_iso3", "")
            continent[grid_id] = row.get("continent", "")
            region[grid_id] = row.get("predicts_region", "")
    return country.reshape(720, 1440), continent.reshape(720, 1440), region.reshape(720, 1440)


def project_states(path: Path, year: int) -> dict[str, np.ndarray]:
    with Dataset(path) as dataset:
        index = time_index(dataset, year)
        return {
            name: sum(finite(dataset.variables[variable], index) for variable in variables)
            for name, variables in LUH2_CLASSES.items()
        }


def main() -> None:
    args = parse_args()
    for path in (
        args.historical, args.future, args.static, args.grid_lookup,
        args.population_2010, args.population_2030,
    ):
        if not path.exists():
            raise FileNotFoundError(path)
    args.landuse_output.parent.mkdir(parents=True, exist_ok=True)
    args.pressure_output.parent.mkdir(parents=True, exist_ok=True)

    with Dataset(args.static) as dataset:
        area = finite(dataset.variables["carea"])
    country, continent, region = read_lookup(args.grid_lookup)
    population_counts = {
        2010: aggregate_population(args.population_2010),
        2030: aggregate_population(args.population_2030),
    }

    land_fields = [
        "scenario", "cell_id", "year", "source_class", "share", "area_km2",
        "region", "country", "continent",
    ]
    pressure_fields = ["scenario", "cell_id", "year", "human_population_density"]
    with args.landuse_output.open("w", newline="", encoding="utf-8") as land_handle, \
            args.pressure_output.open("w", newline="", encoding="utf-8") as pressure_handle:
        land_writer = csv.DictWriter(land_handle, fieldnames=land_fields, lineterminator="\n")
        pressure_writer = csv.DictWriter(pressure_handle, fieldnames=pressure_fields, lineterminator="\n")
        land_writer.writeheader()
        pressure_writer.writeheader()

        for year, scenario, source in YEAR_SPECS:
            states = project_states(args.historical if source == "historical" else args.future, year)
            total = sum(states.values())
            valid = np.isfinite(total) & (total > 0) & np.isfinite(area) & (area > 0)
            land_weights = np.where(valid, area * total, 0.0)
            total_area = block_sum(land_weights)
            class_areas = {name: block_sum(np.where(valid, values * area, 0.0)) for name, values in states.items()}
            country_0p5 = block_mode(country, land_weights)
            continent_0p5 = block_mode(continent, land_weights)
            region_0p5 = block_mode(region, land_weights)
            valid_0p5 = (total_area > 0) & (region_0p5 != "")
            for row, column in zip(*np.where(valid_0p5)):
                cell_id = f"luh2_0p5_{row}_{column}"
                cell_area = float(total_area[row, column])
                denominator = cell_area
                pressure_writer.writerow({
                    "scenario": scenario,
                    "cell_id": cell_id,
                    "year": year,
                    "human_population_density": float(population_counts[year][row, column] / denominator),
                })
                for source_class, values in class_areas.items():
                    land_writer.writerow({
                        "scenario": scenario,
                        "cell_id": cell_id,
                        "year": year,
                        "source_class": source_class,
                        "share": float(values[row, column] / denominator),
                        "area_km2": cell_area,
                        "region": region_0p5[row, column],
                        "country": country_0p5[row, column],
                        "continent": continent_0p5[row, column],
                    })
    print(f"Wrote {args.landuse_output}")
    print(f"Wrote {args.pressure_output}")


if __name__ == "__main__":
    main()
