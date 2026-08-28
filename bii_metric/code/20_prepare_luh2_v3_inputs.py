#!/usr/bin/env python3
"""Prepare detailed LUH2 V3 BII fractions and population pressure at 1 degree.

V3 preserves LUH2's crop, pasture/rangeland and primary classes.  It creates
secondary young (<30 y), intermediate (30--50 y) and mature (>50 y) fractions
from the installed annual state series beginning in 1960 (all older secondary
vegetation is therefore mature by construction). Because the exact 16-GB historical
transitions archive is not bundled, this implementation uses a conservative
net-change cohort proxy: only a net annual increase of secdf + secdn creates a
new cohort, while a net decrease removes all existing cohorts proportionally.
The output records this explicitly; replacing this routine with a transition
reader is a contained future upgrade, not a change to V1/V2.
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

import numpy as np
from netCDF4 import Dataset


TARGET_YEARS = (2010, 2020, 2030)
SECONDARY_AGE_START_YEAR = 1960
STATE_CLASSES = {
    "annual_cropland": ("c3ann", "c4ann"),
    "perennial_cropland": ("c3per", "c4per"),
    "nitrogen_fixing_cropland": ("c3nfx",),
    "managed_pasture": ("pastr",),
    "rangeland": ("range",),
    "urban": ("urban",),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--historical", type=Path, required=True)
    parser.add_argument("--future", type=Path, required=True)
    parser.add_argument("--static", type=Path, required=True)
    parser.add_argument("--grid-lookup", type=Path, required=True)
    parser.add_argument("--population-2010", type=Path, required=True)
    parser.add_argument("--population-2020", type=Path, required=True)
    parser.add_argument("--population-2030", type=Path, required=True)
    parser.add_argument("--intensity-mixture", type=Path, required=True)
    parser.add_argument("--landuse-output", type=Path, required=True)
    parser.add_argument("--pressure-output", type=Path, required=True)
    return parser.parse_args()


def finite(variable, index: int | None = None) -> np.ndarray:
    values = variable[:] if index is None else variable[index]
    return np.ma.filled(np.ma.asarray(values, dtype=np.float32), np.nan)


def first_year(dataset: Dataset) -> int:
    units = str(getattr(dataset.variables["time"], "units", ""))
    if not units.startswith("years since "):
        raise ValueError(f"Unsupported LUH2 time units: {units}")
    return int(units.split("years since ", 1)[1].split()[0].split("-", 1)[0])


def read_states(dataset: Dataset, index: int) -> dict[str, np.ndarray]:
    output = {name: sum(finite(dataset.variables[item], index) for item in members)
              for name, members in STATE_CLASSES.items()}
    output["primary"] = finite(dataset.variables["primf"], index) + finite(dataset.variables["primn"], index)
    output["secondary"] = finite(dataset.variables["secdf"], index) + finite(dataset.variables["secdn"], index)
    return output


GRID_FACTOR = 4  # Aggregate 0.25-degree LUH2 states to 1 degree for V3 fitting.


def block_sum(values: np.ndarray) -> np.ndarray:
    if values.shape != (720, 1440):
        raise ValueError(f"Expected 0.25-degree LUH2 grid, found {values.shape}")
    return values.reshape(180, GRID_FACTOR, 360, GRID_FACTOR).sum(axis=(1, 3))


def block_mode(labels: np.ndarray, weights: np.ndarray) -> np.ndarray:
    blocks = labels.reshape(180, GRID_FACTOR, 360, GRID_FACTOR).transpose(0, 2, 1, 3).reshape(180, 360, 16)
    area = weights.reshape(180, GRID_FACTOR, 360, GRID_FACTOR).transpose(0, 2, 1, 3).reshape(180, 360, 16)
    return np.take_along_axis(blocks, area.argmax(axis=2)[..., None], axis=2)[..., 0]


def read_lookup(path: Path) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    country = np.full(720 * 1440, "", dtype="<U8")
    continent = np.full(720 * 1440, "", dtype="<U24")
    region = np.full(720 * 1440, "", dtype="<U24")
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            index = int(row["grid_id"])
            country[index] = row.get("country_iso3", "")
            continent[index] = row.get("continent", "")
            region[index] = row.get("predicts_region", "")
    return country.reshape(720, 1440), continent.reshape(720, 1440), region.reshape(720, 1440)


def aggregate_population(path: Path) -> np.ndarray:
    with Dataset(path) as dataset:
        names = [name for name in dataset.variables if name not in {"lat", "lon"}]
        if len(names) != 1:
            raise ValueError(f"Expected exactly one population variable in {path}, found {names}")
        counts = finite(dataset.variables[names[0]])
        lat = finite(dataset.variables["lat"])
        lon = finite(dataset.variables["lon"])
    rows = np.floor((90 - lat)).astype(int)
    columns = np.floor(lon + 180).astype(int)
    output = np.zeros((180, 360), dtype=np.float64)
    counts = np.nan_to_num(counts, nan=0.0, posinf=0.0, neginf=0.0)
    for source_row, row in enumerate(rows):
        np.add.at(output[row], columns, counts[source_row])
    return output


class SecondaryCohorts:
    """Age secondary vegetation with explicit net-change assumptions."""

    def __init__(self, initial: np.ndarray):
        shape = initial.shape
        self.young = np.zeros((30, *shape), dtype=np.float32)
        self.intermediate = np.zeros((20, *shape), dtype=np.float32)
        self.mature = np.nan_to_num(initial, nan=0.0).astype(np.float32)
        self.previous = np.nan_to_num(initial, nan=0.0).astype(np.float32)

    def advance(self, current: np.ndarray) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        target = np.nan_to_num(current, nan=0.0).astype(np.float32)
        entering_mature = self.intermediate[0].copy()
        self.intermediate[:-1] = self.intermediate[1:]
        self.intermediate[-1] = self.young[0]
        self.young[:-1] = self.young[1:]
        self.young[-1] = np.maximum(0.0, target - self.previous)
        self.mature += entering_mature
        total = self.young.sum(axis=0) + self.intermediate.sum(axis=0) + self.mature
        scale = np.ones_like(target)
        positive = total > 0
        scale[positive] = np.minimum(1.0, target[positive] / total[positive])
        self.young *= scale
        self.intermediate *= scale
        self.mature *= scale
        self.previous = target
        return self.young.sum(axis=0), self.intermediate.sum(axis=0), self.mature.copy()


def create_snapshots(historical_path: Path, future_path: Path) -> dict[int, dict[str, np.ndarray]]:
    snapshots: dict[int, dict[str, np.ndarray]] = {}
    with Dataset(historical_path) as historical:
        start = first_year(historical)
        start_index = max(0, SECONDARY_AGE_START_YEAR - start)
        first = read_states(historical, start_index)
        cohorts = SecondaryCohorts(first["secondary"])
        if start + start_index in TARGET_YEARS:
            snapshots[start + start_index] = first
        for index in range(start_index + 1, len(historical.dimensions["time"])):
            year = start + index
            states = read_states(historical, index)
            young, intermediate, mature = cohorts.advance(states["secondary"])
            if year in TARGET_YEARS:
                states.update(secondary_young=young, secondary_intermediate=intermediate, secondary_mature=mature)
                snapshots[year] = states
    with Dataset(future_path) as future:
        start = first_year(future)
        for index in range(1, len(future.dimensions["time"])):
            year = start + index
            if year > max(TARGET_YEARS):
                break
            states = read_states(future, index)
            young, intermediate, mature = cohorts.advance(states["secondary"])
            if year in TARGET_YEARS:
                states.update(secondary_young=young, secondary_intermediate=intermediate, secondary_mature=mature)
                snapshots[year] = states
    missing = set(TARGET_YEARS).difference(snapshots)
    if missing:
        raise ValueError(f"Could not construct V3 snapshot(s): {sorted(missing)}")
    return snapshots


def read_intensity_mixture(path: Path) -> dict[str, list[tuple[str, float]]]:
    mixture: dict[str, list[tuple[str, float]]] = {}
    with path.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            mixture.setdefault(row["land_use"], []).append((row["intensity"], float(row["allocation_share"])))
    for key, values in mixture.items():
        total = sum(value for _, value in values)
        if not np.isclose(total, 1.0):
            raise ValueError(f"Intensity allocation for {key} does not sum to 1: {total}")
    return mixture


def main() -> None:
    args = parse_args()
    required = (args.historical, args.future, args.static, args.grid_lookup, args.population_2010,
                args.population_2020, args.population_2030, args.intensity_mixture)
    for path in required:
        if not path.exists():
            raise FileNotFoundError(path)
    args.landuse_output.parent.mkdir(parents=True, exist_ok=True)
    args.pressure_output.parent.mkdir(parents=True, exist_ok=True)
    mixture = read_intensity_mixture(args.intensity_mixture)
    snapshots = create_snapshots(args.historical, args.future)
    with Dataset(args.static) as static:
        area = finite(static.variables["carea"])
    country, continent, region = read_lookup(args.grid_lookup)
    population = {
        2010: aggregate_population(args.population_2010),
        2020: aggregate_population(args.population_2020),
        2030: aggregate_population(args.population_2030),
    }
    land_fields = ["scenario", "cell_id", "year", "land_use", "intensity", "pressure_class", "share",
                   "area_km2", "region", "country", "continent", "secondary_age_method"]
    pressure_fields = ["scenario", "cell_id", "year", "human_population_density"]
    with args.landuse_output.open("w", newline="", encoding="utf-8") as land_handle, \
            args.pressure_output.open("w", newline="", encoding="utf-8") as pressure_handle:
        land_writer = csv.DictWriter(land_handle, fieldnames=land_fields, lineterminator="\n")
        pressure_writer = csv.DictWriter(pressure_handle, fieldnames=pressure_fields, lineterminator="\n")
        land_writer.writeheader()
        pressure_writer.writeheader()
        for year in TARGET_YEARS:
            states = snapshots[year]
            spatial = {key: value for key, value in states.items() if key != "secondary"}
            spatial["primary"] = states["primary"]
            total = sum(spatial.values())
            valid = np.isfinite(total) & (total > 0) & np.isfinite(area) & (area > 0)
            weight = np.where(valid, total * area, 0.0)
            area_0p5 = block_sum(weight)
            country_0p5 = block_mode(country, weight)
            continent_0p5 = block_mode(continent, weight)
            region_0p5 = block_mode(region, weight)
            fractions = {key: block_sum(np.where(valid, value * area, 0.0)) for key, value in spatial.items()}
            for row, col in zip(*np.where((area_0p5 > 0) & (region_0p5 != ""))):
                cell_id = f"luh2_1deg_{row}_{col}"
                cell_area = float(area_0p5[row, col])
                pressure_writer.writerow({"scenario": "historical" if year <= 2015 else "ssp2rcp4p5messageglobiom",
                                         "cell_id": cell_id, "year": year,
                                         "human_population_density": float(population[year][row, col] / cell_area)})
                for land_use, values in fractions.items():
                    share = float(values[row, col] / cell_area)
                    if share <= 0:
                        continue
                    if land_use not in mixture:
                        raise ValueError(f"No strict-recuration intensity mixture for LUH2 class {land_use}")
                    for intensity, allocation in mixture[land_use]:
                        pressure_class = "primary__minimal" if land_use == "primary" and intensity == "minimal" else f"{land_use}__{intensity}"
                        land_writer.writerow({"scenario": "historical" if year <= 2015 else "ssp2rcp4p5messageglobiom",
                                              "cell_id": cell_id, "year": year, "land_use": land_use,
                                              "intensity": intensity, "pressure_class": pressure_class,
                                              "share": share * allocation, "area_km2": cell_area,
                                              "region": region_0p5[row, col], "country": country_0p5[row, col],
                                              "continent": continent_0p5[row, col],
                                              "secondary_age_method": "net_change_proxy_from_states"})
    print(f"Wrote {args.landuse_output}")
    print(f"Wrote {args.pressure_output}")


if __name__ == "__main__":
    main()
