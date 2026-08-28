#!/usr/bin/env python3
"""Sample annual LUH2 states at LPD population coordinates and aggregate them to PREDICTS classes."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

import numpy as np
from netCDF4 import Dataset


CLASSES = {
    "primary": ("primf", "primn"),
    "secondary": ("secdf", "secdn"),
    "cropland": ("c3ann", "c4ann", "c3per", "c4per", "c3nfx"),
    "pasture": ("pastr", "range"),
    "urban": ("urban",),
}


def arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--intervals", type=Path, required=True)
    parser.add_argument("--historical", type=Path, default=Path("bii_metric/input/luh2/raw/luh2_v2h_states_850-2015.nc"))
    parser.add_argument("--future", type=Path, default=Path("bii_metric/input/luh2/raw/luh2_v2f_ssp245_states_2015-2100.nc"))
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def first_year(ds: Dataset) -> int:
    units = str(getattr(ds.variables["time"], "units", ""))
    if not units.startswith("years since "):
        raise ValueError(f"Unsupported LUH2 time units: {units}")
    return int(units.split("years since ", 1)[1].split()[0].split("-", 1)[0])


def read_locations(interval_path: Path) -> tuple[list[dict[str, str]], list[int]]:
    with interval_path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    locations = {}
    years = set()
    for row in rows:
        locations[row["location_id"]] = row
        years.update((int(row["interval_start_year"]), int(row["interval_end_year"])))
    return list(locations.values()), sorted(years)


def nearest_indices(values: np.ndarray, targets: np.ndarray) -> np.ndarray:
    insertion = np.searchsorted(values, targets)
    insertion = np.clip(insertion, 1, len(values) - 1)
    left = values[insertion - 1]
    right = values[insertion]
    return np.where(np.abs(targets - left) <= np.abs(right - targets), insertion - 1, insertion)


def extract(ds: Dataset, year: int, rows: np.ndarray, cols: np.ndarray) -> dict[str, np.ndarray]:
    index = year - first_year(ds)
    if index < 0 or index >= len(ds.dimensions["time"]):
        raise ValueError(f"Year {year} missing from {ds.filepath()}")
    output = {}
    for label, variables in CLASSES.items():
        values = np.zeros(len(rows), dtype=np.float64)
        for variable in variables:
            layer = np.ma.filled(ds.variables[variable][index], np.nan)
            values += layer[rows, cols]
        output[label] = values
    return output


def main() -> None:
    args = arguments()
    locations, years = read_locations(args.intervals)
    if not locations or not years:
        raise ValueError("No LPD locations or interval years found")
    lat = np.array([float(row["latitude"]) for row in locations])
    lon = np.array([float(row["longitude"]) for row in locations])
    lon = ((lon + 180) % 360) - 180
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with Dataset(args.historical) as historical, Dataset(args.future) as future, args.output.open("w", newline="", encoding="utf-8") as handle:
        lats = np.asarray(historical.variables["lat"][:], dtype=np.float64)
        lons = np.asarray(historical.variables["lon"][:], dtype=np.float64)
        # LUH2 latitudes descend north-to-south; reverse only for index lookup.
        rows = (len(lats) - 1) - nearest_indices(lats[::-1], lat)
        cols = nearest_indices(lons, lon)
        fields = ["location_id", "latitude", "longitude", "year", "luh2_source", "luh2_terrestrial_fraction"] + [f"share_{label}" for label in CLASSES]
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        for year in years:
            dataset = historical if year <= 2015 else future
            source = "historical" if year <= 2015 else "ssp245_future"
            values = extract(dataset, year, rows, cols)
            total = sum(values.values())
            for index, location in enumerate(locations):
                record = {"location_id": location["location_id"], "latitude": lat[index], "longitude": lon[index], "year": year,
                          "luh2_source": source, "luh2_terrestrial_fraction": total[index]}
                for label, value in values.items():
                    record[f"share_{label}"] = value[index] / total[index] if np.isfinite(total[index]) and total[index] > 0 else ""
                writer.writerow(record)
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()
