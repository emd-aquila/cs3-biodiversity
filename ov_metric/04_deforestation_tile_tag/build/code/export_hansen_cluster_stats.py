#!/usr/bin/env python3
"""Export Hansen Global Forest Change summaries for cluster-buffer polygons.

This script uses Google Earth Engine as the raster engine and writes a local
CSV consumed by 04_deforestation_tile_tag/build/code/07_build_cluster_footprints.R.
It does not download Hansen GeoTIFF granules.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import os
import sys
import tempfile
import time
import urllib.request
from pathlib import Path


HANSEN_ASSET_ID = "UMD/hansen/global_forest_change_2025_v1_13"
YEARS = list(range(2001, 2026))
DEFAULT_SELECTORS = [
    "hansen_cluster_buffer_id",
    "AEZ",
    "cluster_id",
    "buffer_km",
    "hansen_land_area_ha",
    "hansen_treecover2000_equiv_ha",
    "treecover2000_area_pct_ha",
] + [f"defor_ha_{year}" for year in YEARS]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True, help="Cluster-buffer GeoJSON written by the R build stage.")
    parser.add_argument("--output", required=True, help="Local CSV path to write in long year format.")
    parser.add_argument("--project", default=None, help="Optional Google Cloud project for ee.Initialize().")
    parser.add_argument("--chunk-size", type=int, default=25, help="Number of polygons per Earth Engine request.")
    parser.add_argument("--scale", type=float, default=30.92, help="Hansen reduction scale in meters.")
    parser.add_argument("--tile-scale", type=float, default=4, help="Earth Engine tileScale for reduceRegions.")
    parser.add_argument("--sleep", type=float, default=1, help="Seconds to sleep between chunk downloads.")
    return parser.parse_args()


def import_ee():
    try:
        import ee  # type: ignore
    except ImportError:
        print(
            "Missing Python package 'earthengine-api'. Install it with:\n"
            "  python3 -m pip install earthengine-api",
            file=sys.stderr,
        )
        raise
    return ee


def initialize_ee(ee, project: str | None) -> None:
    try:
        if project:
            ee.Initialize(project=project)
        else:
            ee.Initialize()
    except Exception as err:
        print(
            "Earth Engine is not initialized/authenticated.\n"
            "Run:\n"
            "  earthengine authenticate\n"
            "or rerun this script after configuring an Earth Engine project.\n"
            f"Original error: {err}",
            file=sys.stderr,
        )
        raise


def load_geojson_features(path: str) -> list[dict]:
    with open(path, "r", encoding="utf-8") as handle:
        data = json.load(handle)

    features = data.get("features", [])
    if not features:
        raise ValueError(f"No GeoJSON features found in {path}")
    return features


def chunks(values: list[dict], chunk_size: int):
    for start in range(0, len(values), chunk_size):
        yield start, values[start : start + chunk_size]


def build_summary_image(ee):
    hansen = ee.Image(HANSEN_ASSET_ID)
    lossyear = hansen.select("lossyear")
    datamask = hansen.select("datamask")
    treecover2000 = hansen.select("treecover2000")

    area_ha = ee.Image.pixelArea().divide(10000)
    land = datamask.eq(1)
    land_area = area_ha.updateMask(land).rename("hansen_land_area_ha")
    treecover_area = treecover2000.multiply(area_ha).updateMask(land).rename("treecover2000_area_pct_ha")
    treecover_equiv_area = treecover_area.divide(100).rename("hansen_treecover2000_equiv_ha")

    annual_bands = [
        area_ha.updateMask(land.And(lossyear.eq(year - 2000))).rename(f"defor_ha_{year}")
        for year in YEARS
    ]

    return ee.Image.cat(annual_bands + [land_area, treecover_equiv_area, treecover_area])


def ee_feature_collection(ee, features: list[dict]):
    ee_features = []
    for feature in features:
        props = dict(feature.get("properties") or {})
        geometry = ee.Geometry(feature["geometry"])
        ee_features.append(ee.Feature(geometry, props))
    return ee.FeatureCollection(ee_features)


def download_chunk(ee, image, features: list[dict], args: argparse.Namespace, chunk_index: int, temp_dir: Path) -> Path:
    fc = ee_feature_collection(ee, features)
    reduced = image.reduceRegions(
        collection=fc,
        reducer=ee.Reducer.sum(),
        scale=args.scale,
        tileScale=args.tile_scale,
    )

    selectors = DEFAULT_SELECTORS
    slim = reduced.map(lambda feature: ee.Feature(None, feature.toDictionary(selectors)))
    url = slim.getDownloadURL(filetype="CSV", selectors=selectors, filename=f"hansen_cluster_stats_{chunk_index:04d}")
    out_path = temp_dir / f"hansen_cluster_stats_{chunk_index:04d}.csv"

    print(f"Downloading Earth Engine summary chunk {chunk_index}: {len(features)} polygons", flush=True)
    urllib.request.urlretrieve(url, out_path)
    return out_path


def read_wide_rows(paths: list[Path]):
    for path in paths:
        with open(path, "r", encoding="utf-8") as handle:
            reader = csv.DictReader(handle)
            for row in reader:
                yield row


def as_float(value: str | None) -> float:
    if value is None or value == "":
        return 0.0
    try:
        parsed = float(value)
    except ValueError:
        return 0.0
    if math.isnan(parsed):
        return 0.0
    return parsed


def write_long_output(chunk_paths: list[Path], output_path: str) -> None:
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    fieldnames = [
        "hansen_cluster_buffer_id",
        "AEZ",
        "cluster_id",
        "buffer_km",
        "year",
        "defor_ha_total_raw",
        "hansen_land_area_ha",
        "hansen_treecover2000_equiv_ha",
        "hansen_treecover2000_mean_pct",
    ]

    with open(output_path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()

        for row in read_wide_rows(chunk_paths):
            land_area = as_float(row.get("hansen_land_area_ha"))
            treecover_equiv_area = as_float(row.get("hansen_treecover2000_equiv_ha"))
            treecover_area = as_float(row.get("treecover2000_area_pct_ha"))
            treecover_mean = treecover_area / land_area if land_area > 0 else ""

            for year in YEARS:
                writer.writerow(
                    {
                        "hansen_cluster_buffer_id": row.get("hansen_cluster_buffer_id", ""),
                        "AEZ": row.get("AEZ", ""),
                        "cluster_id": row.get("cluster_id", ""),
                        "buffer_km": row.get("buffer_km", ""),
                        "year": year,
                        "defor_ha_total_raw": as_float(row.get(f"defor_ha_{year}")),
                        "hansen_land_area_ha": land_area,
                        "hansen_treecover2000_equiv_ha": treecover_equiv_area,
                        "hansen_treecover2000_mean_pct": treecover_mean,
                    }
                )


def main() -> int:
    args = parse_args()
    ee = import_ee()
    initialize_ee(ee, args.project)

    features = load_geojson_features(args.input)
    image = build_summary_image(ee)
    total_chunks = math.ceil(len(features) / args.chunk_size)

    with tempfile.TemporaryDirectory(prefix="hansen_gee_chunks_") as temp_name:
        temp_dir = Path(temp_name)
        chunk_paths: list[Path] = []

        for chunk_index, (_, feature_chunk) in enumerate(chunks(features, args.chunk_size), start=1):
            print(f"Chunk {chunk_index}/{total_chunks}", flush=True)
            chunk_paths.append(download_chunk(ee, image, feature_chunk, args, chunk_index, temp_dir))
            if args.sleep > 0:
                time.sleep(args.sleep)

        write_long_output(chunk_paths, args.output)

    print(f"Wrote Hansen Earth Engine summary CSV: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
