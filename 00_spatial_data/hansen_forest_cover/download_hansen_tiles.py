#!/usr/bin/env python3
"""The script downloads the raw Hansen Global Forest Change numerical raster layers.

The Hansen 2000--2025 data is locally stored as 10° x 10° GeoTIFFs, w these
source layers at the ~30m resolution. No forest threshold is applied here (i.e. selecting a percent canopy cover necessary to classify a tile as forest); this would be done when using the data.
1) treecover2000 -- percentage tree canopy cover at baseline
2) lossyear      -- one integer raster that encodes loss years 2001 onward (1 = 2001, 20 = 2020, etc.)
3) datamask      -- distinguishes land from water and no-data
"""

from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import math
import shutil
import ssl
import time
import urllib.request
from pathlib import Path

import certifi
import geopandas as gpd
import pandas as pd


parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
tile_scope = parser.add_mutually_exclusive_group(required=True)
tile_scope.add_argument("--global", dest="global_tiles", action="store_true", help="Select all Hansen 10° x 10° tiles spanning 180°W--180°E and 60°S--80°N.")
tile_scope.add_argument("--buffers", type=Path, help="Select tiles intersecting these GeoJSON buffer polygons in longitude/latitude.")
parser.add_argument("--output-dir", type=Path, default=Path("00_spatial_data/hansen_forest_cover"))
parser.add_argument("--download", action="store_true", help="Download missing files. Without this flag, only write the manifest.")
parser.add_argument("--sleep", type=float, default=0.2, help="Pause between new downloads in seconds.")
parser.add_argument("--workers", type=int, default=2, help="Number of simultaneous downloads (default: 2; lower values are gentler on the source service).")
parser.add_argument("--timeout", type=float, default=180, help="Seconds allowed for one network connection/read before retrying (default: 180).")
args = parser.parse_args()

VERSION = "GFC-2025-v1.13"
BASE_URL = f"https://storage.googleapis.com/earthenginepartners-hansen/{VERSION}"
LAYERS = ["treecover2000", "lossyear", "datamask"]

output_dir = args.output_dir
raster_dir = output_dir / "rasters"
output_dir.mkdir(parents=True, exist_ok=True)

if args.global_tiles:
    # The official product covers all 36 longitude bands and 14 latitude bands:
    # 180°W--180°E and 60°S--80°N.  The source URL lists below determine which
    # layer files actually exist for each tile.
    required_tiles = {(north, west) for north in range(80, -60, -10) for west in range(-180, 180, 10)}
else:
    if not args.buffers.exists():
        raise FileNotFoundError(f"Buffer file does not exist: {args.buffers}")
    buffers = gpd.read_file(args.buffers).to_crs("EPSG:4326")
    if buffers.empty:
        raise ValueError(f"No buffer polygons in {args.buffers}")

    # A Hansen tile is named for its north-west corner. Its latitude coverage
    # is (north - 10, north] and longitude coverage is [west, west + 10). Use
    # each buffer's bounding box to include every tile crossed at a tile edge.
    required_tiles = set()
    for min_x, min_y, max_x, max_y in buffers.geometry.bounds.itertuples(index=False, name=None):
        north_edges = range(math.ceil(min_y / 10) * 10, math.ceil(max_y / 10) * 10 + 1, 10)
        west_edges = range(math.floor(min_x / 10) * 10, math.floor(max_x / 10) * 10 + 1, 10)
        required_tiles.update((north, west) for north in north_edges for west in west_edges)

def format_latitude(north: int) -> str:
    return f"{abs(north):02d}{'N' if north >= 0 else 'S'}"


def format_longitude(west: int) -> str:
    return f"{abs(west):03d}{'E' if west >= 0 else 'W'}"


# The provider publishes authoritative URL lists for each layer. In particular,
# lossyear is intentionally absent for empty/ocean tiles, so constructing a
# Cartesian grid of URLs leads to 404 errors. Read those lists instead.
certificate_context = ssl.create_default_context(cafile=certifi.where())
available_urls = {}
for layer in LAYERS:
    list_url = f"{BASE_URL}/{layer}.txt"
    with urllib.request.urlopen(list_url, context=certificate_context, timeout=args.timeout) as response:
        available_urls[layer] = [line.strip() for line in response.read().decode("utf-8").splitlines() if line.strip()]

requested_tile_ids = {f"{format_latitude(north)}_{format_longitude(west)}" for north, west in required_tiles}
manifest_rows = []
for layer in LAYERS:
    for url in available_urls[layer]:
        filename = url.rsplit("/", 1)[-1]
        stem = filename.removesuffix(".tif")
        tile_id = "_".join(stem.split("_")[-2:])
        if tile_id not in requested_tile_ids:
            continue
        latitude_code, longitude_code = tile_id.split("_")
        north = int(latitude_code[:-1]) * (1 if latitude_code.endswith("N") else -1)
        west = int(longitude_code[:-1]) * (1 if longitude_code.endswith("E") else -1)
        local_path = raster_dir / layer / filename
        manifest_rows.append(
            {
                "version": VERSION,
                "layer": layer,
                "tile_id": tile_id,
                "north_edge": north,
                "west_edge": west,
                "url": url,
                "local_path": str(local_path),
                "downloaded": local_path.exists() and local_path.stat().st_size > 0,
                "bytes": local_path.stat().st_size if local_path.exists() else 0,
            }
        )

manifest = pd.DataFrame(manifest_rows)
manifest_path = output_dir / "hansen_tile_manifest.csv"
manifest.to_csv(manifest_path, index=False)
print(f"Selected {len(required_tiles)} Hansen tiles and {len(manifest)} raster files.")
print(f"Wrote manifest: {manifest_path}")

if not args.download:
    print("Dry run only. Re-run with --download to fetch missing files.")
    raise SystemExit(0)

def download_one(index: int, row: pd.Series) -> str:
    local_path = Path(row.local_path)
    if local_path.exists() and local_path.stat().st_size > 0:
        return "already present"
    local_path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path = local_path.with_suffix(local_path.suffix + ".part")
    if temporary_path.exists():
        temporary_path.unlink()
    for attempt in range(1, 4):
        print(f"Downloading {index + 1}/{len(manifest)}: {row.layer} {row.tile_id} (attempt {attempt}/3)", flush=True)
        try:
            with urllib.request.urlopen(row.url, context=certificate_context, timeout=args.timeout) as response, temporary_path.open("wb") as handle:
                shutil.copyfileobj(response, handle)
            if temporary_path.stat().st_size == 0:
                raise RuntimeError("received an empty file")
            temporary_path.replace(local_path)
            if args.sleep:
                time.sleep(args.sleep)
            return "downloaded"
        except Exception:
            if temporary_path.exists():
                temporary_path.unlink()
            if attempt == 3:
                raise
            time.sleep(attempt)
    raise RuntimeError(f"Could not download {row.url}")


missing_rows = [(index, row) for index, row in manifest.iterrows() if not (Path(row.local_path).exists() and Path(row.local_path).stat().st_size > 0)]
print(f"Downloading {len(missing_rows)} missing files with {args.workers} workers.")
with ThreadPoolExecutor(max_workers=args.workers) as executor:
    futures = [executor.submit(download_one, index, row) for index, row in missing_rows]
    failures = []
    for future in as_completed(futures):
        try:
            future.result()
        except Exception as error:
            failures.append(str(error))
    if failures:
        raise RuntimeError(f"{len(failures)} download(s) failed after three attempts. Re-run this command to resume; first error: {failures[0]}")

manifest["downloaded"] = manifest["local_path"].map(lambda value: Path(value).exists() and Path(value).stat().st_size > 0)
manifest["bytes"] = manifest["local_path"].map(lambda value: Path(value).stat().st_size if Path(value).exists() else 0)
manifest.to_csv(manifest_path, index=False)
if not manifest["downloaded"].all():
    raise RuntimeError("Not every requested Hansen file was downloaded.")
print(f"Downloaded {len(manifest)} files totalling {manifest['bytes'].sum() / 1024**3:.2f} GiB.")
