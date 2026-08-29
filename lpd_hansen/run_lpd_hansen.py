"""The code runs the regression analysis of the LPD against deforestation as calculated using Hansen global forest change tiles.

The steps are:
1) prepare the LPD observations by making spatial buffers around each population coordinate pair
2) Identify the Hansen tiles needed for the buffers and download any that are missing
3) Calculate annual forest stock and loss for each buffer
4) Match forest loss over time to each population interval
5) Fit regression models (local and AEZ-specific) and write tables and figures

FOREST_COVER_THRESHOLD and BUFFER_KM_VALUES dictate which pixels are considered to be "forest" and how large of a buffer to draw around each population coordinate pair.

When the raw Hansen store is incomplete, the script downloads only the tiles
intersecting the generated buffers before continuing.
"""

from __future__ import annotations

# %% Imports and analysis settings

from concurrent.futures import ThreadPoolExecutor, as_completed
import math
import os
import shutil
import ssl
import time
import urllib.request
from pathlib import Path

script_dir = Path(__file__).resolve().parent
repo_root = script_dir.parent

os.environ.setdefault("MPLCONFIGDIR", str(script_dir / "output" / ".matplotlib"))
os.environ.setdefault("MPLBACKEND", "Agg")

import geopandas as gpd
import certifi
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import rasterio
from affine import Affine
from pyfixest.estimation import feols
from pyproj import Geod
from rasterio.features import rasterize
from rasterio.windows import Window, from_bounds
from shapely import make_valid
from shapely.geometry import box, mapping


# Input locations -- replace paths when running elsewhere
lpd_csv = repo_root / "00_biodiversity_data/living_planet/LPD_2024_public.csv"
aez_shapefile = repo_root / "00_spatial_data/aez/AEZ_shp_file.shp"

# Local folders created by this script.
hansen_data_dir = script_dir / "hansen_data"
output_dir = script_dir / "output"
table_dir = output_dir / "tables"
figure_dir = output_dir / "figures"
hansen_dir = output_dir / "hansen"
for directory in (table_dir, figure_dir, hansen_dir):
    directory.mkdir(parents=True, exist_ok=True)

YEAR_MIN = 2000
YEAR_MAX = 2020
YEARS = list(range(2001, 2021))
EDGE_SUBPIXELS = 4
MINIMUM_AEZ_INTERVALS = 30
MINIMUM_AEZ_LOCATIONS = 10
# The adjusted models compare intervals within the same population and within
# the same interval-start year. This is a model choice, not a data setting.
FIXED_EFFECTS = "population_id + interval_start_year"

# analysis settings to change if needed
FOREST_COVER_THRESHOLD = 30.0
BUFFER_KM_VALUES = [1.0]
PRIMARY_BUFFER_KM = 1.0
HANSEN_VERSION = "GFC-2025-v1.13"
HANSEN_BASE_URL = f"https://storage.googleapis.com/earthenginepartners-hansen/{HANSEN_VERSION}"
HANSEN_LAYERS = ("treecover2000", "lossyear", "datamask")
HANSEN_DOWNLOAD_WORKERS = 2
HANSEN_DOWNLOAD_TIMEOUT_SECONDS = 180

timeseries_path = table_dir / "lpd_native_terrestrial_population_year.csv"
intervals_path = table_dir / "lpd_native_terrestrial_intervals_2000_2020.csv"
locations_path = table_dir / "lpd_native_terrestrial_locations_aez.csv"
buffers_path = hansen_dir / "lpd_location_buffers.geojson"
hansen_path = hansen_dir / "hansen_location_year_forest.csv"
exposure_path = table_dir / "lpd_hansen_interval_exposure.csv"


def hansen_tile_id(north_edge: int, west_edge: int) -> str:
    latitude = f"{abs(north_edge):02d}{'N' if north_edge >= 0 else 'S'}"
    longitude = f"{abs(west_edge):03d}{'E' if west_edge >= 0 else 'W'}"
    return f"{latitude}_{longitude}"


def buffer_hansen_tile_ids(buffer_frame: gpd.GeoDataFrame) -> set[str]:
    tile_ids = set()
    for min_x, min_y, max_x, max_y in buffer_frame.geometry.bounds.itertuples(index=False, name=None):
        north_edges = range(math.ceil(min_y / 10) * 10, math.ceil(max_y / 10) * 10 + 1, 10)
        west_edges = range(math.floor(min_x / 10) * 10, math.floor(max_x / 10) * 10 + 1, 10)
        tile_ids.update(hansen_tile_id(north, west) for north in north_edges for west in west_edges)
    return tile_ids


def manifest_file_path(value: str) -> Path:
    path = Path(value)
    return path if path.is_absolute() else repo_root / path


def complete_hansen_tile_ids(manifest: pd.DataFrame, requested_tile_ids: set[str]) -> set[str]:
    required_columns = {"tile_id", "layer", "local_path"}
    if not required_columns.issubset(manifest.columns):
        return set()

    complete = set()
    for tile_id, rows in manifest.loc[manifest["tile_id"].isin(requested_tile_ids)].groupby("tile_id"):
        layer_paths = {row.layer: manifest_file_path(row.local_path) for row in rows.itertuples(index=False)}
        if {"treecover2000", "datamask"}.issubset(layer_paths) and all(
            layer_paths[layer].is_file() and layer_paths[layer].stat().st_size > 0
            for layer in ("treecover2000", "datamask")
        ):
            complete.add(tile_id)
    return complete


def download_missing_hansen_tiles(requested_tile_ids: set[str]) -> pd.DataFrame:
    """Fetch the source rasters needed for the supplied Hansen tile IDs."""
    hansen_data_dir.mkdir(parents=True, exist_ok=True)
    raster_dir = hansen_data_dir / "rasters"
    manifest_path = hansen_data_dir / "hansen_tile_manifest.csv"
    certificate_context = ssl.create_default_context(cafile=certifi.where())
    manifest_rows = []

    # The provider's file lists account for valid tiles where a lossyear
    # raster is not published, avoiding requests for files that do not exist.
    for layer in HANSEN_LAYERS:
        list_url = f"{HANSEN_BASE_URL}/{layer}.txt"
        with urllib.request.urlopen(
            list_url,
            context=certificate_context,
            timeout=HANSEN_DOWNLOAD_TIMEOUT_SECONDS,
        ) as response:
            available_urls = [line.strip() for line in response.read().decode("utf-8").splitlines() if line.strip()]

        for url in available_urls:
            filename = url.rsplit("/", 1)[-1]
            tile_id = "_".join(filename.removesuffix(".tif").split("_")[-2:])
            if tile_id not in requested_tile_ids:
                continue
            latitude_code, longitude_code = tile_id.split("_")
            north_edge = int(latitude_code[:-1]) * (1 if latitude_code.endswith("N") else -1)
            west_edge = int(longitude_code[:-1]) * (1 if longitude_code.endswith("E") else -1)
            local_path = raster_dir / layer / filename
            manifest_rows.append(
                {
                    "version": HANSEN_VERSION,
                    "layer": layer,
                    "tile_id": tile_id,
                    "north_edge": north_edge,
                    "west_edge": west_edge,
                    "url": url,
                    "local_path": str(local_path),
                    "downloaded": local_path.is_file() and local_path.stat().st_size > 0,
                    "bytes": local_path.stat().st_size if local_path.is_file() else 0,
                }
            )

    manifest = pd.DataFrame(manifest_rows)
    if manifest.empty:
        raise RuntimeError("No requested Hansen tiles were found in the official file lists.")
    manifest.to_csv(manifest_path, index=False)

    def download_one(row_number: int, row: pd.Series) -> str:
        local_path = Path(row.local_path)
        if local_path.is_file() and local_path.stat().st_size > 0:
            return "already present"
        local_path.parent.mkdir(parents=True, exist_ok=True)
        temporary_path = local_path.with_suffix(local_path.suffix + ".part")
        if temporary_path.exists():
            temporary_path.unlink()
        for attempt in range(1, 4):
            print(
                f"Downloading Hansen file {row_number}/{len(manifest)}: "
                f"{row.layer} {row.tile_id} (attempt {attempt}/3)",
                flush=True,
            )
            try:
                with urllib.request.urlopen(
                    row.url,
                    context=certificate_context,
                    timeout=HANSEN_DOWNLOAD_TIMEOUT_SECONDS,
                ) as response, temporary_path.open("wb") as handle:
                    shutil.copyfileobj(response, handle)
                if temporary_path.stat().st_size == 0:
                    raise RuntimeError("Received an empty file.")
                temporary_path.replace(local_path)
                return "downloaded"
            except Exception:
                if temporary_path.exists():
                    temporary_path.unlink()
                if attempt == 3:
                    raise
                time.sleep(attempt)
        raise RuntimeError(f"Could not download {row.url}")

    missing_rows = [
        (index + 1, row)
        for index, row in manifest.iterrows()
        if not (Path(row.local_path).is_file() and Path(row.local_path).stat().st_size > 0)
    ]
    if missing_rows:
        print(f"Downloading {len(missing_rows)} Hansen file(s) for {len(requested_tile_ids)} buffer tile(s).")
        with ThreadPoolExecutor(max_workers=HANSEN_DOWNLOAD_WORKERS) as executor:
            futures = [executor.submit(download_one, index, row) for index, row in missing_rows]
            failures = []
            for future in as_completed(futures):
                try:
                    future.result()
                except Exception as error:
                    failures.append(str(error))
        if failures:
            raise RuntimeError(
                f"{len(failures)} Hansen file download(s) failed after three attempts. "
                f"Re-run the script to resume; first error: {failures[0]}"
            )

    manifest["downloaded"] = manifest["local_path"].map(
        lambda value: Path(value).is_file() and Path(value).stat().st_size > 0
    )
    manifest["bytes"] = manifest["local_path"].map(
        lambda value: Path(value).stat().st_size if Path(value).is_file() else 0
    )
    manifest.to_csv(manifest_path, index=False)
    return manifest


# %% 1. Prepare LPD observations, assign AEZs, and create spatial buffers

# Check that the required inputs are present and have the expected size.
if not lpd_csv.exists():
    raise FileNotFoundError(f"Missing LPD input: {lpd_csv}")
if not aez_shapefile.exists():
    raise FileNotFoundError(f"Missing AEZ geometry: {aez_shapefile}")

raw = pd.read_csv(lpd_csv, dtype={"Native": "string"}, low_memory=False)
if len(raw) != 35_996:
    raise ValueError(f"Expected 35,996 rows in the pinned 2024 public LPD release; found {len(raw):,}.")
for column in raw.select_dtypes(include=["object", "string"]):
    raw[column] = raw[column].str.strip()

year_columns = [str(year) for year in range(YEAR_MIN, YEAR_MAX + 1)]
missing_year_columns = sorted(set(year_columns) - set(raw.columns))
if missing_year_columns:
    raise ValueError(f"LPD input is missing year columns: {', '.join(missing_year_columns)}")

cohort_columns = {
    "ID": "population_id",
    "Binomial": "binomial",
    "Class": "taxon_class",
    "Order": "taxon_order",
    "Family": "taxon_family",
    "Genus": "genus",
    "Species": "species",
    "Country": "country",
    "Region": "region",
    "IPBES_region": "ipbes_region",
    "Latitude": "latitude",
    "Longitude": "longitude",
    "Units": "units",
    "Included in LPR2024": "lpr2024_included",
}

cohort = raw.loc[(raw["System"] == "Terrestrial") & (raw["Native"] == "1"), list(cohort_columns) + year_columns].copy()
cohort = cohort.rename(columns=cohort_columns)

# Normalize numeric LPD IDs to plain integer strings for joins and outputs.
id_numbers = pd.to_numeric(cohort["population_id"], errors="raise")
if not (np.isfinite(id_numbers) & np.equal(id_numbers, np.floor(id_numbers))).all():
    raise ValueError("LPD population IDs must be finite whole numbers.")
cohort["population_id"] = id_numbers.astype("int64").astype(str)
cohort["latitude"] = pd.to_numeric(cohort["latitude"], errors="coerce")
cohort["longitude"] = pd.to_numeric(cohort["longitude"], errors="coerce")
if len(cohort) != 11_702:
    raise ValueError(f"Expected 11,702 native terrestrial populations; found {len(cohort):,}.")
if cohort["population_id"].duplicated().any():
    raise ValueError("LPD population IDs must be unique in the raw wide table.")

timeseries = cohort.melt(
    id_vars=list(cohort_columns.values()),
    value_vars=year_columns,
    var_name="year",
    value_name="abundance_raw",
)
timeseries["year"] = pd.to_numeric(timeseries["year"], errors="raise").astype(int)
timeseries["abundance"] = pd.to_numeric(timeseries["abundance_raw"], errors="coerce")
timeseries = timeseries.loc[
    np.isfinite(timeseries["abundance"])
    & (timeseries["abundance"] >= 0)
    & np.isfinite(timeseries["latitude"])
    & np.isfinite(timeseries["longitude"])
].drop(columns="abundance_raw")
timeseries = timeseries.sort_values(["population_id", "year"]).reset_index(drop=True)
if timeseries.duplicated(["population_id", "year"]).any():
    raise ValueError("A population has duplicate annual LPD values.")
timeseries.to_csv(timeseries_path, index=False, na_rep="")

# An interval is made between the next available abundance observation for the
# same population. Thus missing 2002--03 values make a 2001--04 interval.
positive_mean = timeseries["abundance"].where(timeseries["abundance"] > 0).groupby(timeseries["population_id"]).transform("mean")
timeseries["abundance_replacement"] = 0.01 * positive_mean
timeseries["abundance_for_log"] = timeseries["abundance"].where(timeseries["abundance"] != 0, timeseries["abundance_replacement"])
timeseries["zero_replaced"] = timeseries["abundance"] == 0
grouped_timeseries = timeseries.groupby("population_id", sort=False)
timeseries["interval_end_year"] = grouped_timeseries["year"].shift(-1)
timeseries["abundance_end"] = grouped_timeseries["abundance"].shift(-1)
timeseries["abundance_end_for_log"] = grouped_timeseries["abundance_for_log"].shift(-1)
timeseries["zero_replaced_end"] = grouped_timeseries["zero_replaced"].shift(-1)

intervals = timeseries.loc[
    timeseries["interval_end_year"].notna()
    & np.isfinite(timeseries["abundance_for_log"])
    & np.isfinite(timeseries["abundance_end_for_log"])
    & (timeseries["abundance_for_log"] > 0)
    & (timeseries["abundance_end_for_log"] > 0)
].copy()
intervals["location_id"] = intervals["latitude"].map("{:.7f}".format) + "__" + intervals["longitude"].map("{:.6f}".format)
intervals["interval_start_year"] = intervals["year"].astype(int)
intervals["interval_end_year"] = intervals["interval_end_year"].astype(int)
intervals["interval_duration"] = intervals["interval_end_year"] - intervals["interval_start_year"]
intervals["log_abundance_change"] = np.log(intervals["abundance_end_for_log"]) - np.log(intervals["abundance_for_log"])
intervals = intervals.loc[
    (intervals["interval_duration"] > 0)
    & (intervals["interval_start_year"] >= YEAR_MIN)
    & (intervals["interval_end_year"] <= YEAR_MAX)
].copy()
intervals["interval_id"] = (
    intervals["population_id"]
    + "__"
    + intervals["interval_start_year"].astype(str)
    + "__"
    + intervals["interval_end_year"].astype(str)
)

interval_columns = [
    "interval_id", "population_id", "location_id", "binomial", "taxon_class", "taxon_order", "taxon_family",
    "genus", "species", "country", "region", "ipbes_region", "latitude", "longitude", "units",
    "lpr2024_included", "interval_start_year", "interval_end_year", "interval_duration", "abundance_start",
    "abundance_end", "abundance_start_for_log", "abundance_end_for_log", "zero_replaced_start",
    "zero_replaced_end", "log_abundance_change",
]
intervals = intervals.rename(
    columns={
        "abundance": "abundance_start",
        "abundance_for_log": "abundance_start_for_log",
        "zero_replaced": "zero_replaced_start",
    }
)[interval_columns].reset_index(drop=True)
if intervals.empty or intervals["interval_id"].duplicated().any():
    raise ValueError("No valid or non-unique LPD intervals were created.")
if not (intervals["interval_start_year"] < intervals["interval_end_year"]).all():
    raise ValueError("A non-positive interval duration was found.")
if not np.isfinite(intervals["log_abundance_change"]).all():
    raise ValueError("A non-finite log abundance change was found.")
intervals.to_csv(intervals_path, index=False, na_rep="")


# Assign each unique coordinate to an AEZ and make Hansen buffer polygons.

locations = intervals[["location_id", "latitude", "longitude", "country", "region", "ipbes_region"]].drop_duplicates("location_id").copy()
points = gpd.GeoDataFrame(
    locations,
    geometry=gpd.points_from_xy(locations["longitude"], locations["latitude"]),
    crs="EPSG:4326",
)
aez = gpd.read_file(aez_shapefile)[["Id", "AEZ", "geometry"]].copy()
aez["geometry"] = aez.geometry.map(make_valid)
aez = aez.to_crs("EPSG:4326").rename(columns={"Id": "AEZ_id"})

# `within` deliberately leaves points on AEZ borders, and points outside all
# AEZs, unassigned. They remain in global models but not AEZ-specific models.
locations_with_aez = gpd.sjoin(points, aez, how="left", predicate="within")
locations_with_aez = locations_with_aez.sort_values(["location_id", "AEZ_id"], na_position="last")
locations_with_aez = locations_with_aez.drop_duplicates("location_id").drop(columns=["geometry", "index_right"])
locations_with_aez["AEZ_assignment"] = np.where(locations_with_aez["AEZ"].isna(), "unassigned", "within_polygon")
locations_with_aez = locations_with_aez[["location_id", "latitude", "longitude", "country", "region", "ipbes_region", "AEZ_id", "AEZ", "AEZ_assignment"]]
if locations_with_aez["location_id"].duplicated().any():
    raise ValueError("Location IDs are not unique after AEZ assignment.")
locations_with_aez.to_csv(locations_path, index=False, na_rep="")

# EPSG:6933 is an equal-area CRS, so these are true kilometre buffers rather
# than degree buffers. Buffers are not dissolved: each location remains an
# exposure unit even when neighbouring buffers overlap.
point_geometry = gpd.GeoDataFrame(locations_with_aez, geometry=points.geometry, crs="EPSG:4326").to_crs("EPSG:6933")
buffer_frames = []
for buffer_km in BUFFER_KM_VALUES:
    one_buffer = point_geometry.copy()
    one_buffer["buffer_km"] = buffer_km
    one_buffer["geometry"] = one_buffer.geometry.buffer(buffer_km * 1000)
    buffer_frames.append(one_buffer)
buffers = pd.concat(buffer_frames, ignore_index=True)
buffers = gpd.GeoDataFrame(buffers, geometry="geometry", crs="EPSG:6933").to_crs("EPSG:4326")
if buffers_path.exists():
    buffers_path.unlink()
buffers.to_file(buffers_path, driver="GeoJSON", index=False)

prepare_audit = pd.DataFrame(
    {
        "metric": [
            "raw_lpd_populations", "native_terrestrial_populations", "population_year_observations_2000_2020",
            "adjacent_intervals_2000_2020", "interval_populations", "locations", "unassigned_aez_locations",
            "intervals_with_zero_replacement",
        ],
        "value": [
            len(raw), len(cohort), len(timeseries), len(intervals), intervals["population_id"].nunique(),
            len(locations_with_aez), (locations_with_aez["AEZ_assignment"] == "unassigned").sum(),
            (intervals["zero_replaced_start"] | intervals["zero_replaced_end"]).sum(),
        ],
    }
)
prepare_audit.to_csv(table_dir / "prepare_audit.csv", index=False)
print(f"Prepared {len(intervals):,} adjacent intervals from {intervals['population_id'].nunique():,} populations at {len(locations_with_aez):,} locations.")
print(f"Wrote Hansen point buffers: {buffers_path}")


# %% 2. Identify and download the Hansen tiles that overlap each buffer

# The raw tiles are kept separately from LPD outputs. They contain continuous
# treecover2000, lossyear, and datamask rasters, so the forest threshold is
# applied later without modifying the source data.
manifest_path = hansen_data_dir / "hansen_tile_manifest.csv"
hansen_buffers = gpd.read_file(buffers_path).to_crs("EPSG:4326").reset_index(drop=True)
hansen_buffer_index = hansen_buffers.sindex
geod = Geod(ellps="WGS84")
requested_tile_ids = buffer_hansen_tile_ids(hansen_buffers)

existing_manifest = pd.read_csv(manifest_path) if manifest_path.exists() else pd.DataFrame()
missing_tile_ids = requested_tile_ids - complete_hansen_tile_ids(existing_manifest, requested_tile_ids)
if missing_tile_ids:
    print(f"Downloading Hansen data for {len(missing_tile_ids)} missing buffer tile(s).")
    download_missing_hansen_tiles(requested_tile_ids)

if not manifest_path.exists():
    raise FileNotFoundError(f"Hansen manifest is missing: {manifest_path}")
hansen_manifest = pd.read_csv(manifest_path)
incomplete_tile_ids = requested_tile_ids - complete_hansen_tile_ids(hansen_manifest, requested_tile_ids)
if incomplete_tile_ids:
    raise RuntimeError(
        "Hansen data remain incomplete for these buffers after the download: "
        f"{', '.join(sorted(incomplete_tile_ids)[:12])}."
    )

# Keep only locally complete source tiles. The official download omits
# lossyear for tiles with no applicable loss layer; those pixels are treated
# below as having lossyear == 0.
tile_paths = {}
for tile_id, tile_rows in hansen_manifest.loc[hansen_manifest["tile_id"].isin(requested_tile_ids)].groupby("tile_id"):
    paths = {row.layer: manifest_file_path(row.local_path) for row in tile_rows.itertuples(index=False)}
    if {"treecover2000", "datamask"}.issubset(paths) and paths["treecover2000"].is_file() and paths["datamask"].is_file():
        tile_paths[tile_id] = paths
if not tile_paths:
    raise FileNotFoundError("No complete local Hansen treecover2000/datamask tiles were found.")

tile_edges = (
    hansen_manifest.loc[hansen_manifest["tile_id"].isin(tile_paths)]
    .groupby("tile_id")[["north_edge", "west_edge"]]
    .first()
    .to_dict("index")
)


# %% 3. Calculate annual forest stock and loss within each buffer

# Work tile-by-tile, so a GeoTIFF is opened once even when many buffers
# intersect it. Values in metrics are hectares; loss_by_year_code[8] is
# forest lost during calendar year 2008.
metrics = {
    index: {"hansen_area_ha": 0.0, "baseline_forest_area_ha": 0.0, "loss_by_year_code": np.zeros(26)}
    for index in hansen_buffers.index
}
buffers_with_data = {index: 0 for index in hansen_buffers.index}

for tile_number, (tile_id, paths) in enumerate(sorted(tile_paths.items()), start=1):
    north_edge = tile_edges[tile_id]["north_edge"]
    west_edge = tile_edges[tile_id]["west_edge"]
    tile_geometry = box(west_edge, north_edge - 10, west_edge + 10, north_edge)
    candidate_indices = list(hansen_buffer_index.query(tile_geometry, predicate="intersects"))
    if not candidate_indices:
        continue
    with rasterio.open(paths["treecover2000"]) as treecover, rasterio.open(paths["datamask"]) as datamask:
        tile_geometry = box(*treecover.bounds)
        loss_path = paths.get("lossyear")
        loss_dataset = rasterio.open(loss_path) if loss_path and loss_path.is_file() else None
        try:
            if treecover.shape != datamask.shape or treecover.transform != datamask.transform:
                raise ValueError(f"treecover2000 and datamask are not aligned for {tile_id}.")
            if loss_dataset and (treecover.shape != loss_dataset.shape or treecover.transform != loss_dataset.transform):
                raise ValueError(f"treecover2000 and lossyear are not aligned for {tile_id}.")
            print(f"Reading local Hansen tile {tile_number}/{len(tile_paths)}: {tile_id}")

            for buffer_index_value in candidate_indices:
                buffer_geometry = hansen_buffers.geometry.iloc[buffer_index_value]
                overlap_geometry = buffer_geometry.intersection(tile_geometry)
                if overlap_geometry.is_empty:
                    continue
                window = from_bounds(*overlap_geometry.bounds, transform=treecover.transform).round_offsets().round_lengths()
                window = window.intersection(Window(0, 0, treecover.width, treecover.height))
                if window.width <= 0 or window.height <= 0:
                    continue
                window = Window(int(window.col_off), int(window.row_off), int(window.width), int(window.height))

                # Rasterise the buffer onto a small N x N sub-pixel grid
                # and average it back to Hansen pixels. Thus interior
                # pixels have weight 1 and edge pixels have fractional
                # area, rather than being included/excluded wholesale.
                fine_transform = treecover.window_transform(window) * Affine.scale(1 / EDGE_SUBPIXELS, 1 / EDGE_SUBPIXELS)
                fine_shape = (int(window.height) * EDGE_SUBPIXELS, int(window.width) * EDGE_SUBPIXELS)
                overlap_fraction = rasterize(
                    [(mapping(buffer_geometry), 1)], out_shape=fine_shape, transform=fine_transform,
                    fill=0, all_touched=False, dtype="uint8",
                ).reshape(int(window.height), EDGE_SUBPIXELS, int(window.width), EDGE_SUBPIXELS).mean(axis=(1, 3))
                if not overlap_fraction.any():
                    continue

                tree_cover = treecover.read(1, window=window)
                data_mask = datamask.read(1, window=window)
                loss_year = loss_dataset.read(1, window=window) if loss_dataset else np.zeros(tree_cover.shape, dtype=np.uint8)
                if np.any(loss_year > 25):
                    raise ValueError(f"Unexpected lossyear code in {tile_id}.")

                # The GeoTIFF is in longitude/latitude. Pixel width is
                # fixed, but true area changes by raster row, so calculate
                # geodesic area per row before applying edge fractions.
                window_transform = treecover.window_transform(window)
                row_areas_ha = []
                for row in range(int(window.height)):
                    west, north = window_transform * (0, row)
                    east, south = window_transform * (1, row + 1)
                    area_m2, _ = geod.polygon_area_perimeter([west, east, east, west], [north, north, south, south])
                    row_areas_ha.append(abs(area_m2) / 10_000)
                pixel_area_ha = overlap_fraction * np.asarray(row_areas_ha)[:, None]

                land = data_mask == 1
                baseline_forest = land & (tree_cover >= FOREST_COVER_THRESHOLD)
                metrics[buffer_index_value]["hansen_area_ha"] += pixel_area_ha[land].sum()
                metrics[buffer_index_value]["baseline_forest_area_ha"] += pixel_area_ha[baseline_forest].sum()
                metrics[buffer_index_value]["loss_by_year_code"] += np.bincount(
                    loss_year[baseline_forest], weights=pixel_area_ha[baseline_forest], minlength=26,
                )[:26]
                buffers_with_data[buffer_index_value] += 1
        finally:
            if loss_dataset:
                loss_dataset.close()

if not all(buffers_with_data.values()):
    raise RuntimeError("At least one LPD buffer did not overlap a local Hansen tile.")

# Write one annual row per buffer. A 2008 loss is a 2008 flow and is
# excluded from forest_area_remaining_ha in 2008, matching the stated
# binary forest reconstruction (not an annual canopy-cover measurement).
annual_rows = []
for buffer_index_value, buffer_row in hansen_buffers.iterrows():
    metric = metrics[buffer_index_value]
    cumulative_loss = 0.0
    for year in range(2000, 2026):
        annual_defor_ha = metric["loss_by_year_code"][year - 2000] if year > 2000 else 0.0
        cumulative_loss += annual_defor_ha
        forest_area_remaining_ha = max(metric["baseline_forest_area_ha"] - cumulative_loss, 0.0)
        annual_rows.append(
            {
                "location_id": buffer_row.location_id,
                "buffer_km": buffer_row.buffer_km,
                "year": year,
                "forest_cover_threshold_pct": FOREST_COVER_THRESHOLD,
                "hansen_area_ha": metric["hansen_area_ha"],
                "baseline_forest_area_ha": metric["baseline_forest_area_ha"],
                "forest_area_remaining_ha": forest_area_remaining_ha,
                "forest_cover_pct": 100 * forest_area_remaining_ha / metric["hansen_area_ha"] if metric["hansen_area_ha"] else np.nan,
                "annual_defor_ha": annual_defor_ha,
                "edge_subpixels": EDGE_SUBPIXELS,
            }
        )
pd.DataFrame(annual_rows).sort_values(["location_id", "buffer_km", "year"]).to_csv(hansen_path, index=False)
print(f"Wrote local Hansen forest summary: {hansen_path}")


# %% 4. Match annual Hansen forest loss to each population interval

hansen = pd.read_csv(hansen_path, dtype={"location_id": "string"})
hansen["buffer_km"] = pd.to_numeric(hansen["buffer_km"], errors="raise")
hansen["year"] = pd.to_numeric(hansen["year"], errors="raise").astype(int)
required_hansen_columns = {
    "location_id", "buffer_km", "year", "forest_cover_threshold_pct", "annual_defor_ha", "hansen_area_ha",
    "baseline_forest_area_ha", "forest_area_remaining_ha", "forest_cover_pct",
}
if not required_hansen_columns.issubset(hansen.columns):
    raise ValueError(f"Hansen output is missing: {sorted(required_hansen_columns - set(hansen.columns))}")
if not hansen["year"].between(2000, 2025).all() or hansen.duplicated(["location_id", "buffer_km", "year"]).any():
    raise ValueError("Hansen output has invalid years or duplicate location-buffer-year rows.")
# Preserve the full 2000--2025 annual table on disk, but LPD intervals in this
# analysis end in 2020 and therefore need only loss years 2001--2020.
hansen = hansen.loc[hansen["year"].isin(YEARS)].copy()

exposure = intervals.merge(
    locations_with_aez[["location_id", "AEZ", "AEZ_id", "AEZ_assignment"]],
    on="location_id",
    how="inner",
    validate="many_to_one",
).merge(hansen, on="location_id", how="inner", validate="many_to_many")

# A 2005--08 abundance interval receives loss recorded in 2006--08. This
# start-exclusive/end-inclusive convention is an explicit analysis choice.
exposure = exposure.loc[
    (exposure["year"] > exposure["interval_start_year"])
    & (exposure["year"] <= exposure["interval_end_year"])
].copy()

first_columns = [
    column
    for column in exposure.columns
    if column
    not in {
        "year", "annual_defor_ha", "hansen_area_ha", "baseline_forest_area_ha", "forest_area_remaining_ha",
        "forest_cover_pct", "forest_cover_threshold_pct", "edge_subpixels",
    }
    and column not in {"interval_id", "buffer_km"}
]
exposure_first = exposure.groupby(["interval_id", "buffer_km"], as_index=False)[first_columns].first()
exposure_sums = exposure.groupby(["interval_id", "buffer_km"], as_index=False).agg(
    n_hansen_years=("year", "nunique"),
    forest_loss_ha=("annual_defor_ha", "sum"),
    hansen_area_ha=("hansen_area_ha", "first"),
    forest_cover_threshold_pct=("forest_cover_threshold_pct", "first"),
    baseline_forest_area_ha=("baseline_forest_area_ha", "first"),
    forest_area_remaining_ha=("forest_area_remaining_ha", "last"),
    forest_cover_pct=("forest_cover_pct", "last"),
)
exposure = exposure_first.merge(exposure_sums, on=["interval_id", "buffer_km"], validate="one_to_one")
exposure["expected_hansen_years"] = exposure["interval_duration"]
exposure["forest_loss_log1p_ha"] = np.log1p(exposure["forest_loss_ha"])
exposure["forest_loss_baseline_forest_share"] = np.where(
    exposure["baseline_forest_area_ha"] > 0,
    exposure["forest_loss_ha"] / exposure["baseline_forest_area_ha"],
    np.nan,
)
if not (exposure["n_hansen_years"] == exposure["expected_hansen_years"]).all():
    raise ValueError("At least one interval is missing a matched Hansen year or has an invalid loss window.")
if exposure.duplicated(["interval_id", "buffer_km"]).any():
    raise ValueError("Exposure rows are not unique per interval and buffer.")
exposure.to_csv(exposure_path, index=False, na_rep="")


# %% 5. Fit global and AEZ-specific regression models

# log_abundance_change ~ exposure + interval_duration | FIXED_EFFECTS
# Standard errors are clustered by population and location.
results = []
models = {}
for buffer_km in BUFFER_KM_VALUES:
    model_data = exposure.loc[
        np.isclose(exposure["buffer_km"], buffer_km)
        & np.isfinite(exposure["log_abundance_change"])
        & np.isfinite(exposure["forest_loss_ha"])
    ].copy()
    for exposure_name in ("forest_loss_ha", "forest_loss_log1p_ha"):
        model = feols(
            f"log_abundance_change ~ {exposure_name} + interval_duration | {FIXED_EFFECTS}",
            data=model_data,
            vcov={"CRV1": "population_id + location_id"},
        )
        models[f"global_buffer_{buffer_km}_{exposure_name}"] = model
        estimate = model.coef().loc[exposure_name]
        standard_error = model.se().loc[exposure_name]
        results.append(
            {
                "group": "global", "buffer_km": buffer_km, "exposure": exposure_name, "term": exposure_name,
                "estimate": estimate, "std_error": standard_error, "statistic": model.tstat().loc[exposure_name],
                "p_value": model.pvalue().loc[exposure_name], "conf_low": estimate - 1.96 * standard_error,
                "conf_high": estimate + 1.96 * standard_error, "n_intervals": model._N,
                "n_populations": model_data["population_id"].nunique(), "n_locations": model_data["location_id"].nunique(),
                "n_countries": model_data["country"].nunique(), "zero_loss_share": (model_data["forest_loss_ha"] == 0).mean(),
            }
        )

primary_data = exposure.loc[np.isclose(exposure["buffer_km"], PRIMARY_BUFFER_KM)].copy()
unadjusted = feols("log_abundance_change ~ forest_loss_ha", data=primary_data, vcov={"CRV1": "location_id"})
unadjusted_estimate = unadjusted.coef().loc["forest_loss_ha"]
unadjusted_se = unadjusted.se().loc["forest_loss_ha"]
results.append(
    {
        "group": "global_unadjusted", "buffer_km": PRIMARY_BUFFER_KM, "exposure": "forest_loss_ha", "term": "forest_loss_ha",
        "estimate": unadjusted_estimate, "std_error": unadjusted_se, "statistic": unadjusted.tstat().loc["forest_loss_ha"],
        "p_value": unadjusted.pvalue().loc["forest_loss_ha"], "conf_low": unadjusted_estimate - 1.96 * unadjusted_se,
        "conf_high": unadjusted_estimate + 1.96 * unadjusted_se, "n_intervals": unadjusted._N,
        "n_populations": primary_data["population_id"].nunique(), "n_locations": primary_data["location_id"].nunique(),
        "n_countries": primary_data["country"].nunique(), "zero_loss_share": (primary_data["forest_loss_ha"] == 0).mean(),
    }
)

# First determine which AEZs have enough independent spatial coverage, then
# fit the same adjusted primary specification separately inside each one.
aez_counts = primary_data.dropna(subset=["AEZ"]).groupby("AEZ").agg(
    n_intervals=("interval_id", "size"),
    n_locations=("location_id", "nunique"),
    n_loss_values=("forest_loss_ha", "nunique"),
)
eligible_aez_names = aez_counts.index[
    (aez_counts["n_intervals"] >= MINIMUM_AEZ_INTERVALS)
    & (aez_counts["n_locations"] >= MINIMUM_AEZ_LOCATIONS)
    & (aez_counts["n_loss_values"] >= 2)
]
eligible_aez = primary_data.loc[primary_data["AEZ"].isin(eligible_aez_names)].copy()
for aez_name, model_data in eligible_aez.groupby("AEZ", sort=True):
    try:
        model = feols(
            f"log_abundance_change ~ forest_loss_ha + interval_duration | {FIXED_EFFECTS}",
            data=model_data,
            vcov={"CRV1": "population_id + location_id"},
        )
    except Exception as error:
        print(f"Skipping AEZ {aez_name}: {error}")
        continue
    models[f"aez_{aez_name}"] = model
    estimate = model.coef().loc["forest_loss_ha"]
    standard_error = model.se().loc["forest_loss_ha"]
    results.append(
        {
            "group": str(aez_name), "buffer_km": PRIMARY_BUFFER_KM, "exposure": "forest_loss_ha", "term": "forest_loss_ha",
            "estimate": estimate, "std_error": standard_error, "statistic": model.tstat().loc["forest_loss_ha"],
            "p_value": model.pvalue().loc["forest_loss_ha"], "conf_low": estimate - 1.96 * standard_error,
            "conf_high": estimate + 1.96 * standard_error, "n_intervals": model._N,
            "n_populations": model_data["population_id"].nunique(), "n_locations": model_data["location_id"].nunique(),
            "n_countries": model_data["country"].nunique(), "zero_loss_share": (model_data["forest_loss_ha"] == 0).mean(),
        }
    )

regression_coefficients = pd.DataFrame(results).sort_values(["group", "buffer_km", "exposure"])
regression_coefficients.to_csv(table_dir / "regression_coefficients.csv", index=False)
# pyfixest model objects deliberately contain non-serialisable callbacks. The
# coefficient table is the reproducible statistical output; this small manifest
# records the names and formula of every fitted model without pretending a
# Python pickle is a durable interchange format.
pd.DataFrame(
    {
        "model_name": list(models),
        "formula": [f"log_abundance_change ~ exposure + interval_duration | {FIXED_EFFECTS}"] * len(models),
    }
).to_csv(table_dir / "regression_model_manifest.csv", index=False)

coverage = exposure.groupby("buffer_km", as_index=False).agg(
    n_intervals=("interval_id", "size"), n_populations=("population_id", "nunique"), n_locations=("location_id", "nunique"),
    n_countries=("country", "nunique"), zero_loss_share=("forest_loss_ha", lambda values: (values == 0).mean()),
)
coverage.to_csv(table_dir / "exposure_coverage.csv", index=False)

# Exact leave-one-location-out slopes for the unadjusted primary scatter fit.
total_n = len(primary_data)
total_sx = primary_data["forest_loss_ha"].sum()
total_sy = primary_data["log_abundance_change"].sum()
total_sxx = (primary_data["forest_loss_ha"] ** 2).sum()
total_sxy = (primary_data["forest_loss_ha"] * primary_data["log_abundance_change"]).sum()
location_sums = primary_data.groupby("location_id", as_index=False).agg(
    n_removed=("interval_id", "size"), sx=("forest_loss_ha", "sum"), sy=("log_abundance_change", "sum"),
    sxx=("forest_loss_ha", lambda values: (values**2).sum()),
    sxy=("forest_loss_ha", lambda values: (values * primary_data.loc[values.index, "log_abundance_change"]).sum()),
)
location_sums["n_remaining"] = total_n - location_sums["n_removed"]
numerator = (total_sxy - location_sums["sxy"]) - (total_sx - location_sums["sx"]) * (total_sy - location_sums["sy"]) / location_sums["n_remaining"]
denominator = (total_sxx - location_sums["sxx"]) - (total_sx - location_sums["sx"]) ** 2 / location_sums["n_remaining"]
location_sums["slope_without_location"] = numerator / denominator
full_slope = np.polyfit(primary_data["forest_loss_ha"], primary_data["log_abundance_change"], deg=1)[0]
location_sums["full_unadjusted_slope"] = full_slope
location_sums["slope_shift"] = location_sums["slope_without_location"] - full_slope
location_sums = location_sums.sort_values("slope_shift", key=lambda values: values.abs(), ascending=False)
location_sums.to_csv(table_dir / "primary_location_unadjusted_influence.csv", index=False)


# Write regression diagnostics and figures.

plt.style.use("seaborn-v0_8-whitegrid")
annotation = f"n = {len(primary_data):,} intervals; {primary_data['location_id'].nunique():,} locations"
fig, axis = plt.subplots(figsize=(8.5, 5.5))
axis.scatter(primary_data["forest_loss_ha"], primary_data["log_abundance_change"], alpha=0.08, s=5, color="#2C7FB8", edgecolors="none")
line_x = np.linspace(primary_data["forest_loss_ha"].min(), primary_data["forest_loss_ha"].max(), 200)
line_slope, line_intercept = np.polyfit(primary_data["forest_loss_ha"], primary_data["log_abundance_change"], deg=1)
axis.plot(line_x, line_intercept + line_slope * line_x, color="#D7301F", linewidth=1.5)
axis.text(0.98, 0.97, annotation, transform=axis.transAxes, ha="right", va="top", bbox={"facecolor": "white", "edgecolor": "0.7"})
axis.set(title="LPD population abundance change and cumulative forest loss", xlabel="Cumulative Hansen forest loss during interval (ha)", ylabel="Log abundance change")
fig.tight_layout()
fig.savefig(figure_dir / "global_scatter_primary_1km.png", dpi=220)
plt.close(fig)

if not eligible_aez.empty:
    aez_names = sorted(eligible_aez["AEZ"].unique())
    n_columns = 4
    n_rows = math.ceil(len(aez_names) / n_columns)
    fig, axes = plt.subplots(n_rows, n_columns, figsize=(11, max(4, n_rows * 2.2)), squeeze=False)
    for axis, aez_name in zip(axes.flat, aez_names):
        aez_data = eligible_aez.loc[eligible_aez["AEZ"] == aez_name]
        axis.scatter(aez_data["forest_loss_ha"], aez_data["log_abundance_change"], alpha=0.10, s=4, color="#2C7FB8", edgecolors="none")
        slope, intercept = np.polyfit(aez_data["forest_loss_ha"], aez_data["log_abundance_change"], deg=1)
        x_values = np.linspace(aez_data["forest_loss_ha"].min(), aez_data["forest_loss_ha"].max(), 100)
        axis.plot(x_values, intercept + slope * x_values, color="#D7301F", linewidth=0.8)
        axis.set_title(aez_name, fontsize=9)
    for axis in axes.flat[len(aez_names) :]:
        axis.set_visible(False)
    fig.suptitle("LPD abundance change and forest loss by AEZ", y=1.01)
    fig.supxlabel("Cumulative Hansen forest loss during interval (ha)")
    fig.supylabel("Log abundance change")
    fig.tight_layout()
    fig.savefig(figure_dir / "aez_scatter_primary_1km.png", dpi=220, bbox_inches="tight")
    plt.close(fig)

aez_results = regression_coefficients.loc[
    ~regression_coefficients["group"].isin(["global", "global_unadjusted"])
    & (regression_coefficients["exposure"] == "forest_loss_ha")
].sort_values("estimate")
if not aez_results.empty:
    fig, axis = plt.subplots(figsize=(8.5, 6.5))
    y_positions = np.arange(len(aez_results))
    axis.axvline(0, color="0.6")
    axis.errorbar(
        aez_results["estimate"], y_positions,
        xerr=[aez_results["estimate"] - aez_results["conf_low"], aez_results["conf_high"] - aez_results["estimate"]],
        fmt="o", color="#2C7FB8", capsize=2,
    )
    axis.set_yticks(y_positions, aez_results["group"])
    axis.set(title="AEZ-specific association of forest loss with LPD abundance change", xlabel="Regression coefficient per hectare", ylabel="AEZ")
    fig.tight_layout()
    fig.savefig(figure_dir / "aez_forest_loss_coefficients_1km.png", dpi=220)
    plt.close(fig)

print(f"Analysis complete. Main results: {table_dir / 'regression_coefficients.csv'}")
