# BII metric workflow

This workflow independently implements the PREDICTS Biodiversity Intactness
Index (BII) pathway. It fits two terrestrial abundance-data models:

1. relative total abundance by land-use pressure class; and
2. balanced Bray-Curtis compositional similarity between minimally used primary
   vegetation and matched sites in the same PREDICTS study.

For each land-use cell, the workflow area-weights the modelled abundance and
compositional responses separately, then multiplies the two projected component
maps to calculate BII. This follows the structure of the De Palma PREDICTS
tutorial, while making the land-use source interchangeable through one strict
long-format contract.

Taxon-specific models are reported for Mammals, Birds, Amphibians, Reptiles,
Invertebrates, Plants, and Fungi. The four vertebrate groups use PREDICTS
`Class`; the three broad groups use PREDICTS `Kingdom`.
Model convergence and singularity diagnostics are exported to
`bii_model_diagnostics.csv` and should be reviewed before interpreting a
taxon-specific projection.

## Run

```sh
Rscript bii_metric/run_bii_analysis.R
```

The source PREDICTS extract and published NHM benchmark are read from the root
`00_biodiversity_data/` folder. The first run constructs and caches the
PREDICTS compositional site pairs under `bii_metric/tmp/`; this is the most
computationally intensive step.

## LUH2 projection: 2010 and 2030

`run_luh2_bii_analysis.R` projects the fitted response table directly onto
LUH2's 0.25-degree state layers, without converting the global NetCDF files to
an impractically large CSV. It uses historical LUH2 v2h for 2010 and the
SSP2-RCP4.5 MESSAGE-GLOBIOM LUH2 v2f pathway for 2030:

```sh
Rscript bii_metric/run_luh2_bii_analysis.R
```

The raw NetCDF files, LUH2 static file, and Natural Earth 1:50m country
boundaries must first be placed in the paths configured in `code/01_config.R`.
The script writes separate global, national, taxon, continent, PREDICTS-region,
model-coverage, and published-comparison tables beginning `bii_luh2_` under
`output/`. `input/luh2/README.md` records the exact direct-download locations
and the land-use crosswalk.

The LUH2 projection is a land-use-only BII analysis. LUH2 does not distinguish
primary-use intensity or plantation forest in its state layers, so the current
crosswalk maps `primf + primn` to `primary_minimal`, does not impute a
plantation class, and maps all secondary states together. These are material
differences from the full published BII production models; the generated
validation files quantify, rather than conceal, their consequences. Some
taxon-specific models lack one or more observed PREDICTS land-use classes. The
projection uses the global response for only those missing classes and labels
such results `hybrid_global_fallback`; inspect
`bii_luh2_model_coverage.csv` before interpreting those taxa.

## Optional V2: land-use intensity and related pressures

V1 remains the default and all V1 code paths and outputs are retained. V2 is
an isolated, opt-in model intended to be materially closer to the PREDICTS/NHM
projection framework. It adds land use × use intensity, human population
density, and their documented interactions; road density at 1 km and 50 km
is available as a separate opt-in extension.
It estimates all transformations and caps from the PREDICTS training data,
then applies those same caps to gridded projections.

V2 deliberately implements the core, spatially projectable pressure terms; it
is not yet a byte-for-byte replication of the published production models.
In particular, environmental pair covariates, the published model-selection /
bootstrap procedure, and a fully observed annual land-use downscaling remain
separate future work. Its outputs should therefore be validated against the
published historical maps before being used as an equivalence claim.

Enable it only after supplying the reviewed inputs described in
`input/v2/README.md`:

```sh
CS3_BII_VERSION=v2 Rscript bii_metric/run_bii_analysis.R
```

V2 writes only `bii_v2_*` artefacts and never overwrites V1. If V2 is not
ready or gives unsatisfactory diagnostics, omit the environment variable and
the existing V1 workflow runs unchanged. A direct V2 LUH2 comparison is also
available once the public NCAR SSP2 population files are in `input/v2/raw/`:

```sh
CS3_BII_VERSION=v2 Rscript bii_metric/run_luh2_bii_analysis.R
```

It uses the explicit initial LUH2 intensity mapping in
`input/crosswalks/luh2_to_bii_v2_crosswalk.csv` and an aggregated 0.5-degree
comparison grid. It is a reproducible sensitivity test—not a replacement for
the Global-Land-Systems intensity allocator used in published production maps.

## Published 2010/2030 benchmark: input-equivalent profile

The bundled 2010/2030 comparator is the deprecated 2021 NHM Biodiversity
Trends Explorer release, not the annual tropical-forest analysis in De Palma
et al. (2021). Its most closely documented PREDICTS scenario implementation
uses four linked pressure inputs:

1. LUH2 state fractions for primary, secondary, cropland, pasture and urban;
2. LUH2 transition histories to divide secondary vegetation into young
   (<30 years), intermediate (30-50 years), and mature (>50 years) shares;
3. the 5-arc-minute Global Land Systems reference map, reclassified using
   Newbold et al. (2015), then statistically allocated into minimal/light/
   intense shares from land-use extent, population density, UN subregion and
   their interactions; and
4. historical/future gridded human population density.

The existing V1 and direct V2 LUH2 results are *not* input-equivalent to this
profile: V1 is deliberately land-use-only, and V2 currently uses an explicit
fixed-intensity sensitivity mapping. The exact published Global Land Systems
conversion is versioned in
`input/crosswalks/global_land_systems_to_predicts_intensity.csv`; it is ready
to apply once the original 5-arc-minute reference raster is obtained. Do not
label a run as a published-model reproduction until it also uses the LUH2
transition histories and compatible population grids.

## Hill 2018 LUH2-compatible PREDICTS re-curation

The current public PREDICTS releases retain broad land-use labels. Hill et al.
(2018) re-curated sites to the expanded LUH2 classes: primary forest/non-forest,
young/intermediate/mature secondary vegetation, annual/perennial/nitrogen-fixing
cropland, managed pasture, rangeland, and urban. Run the evidence-based local
re-curation with:

```sh
Rscript bii_metric/run_hill2018_recuration.R
```

It uses the LUH2 static `fstnf` mask to split georeferenced primary sites and
retains PREDICTS' existing secondary-age labels. For cropland, pasture and
plantation forest, it applies the versioned, inspectable rules in
`input/crosswalks/hill2018_recuration_text_rules.csv` to retained site
descriptions and reference titles. Timber plantations are explicitly excluded
because LUH2 has no equivalent state; permanent woody crops are assigned to
perennial cropland when their description supports that conclusion.

The output `predicts_hill2018_site_recuration.csv` records each decision and
its evidence. `predicts_hill2018_recuration_review_queue.csv` contains sites
for which the public extract does not retain enough information to reproduce
Hill's original paper-by-paper/data-provider curation. Those sites are not
silently assigned a class and should be excluded from a strict Hill-compatible
model until evidence is obtained. This workflow does not modify the source
PREDICTS extract.

## Optional V3: strict re-curation and detailed LUH2 states

V3 is a separate, opt-in pathway built on the strict re-curated subset. It
fits abundance and compositional-similarity models using detailed annual,
perennial and nitrogen-fixing cropland; managed pasture and rangeland; urban;
and young/intermediate/mature secondary vegetation. It includes human
population density and land-use × intensity interactions.

```sh
Rscript bii_metric/run_v3_luh2_bii_analysis.R
```

V3 leaves V1 and V2 untouched and writes only `bii_v3_*` artefacts. Its LUH2
projection uses 1-degree, area-weighted aggregation of the 0.25-degree states.
It uses a reproducible net-change secondary-age proxy from 1960 onward:
increases in `secdf + secdn` enter young cohorts, declines remove cohorts
proportionally, and all vegetation already secondary in 1960 is mature. This
is not Hill's exact transition-history calculation; that requires LUH2's
separate 16-GB historical transition archive.

The present intensity allocator is transparent but provisional: for each
detailed LUH2 class it uses the minimal/light/intense proportions among the
strictly retained re-curated PREDICTS sites. The metadata table records this
choice. Replacing it with the published Global Land Systems/HPD/subregion
allocator is the next compatibility upgrade, so V3 is a sensitivity analysis,
not a claimed reconstruction of the NHM production maps.

`bii_v211_v3_validation_*` compares the overlapping 2010 and 2020 results to
NHM v2.1.1. `bii_v1_v2_v3_nhm_global_comparison.csv` is the compact,
year-aligned global comparison; V2 contributes its available 2010 value.

## Required Landsat input

Place `landsat_landuse_long.csv` in `input/landsat_landuse/`. It must contain
one row per source class in each spatial cell and year:

| Column | Meaning |
| --- | --- |
| `cell_id`, `year`, `source_class` | Unique cell/time and the original land-use label. |
| `share` | Fraction of the cell in that source class; sums to one per cell/year. |
| `area_km2` | Total cell area, repeated across its source-class rows. |
| `region`, `country`, `scenario` | Optional but strongly recommended identifiers. `region` should use UN-region labels. |

`input/crosswalks/landsat_to_bii_crosswalk.csv` maps every `source_class` to a
PREDICTS-compatible BII class. A tree-cover class alone is not sufficient to
identify primary, secondary, or plantation forest. Resolve those labels with
an ancillary forest-history/management layer before running the workflow; the
crosswalk will fail rather than quietly guess.

## Validation

The workflow compares historical global, five UN-region, and ISO3 country
values against the bundled published PREDICTS/NHM BII series. Exact agreement
is not expected unless the same pressure layers, class definitions, resolution,
and additional pressure predictors are used. The validation tables quantify the
difference, signed bias, and Pearson/Spearman agreement rather than treating
it as a pass/fail test. The bundled benchmark is aggregate-taxa only, so taxon
models are reported as response functions and coverage tables rather than
compared to a nonexistent taxon-specific benchmark.

The workflow always exports the bundled historical benchmark separately as
`bii_published_benchmark_global.csv`, `bii_published_benchmark_regions.csv`,
and `bii_published_benchmark_countries.csv`. These tables are labelled
published benchmarks—not outputs calculated from the supplied Landsat data.

## EPPA adapter

Place EPPA output in `input/eppa/eppa_landuse_long.csv` with
`scenario`, `year`, `region`, `source_class`, and `area_km2`. The adapter turns
each EPPA region into a projection cell, converts areas to shares, and applies
the reviewed `eppa_to_bii_crosswalk.csv`. Thus `06_project_bii.R` uses exactly
the same projection calculation for Landsat and EPPA data.

Use `model_variant = "land_use_intensity"` only once fractions can be supplied
or scenario-expanded for every land-use intensity. The EPPA adapter then uses
`eppa_intensity_scenarios.csv` to generate low/central/high intensity fractions
from an explicit, editable assumption table.

## NHM BII v2.1.1 raster benchmark: 2000--2020

The NHM v2.1.1 limited release is a separate aggregate-taxa raster benchmark
for 2000, 2005, 2010, 2015, and 2020. It is not the bundled 2021
1970--2050 benchmark. Download the ZIP manually from the [NHM data
portal](https://data.nhm.ac.uk/dataset/bii-developed-by-nhm-v2-1-1-limited-release/resource/c4c281c4-befa-4e1b-a162-ba2f25e5ae82)
and place it at:

```
00_biodiversity_data/bii/v2_1_1/raw/bii-v2-1-1-nhm-data-portal.zip
```

Summarise just the published maps with:

```sh
Rscript bii_metric/run_nhm_v211_benchmark.R
```

This converts the documented 0--100 BII raster values to proportions, then
exports area-weighted global values and matched Natural-Earth-based national,
continental, and PREDICTS broad-region tables beginning `bii_v211_published_`.
To generate a matched V1 LUH2 projection at all five years and compare it to
those published summaries, run:

```sh
Rscript bii_metric/run_nhm_v211_comparison.R
```

The latter writes its V1 projections under the distinct
`bii_luh2_v211_years_` prefix, leaving the normal 2010/2030 outputs untouched,
and writes differences, MAE, signed bias, and Pearson/Spearman agreement to
`bii_v211_v1_validation_*`. Existing direct V2 LUH2 outputs are also compared
for their overlapping year (currently 2010) only. The public v2.1.1 archive
contains aggregate BII maps rather than taxon-specific layers, so it can test
the aggregate model and geographic aggregation, but cannot validate our
Mammal, Bird, Amphibian, Reptile, Invertebrate, Plant, or Fungi response
functions individually.
