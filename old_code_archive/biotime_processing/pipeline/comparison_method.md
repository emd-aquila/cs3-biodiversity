# BioTIME and PREDICTS Comparison Method

## Question

Would BioTIME add enough terrestrial abundance data from studies starting in
2000 or later to justify a
harmonization pilot with PREDICTS, after allowing for possible redundancy?

The answer from this first screen is yes. The datasets should not be row-bound
directly, but BioTIME appears to contain substantial incremental information.

## Available Source Files and Package Tools

### BioTIME

The official BioTIME 2.0 Zenodo record provides:

- `biotime_v2_query_15April25.rds`: observation records joined to species names.
- `biotime_v2_metadata_15April25.csv`: study-level metadata.
- `references_biotime_v2_15April25.csv`: citations for individual studies.
- `biotime_v2_sql_15April25.sql`: the database SQL dump.

Source: <https://zenodo.org/records/15222193>

The `BioTIMEr` package provides recommended tools for working with BioTIME
communities, including gridding, rarefaction, and diversity metrics. Its bundled
data are a sample subset; full versions remain available through BioTIME.

Sources:

- <https://cran.r-project.org/package=BioTIMEr>
- <https://biotimehub.github.io/BioTIMEr/>

### PREDICTS

The `predictsr` package accesses the PREDICTS database and metadata from the
Natural History Museum data portal. Relevant functions are:

- `LoadPredictsData()`: load or download full extracts with an RDS cache and
  companion JSON metadata file.
- `GetPredictsData()`: download full 2016 and/or 2022 release extracts.
- `GetSitelevelSummaries()`: download standardized site-level summaries.
- `GetColumnDescriptions()`: download descriptions of the extract columns.

Source: <https://cran.r-project.org/package=predictsr>

## Filters Applied

### BioTIME

The study-level metadata filter keeps studies where:

1. `REALM == "Terrestrial"`.
2. `AB_BIO` contains `"A"`, indicating that abundance data are available.
3. `START_YEAR >= 2000`.

The observation-level filter then keeps rows where:

1. The study passed the metadata filter.
2. `YEAR >= 2000`.
3. `ABUNDANCE` is non-missing.

This produced:

The generated `biotime_filter_summary.csv` records the resulting counts.

### PREDICTS

PREDICTS is already a terrestrial database, so no realm filter is necessary.
The full cached `predictsr` extract is filtered where:

1. `Diversity_metric_type == "Abundance"`.
2. The earliest abundance observation for the PREDICTS study unit is on or
   after January 1, 2000.

The original PREDICTS extract contains a biogeographic `Realm` field, but it is
not relevant to OV calculation, geotagging, or provenance. The cleaned output
does not retain it.

The generated `predicts_filter_summary.csv` records the resulting counts.

## Slim Output Fields

The cleaned extracts intentionally retain only fields needed for:

1. OV calculation, including taxonomic fields, abundance measurements, sampling
   effort, biome, land use, and use intensity.
2. Site/date geotagging, including coordinates and sample dates.
3. Provenance, including source, study, site, and citation identifiers.

The generated `schema_inventory.md` and `schema_inventory.csv` record every
original field and whether the slim pipeline retains it.

## Redundancy Screening

The databases do not share a universal study identifier, so the pipeline emits
candidate overlaps for manual review rather than deleting records.

### Citation Screen

For BioTIME, the pipeline parses each BibTeX citation into author, year, title,
and DOI fields. For PREDICTS, it parses the abbreviated `Reference` field. It
then compares normalized first-author surname and publication year keys.

The generated `reference_overlap_candidates.csv` records the current citation
candidates. These are weak signals and require manual review.

### Spatiotemporal Screen

The pipeline summarizes each database to study-level observation centroids and
year ranges, then flags pairs where:

1. Observation year ranges overlap.
2. Centroids are at most 10 km apart.

It separately counts pairs within 1 km as stronger spatial candidates.

The generated `spatiotemporal_overlap_candidates.csv` records the current
spatiotemporal candidates.

Nearby centroids do not prove redundancy. They can also reflect separate
studies in well-sampled landscapes.

## Initial Assessment

The generated `database_comparison_summary.csv` and `comparison_report.md`
record the current assessment after applying the configured filters.

## Why a Direct Row Bind Would Be Wrong

BioTIME and PREDICTS have different analytical designs:

- BioTIME is centered on assemblage time series and repeated sample events.
- PREDICTS is centered on standardized spatial comparisons of biodiversity
  across land use and pressure gradients.
- Site identifiers, sampling effort, taxonomic resolution, and repeated
  measurements need explicit harmonization.
- BioTIME study licences and underlying citations must be retained.
- PREDICTS extracts have non-commercial licensing constraints documented by
  `predictsr`.

## Recommended Pilot

1. Manually classify the citation candidates.
2. Manually review the spatiotemporal candidates using titles, methods, taxa,
   sample design, and coordinates.
3. Select a small set of clearly non-overlapping BioTIME studies.
4. Standardize them to a site-date-taxon table with sampling-effort metadata.
5. Apply BioTIMEr-style gridding and rarefaction where necessary.
6. Recalculate the biodiversity metric for PREDICTS alone and for the pilot
   union.
7. Compare the change in usable sites, temporal coverage, geographic coverage,
   AEZ coverage, and downstream model estimates before scaling up.

Generated candidate tables and the current results are in `output/`.
