# LPI-like BioTIME OV analysis

This analysis measures change *within* each BioTIME assemblage. It does not
compare the absolute OV score from one assemblage with another.

The primary cohort requires a first-to-last calendar span of at least three
years. The supplemental cohort includes all assemblages with at least two
observations. Outputs include first-to-last and adjacent-observation-pair
changes, with arithmetic and geometric mean summaries.

`headline_change_summary.csv` is the primary results table. A geometric mean is
the exponentiated average log ratio: it is appropriate for multiplicative
changes and corresponds to an LPI-like aggregation. An arithmetic mean is the
ordinary average of percent changes, but can be dominated by very large positive
or negative changes when baseline values are small. They answer different
questions and are both retained.

The `MSA`, `Phylogenetic diversity`, and `Shannon diversity` metrics are the raw
components. The transformed/scaled fields additionally show the exact component
quantities used to build the composite OV score. Shannon is Shannon diversity,
not raw species richness; `SR` is available in the source table but is not part
of the current OV formula.

The script will include a protected-area comparison if
`input/wdpa_site_membership.csv` exists. It must contain `assemblage_id` and
`protected_area`, where protected area is `Inside` or `Outside`. This separation
keeps the sensitive BioTIME coordinates local.

`code/prepare_wdpa_membership.R` downloads public WDPA polygons for the
countries represented here and performs the point-in-polygon test locally. It
uses the February 2024 public WDPA polygon service—the closest accessible
release to the October 2017 WDPA source cited by Leclère et al. (2020). It
excludes proposed areas and requires the reported status year to be no later
than an assemblage's first sampled year.

To add a published LPI comparison, place the 2024 OWID/WWF-ZSL CSV at
`input/living_planet_index_by_region.csv`, then run:

```sh
Rscript ov_metric/data_exploration/lpi_like_biotime/code/run_lpi_like_biotime.R
```
