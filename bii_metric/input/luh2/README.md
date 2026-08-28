# LUH2 input

This directory holds the direct LUH2 projection input. The current run uses
these official files:

- historical LUH2 v2h `states.nc` (850--2015), saved as
  `raw/luh2_v2h_states_850-2015.nc`;
- the SSP2-RCP4.5 MESSAGE-GLOBIOM LUH2 v2f state file (2015--2100), saved as
  `raw/luh2_v2f_ssp245_states_2015-2100.nc`;
- LUH2 `staticData_quarterdeg.nc`, saved as
  `raw/luh2_v2h_static_quarterdeg.nc`.

Official direct-download URLs:

```text
https://luh.umd.edu/LUH2/LUH2_v2h/states.nc
https://luh.umd.edu/LUH2/LUH2_v2f/MESSAGE/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MESSAGE-ssp245-2-1-f_gn_2015-2100.nc
https://luh.umd.edu/LUH2/LUH2_v2h/staticData_quarterdeg.nc
```

The files are not source-controlled because the historical state file is about
5.8 GB. `derived/luh2_grid_country_lookup.csv` is generated from the static
grid and `reference/natural_earth_50m/` boundaries when the LUH2 runner starts.

## BII crosswalk

LUH2 state fractions are fractions of the whole grid cell; their total equals
the non-ice/non-water fraction. The projection divides the following state sums
by that terrestrial fraction within each cell, calculates BII there, then
weights aggregates by `carea * terrestrial_fraction`.

| LUH2 states | PREDICTS BII class | Treatment |
| --- | --- | --- |
| `primf + primn` | `primary_minimal` | Necessary land-use-only assumption; LUH2 does not identify use intensity. |
| `secdf + secdn` | `secondary` | Secondary age and biomass variables are retained for a future extended model, not used as shares. |
| `c3ann + c4ann + c3per + c4per + c3nfx` | `cropland` | Crop functional types combined. |
| `pastr + range` | `pasture` | Managed pasture and rangeland combined. |
| `urban` | `urban` | Direct mapping. |

LUH2 v2 state files have no distinct plantation state, so none is fabricated.
This makes the result a transparent land-use-only BII implementation, not a
reproduction of the published PREDICTS/NHM global BII model that also uses
land-use intensity, secondary-age detail, population density, and road density.

## Published benchmark profile

For the 2021 NHM 1970-2050 BII benchmark, LUH2 is the correct base land-use
source, but `states.nc` alone is insufficient. The published PREDICTS scenario
framework also used LUH2 *transition* files to derive fractional secondary-age
classes and a 5-arc-minute Global Land Systems reference map to calibrate
minimal/light/intense land-use shares. The latter was then projected from land
use, population density and UN subregion. The class conversion is recorded in
`../crosswalks/global_land_systems_to_predicts_intensity.csv`.

`secma` in the state NetCDF is a mean secondary age, not the fractional
young/intermediate/mature distribution required by that framework; it cannot
replace the transition calculation in a strict reproduction. Similarly, do not
replace the Global Land Systems allocator with one fixed intensity per LUH2
class when testing agreement with the published BII.
