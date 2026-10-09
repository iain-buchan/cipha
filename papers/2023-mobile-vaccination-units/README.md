# Mobile vaccination units and COVID-19 vaccination uptake in Cheshire and Merseyside: synthetic control analysis

**Paper.** Zhang X, Tulloch JSP, Knott S, Allison R, Parvulescu P, Buchan IE, García-Fiñana M, Piroddi R, Green MA, Baird S, Barr B. Evaluating the impact of using mobile vaccination units to increase COVID-19 vaccination uptake in Cheshire and Merseyside, UK: a synthetic control analysis. *BMJ Open* 2023;13:e071852. https://doi.org/10.1136/bmjopen-2023-071852

**What it does.** Builds LSOA-level intervention and comparison groups from mobile vaccination unit deployment records (geocoded from postcodes with PostcodesioR), derives a balanced LSOA panel from the CIPHA vaccination data, estimates the effect of the units on uptake with synthetic control (microsynth, permutation and jackknife inference), then runs individual-level analyses with interactions, cumulative-uptake figures by ethnicity and local authority, and sensitivity analyses (excluding ethnicity, excluding pop-in sites, distance thresholds and spatial spill-over, individual-level survival analysis), with the maps in the paper.

**Source.** Copied from https://github.com/civicdatacoop/mobileVaxUnit (commit `53cadb6c`, 6 August 2023), the repository named in the paper’s data availability statement. Author: Xingna Zhang. File paths were already masked by the author. **No licence file was present in the source repository**; the authors’ agreement is needed before this folder is treated as covered by the repository’s MIT licence (see `ATTRIBUTION.md`). Scripts are unchanged.

## Scripts

| Script | Purpose |
|---|---|
| `mobile_vax_lookup_CDCG.R` | Builds the LSOA-level lookup that defines intervention and non-intervention LSOAs from the mobile unit deployment records |
| `mobile_vax_units_CDCG.R` | All other analysis in 23 numbered sections: data preparation, LSOA panel, synthetic control, individual-level models, results tables and heat map, uptake figures, sensitivity analyses, maps |

## Data

Individual-level vaccination data are sensitive and could not be shared; the paper states that data may be obtained from a third party. Deployment records came from the Cheshire and Merseyside vaccination programme. Public inputs include the 2011 rural-urban classification and LSOA geographies.

## R packages

AER, DescTools, Hmisc, ISOweek, MatchIt, Matching, MuMIn, PostcodesioR, aweek, biostat3, biscale, brm, colourvalues, cowplot, data.table, dplyr, fuzzyjoin, gee, geepack, ggcorrplot, ggplot2, ggspatial, gifski, grid, gridExtra, gtsummary, imputeTS, janitor, jtools, kableExtra, knitr, leaflet, lubridate, mapview, margins, mice, microsynth, plm, readxl, rgdal, scales, sf, sp, spdep, splitstackshape, stargazer, stringdist, stringr, survey, survival, survminer, targeted, tidyr, tmap, tsibble, viridis, viridisLite. (`rgdal` has since been retired from CRAN; `sf` and `terra` provide the same functions.)
