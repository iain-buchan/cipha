# SARS-CoV-2 infection through the Delta and Omicron waves by vaccination, previous infection and deprivation: 2.7M-person cohort

**Paper.** Green MA, Hungerford DJ, Hughes DM, García-Fiñana M, Turtle L, Cheyne C, Ashton M, Leeming G, Semple MG, Singleton A, Buchan I. Changing patterns of SARS-CoV-2 infection through Delta and Omicron waves by vaccination status, previous infection and neighbourhood deprivation: a cohort analysis of 2.7 M people. *BMC Infectious Diseases* 2022;22:889. https://doi.org/10.1186/s12879-022-07878-2 (preprint: medRxiv https://doi.org/10.1101/2022.04.05.22273169)

An accompanying briefing was sent to the Chief Medical Officer for England on 15 March 2022 as one of CIPHA’s weekly reports.

**What it does.** Cox regression on the CIPHA population cohort (3 June 2021 to 1 March 2022) of registered positive tests by vaccination status, previous positive test and neighbourhood deprivation, with descriptive statistics, sensitivity analyses, age-stratified models and a spatial cluster analysis of positive tests and deaths used in the CMO briefing.

**Source.** Copied from https://github.com/markagreen/social_flip_COVID-19 (commit `b1f13dbf`, 28 November 2022), MIT licence, copyright (c) 2022 Mark A Green; licence text in `LICENSE`. Unchanged apart from removing the original README files, whose content is reproduced below. The author notes that Omicron is spelled Omnicron throughout the scripts.

## Scripts (`Code/`)

| Script | Purpose |
|---|---|
| `vaccine_reinfection_survival.R` | Cleans all data and runs the main Cox regression models |
| `vaccine_reinfection_descriptives.R` | Sample characteristics and descriptive statistics (Table 1) |
| `vaccine_reinfection_sensitivity.R` | All sensitivity analyses, including some not in the paper |
| `vaccine_reinfection_plots.R` | Summary plots and graphs in the paper |
| `vaccine_reinfection_age.R` | Age-stratified analyses for the negative-test models and summary plot |
| `spatial_clusters_reinfections.R` | Spatial clusters of registered positive tests and deaths (CMO report, not in the paper) |

## Data

Electronic health records cannot be shared. Data are accessible via CIPHA, now the NHS Cheshire and Merseyside Data into Action programme; requests for extracts can be made to its data access group. Commented-out paths in the scripts show the expected input files (vaccine analysis datasets, IMD 2019, CIPHA area lookups).

## R packages

broom, data.table, dplyr, flexsurv, ggeffects, ggplot2, lubridate, nnet, patchwork, plyr, sf, spatialreg, spdep, survival, table1, tidyr, viridis, zoo.
