# Tiered restrictions in December 2020 and COVID-19 hospitalisations in England: synthetic control study

**Paper.** Zhang X, Hungerford D, Green M, García-Fiñana M, Buchan I, Barr B. Impact of tiered restrictions in December 2020 on COVID-19 hospitalisations in England: a synthetic control study. *BMJ Open* 2025;15:e086802. https://doi.org/10.1136/bmjopen-2024-086802 (preprint: medRxiv https://doi.org/10.1101/2024.02.28.24303487)

Related earlier paper on cases rather than admissions, whose code is not yet public: Zhang X, Owen G, Green MA, Buchan I, Barr B. Evaluating the impacts of tiered restrictions introduced in England, during October and December 2020 on COVID-19 cases: a synthetic control study. *BMJ Open* 2022;12:e054101. https://doi.org/10.1136/bmjopen-2021-054101

**What it does.** Compares weekly COVID-19 admissions in MSOAs placed in tier 3 on 2 December 2020 with a synthetic control built from tier 2 areas (microsynth, with permutation and jackknife inference), with subgroup analyses by deprivation and by the Alpha (new variant) period, sensitivity analyses (removing the 182 Kent MSOAs; including the 200 Liverpool City Region MSOAs and the 142 high-testing MSOAs; spatial spill-over), and the maps, figures and tables in the paper.

**Source.** Copied from https://github.com/civicdatacoop/tier3_covid_hospitalisation (commit `6563c393`, 30 January 2024), the repository named in the preprint’s data availability statement. Author: Xingna Zhang. File paths were already masked by the author (`filepath.a`, `filepath.b`, `filepath.c`). **No licence file was present in the source repository**; the authors’ agreement is needed before this folder is treated as covered by the repository’s MIT licence (see `ATTRIBUTION.md`). Script unchanged.

## Script

`tier3_hospitalisation_github.R` (one script, about 1,000 lines): loads the MSOA admissions extract and case data, sets the timetable, fits the main synthetic control model, runs the subgroup and sensitivity analyses and produces the figures and tables.

## Data

MSOA-level COVID-19 admissions were provided by NHS Digital under data sharing agreement DARS-NIC-16656-D9B5T-v3.10 and are available by application to NHS Digital. All other inputs are publicly available.

## R packages

FNN, GGally, MASS, RColorBrewer, aweek, biostat3, broom, caret, cartogram, data.table, dplyr, epitools, foreign, fuzzyjoin, geepack, ggplot2, ggpmisc, ggspatial, haven, janitor, mapproj, maptools, microsynth, panelr, psych, randomForest, readODS, readxl, regclass, scales, sf, sjstats, splitstackshape, statar, stringr, survey, svglite, tidyverse, tmap, tmaptools, tsibble, viridis. (`maptools` has since been retired from CRAN.)
