# Impact of community asymptomatic rapid antigen testing on COVID-19 hospital admissions: synthetic control study

**Paper.** Zhang X, Barr B, Green M, Hughes D, Ashton M, Charalampopoulos D, García-Fiñana M, Buchan I. Impact of community asymptomatic rapid antigen testing on covid-19 related hospital admissions: synthetic control study. *BMJ* 2022;379:e071374. https://doi.org/10.1136/bmj-2022-071374

Companion editorial: Barr B, Zhang X, Green M, Buchan I. A blueprint for synthetic control methodology: a causal inference tool for evaluating natural experiments in population health. *BMJ* 2022;379:o2712. https://doi.org/10.1136/bmj.o2712

**What it does.** Compares weekly COVID-19 hospital admissions in Liverpool’s MSOAs with a synthetic control built from similar areas elsewhere in England (microsynth, with permutation and jackknife inference) across the community testing pilot period. Three main models handle the December 2020 tier change differently (pilot period only; mean tier effect assumed; no tier effect), followed by sensitivity analyses on extra covariates, the tier-effect confidence limits, local-authority-level matching (MatchIt) and the inclusion threshold for low-testing areas, plus the cartograms, maps, summary and results tables in the paper.

**Source.** Copied from https://github.com/civicdatacoop/COVID-SMART-in-Liverpool (commit `357cea13`, 22 June 2022), the repository named in the paper’s data sharing statement. Authors: Xingna Zhang and Ben Barr. **No licence file was present in the source repository**; the authors’ agreement is needed before this folder is treated as covered by the repository’s MIT licence (see `ATTRIBUTION.md`).

**Changes made here.** Lines 57 to 62 of `Liverpool_COVID_SMART_Hospitalisation.R` originally held the authors’ local and network drive paths. They now read the three data folders from environment variables (`CIPHA_ADMISSIONS_DIR`, `CIPHA_PROJECT_DIR`, `CIPHA_SHARING_DIR`) with placeholder defaults. Nothing else was changed.

## Script

`Liverpool_COVID_SMART_Hospitalisation.R` (one script, about 2,200 lines): loads MSOA-level weekly admissions and case data, builds the panel, estimates the tier-3 effect, fits the synthetic control models, runs the sensitivity analyses and produces the figures and tables.

## Data

MSOA-level COVID-19 admissions were provided by NHS Digital under data sharing agreement DARS-NIC-16656-D9B5T-v3.10 and are available by application to NHS Digital. All other inputs are publicly available (ONS, PHE/UKHSA coronavirus dashboard, Fingertips, geography lookups). The source repository notes that one dataset could not be shared pending disclosure control.

## R packages

CausalImpact, FNN, GGally, MASS, MatchIt, RColorBrewer, aweek, biostat3, broom, caret, cartogram, data.table, dplyr, epitools, fingertipsR, foreign, fuzzyjoin, geepack, ggplot2, ggpmisc, ggrepel, ggspatial, haven, janitor, mapproj, maptools, microsynth, panelr, psych, randomForest, readODS, readxl, regclass, scales, sf, sjstats, splitstackshape, statar, stringr, survey, tidyverse, tmap, tmaptools, tsibble, viridis. (The paper reports R 4.0.3; `maptools` has since been retired from CRAN.)
