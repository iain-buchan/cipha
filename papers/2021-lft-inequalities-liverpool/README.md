# Social and spatial inequalities in rapid lateral flow testing, Liverpool, November 2020 to January 2021

**Paper.** Green MA, García-Fiñana M, Barr B, Burnside G, Cheyne CP, Hughes D, Ashton M, Sheard S, Buchan IE. Evaluating social and spatial inequalities of large scale rapid lateral flow SARS-CoV-2 antigen testing in COVID-19 management: An observational study of Liverpool, UK (November 2020 to January 2021). *Lancet Regional Health Europe* 2021;6:100107. https://doi.org/10.1016/j.lanepe.2021.100107 (preprint: medRxiv https://doi.org/10.1101/2021.02.10.21251256)

**What it does.** Evaluates who came forward for voluntary asymptomatic lateral flow testing in the Liverpool pilot, who tested more than once, and who tested positive, by neighbourhood (LSOA) deprivation, ethnicity, age and sex, using Bayesian spatial (BYM) models with expected counts, plus individual-level multilevel sensitivity analyses, a check on confirmatory PCR uptake after positive LFTs, and maps.

**Source.** Copied from https://github.com/markagreen/asymptomatic_testing_evaluation (commit `3fe789f6`, 23 March 2021), MIT licence, copyright (c) 2021 Mark A Green; licence text in `LICENSE`. Code by Mark Green (lead), Ben Barr and David Hughes. The scripts are unchanged apart from removing the original README files, whose content is reproduced below.

## Scripts (`scripts/`)

Workflow: run `clean_test_data.R` first, then each `spatial_model_*.R` (which calls the matching `tidy_analytical_*.R` and the descriptives scripts), then the plotting scripts, then the supplementary analyses.

| Script | Purpose |
|---|---|
| `clean_test_data.R` | Cleans the record-level test data and imputes missing ethnicity (single imputation); writes the clean dataset used by everything else |
| `tidy_analytical_dataset.R`, `tidy_analytical_multiple.R`, `tidy_analytical_positivity.R` | Build the LSOA-level analytical datasets: observed counts, expected counts by age, sex and ethnicity, and matched socioeconomic data, for uptake, repeat testing and positivity |
| `spatial_model_uptake.R`, `spatial_model_multiple_lfts.R`, `spatial_model_positivity.R` | BYM spatial models (INLA) for the three outcomes; each calls the tidy and descriptives scripts |
| `calculate_descriptives.R` | Summary statistics by outcome (Table 1 and Appendix B) |
| `test_descriptives.R` | Summary statistics by number of tests rather than persons |
| `summary_plots.R` | Summary statistics and graphs for the paper |
| `create_reg_plots.R` | Plots of the spatial regression results (Figures 2 to 4) |
| `multi_level_models.R` | Individual-level multilevel models for the sensitivity analyses (repeat testing and positivity) |
| `confirmatory_PCRs.R` | How often confirmatory PCRs followed positive LFTs (Appendix F) |
| `appendix_maps.R` | All maps in Appendix G |

## Data

Record-level testing data and the linked CIPHA data cannot be shared. Data are accessible via CIPHA, now the NHS Cheshire and Merseyside Data into Action programme: requests for extracts of the larger-scale data can be made to its data access group. The scripts expect the pillar 2 testing extract and LSOA lookups described in `clean_test_data.R`.

## R packages

INLA (installed from https://www.r-inla.org, not CRAN), bit64, data.table, dplyr, ggplot2, lme4, lubridate, mice, missMDA, patchwork, plyr, sf, spatialreg, spdep, tidyr, viridis, zoo.
