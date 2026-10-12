# Rapid antigen testing for school-aged children in Cheshire and Merseyside

**Paper.** Hughes DM, Bird SM, Cheyne CP, Ashton M, Campbell MC, García-Fiñana M, Buchan I. Rapid antigen testing in COVID-19 management for school-aged children: an observational study in Cheshire and Merseyside, UK. *Journal of Public Health (Oxford)* 2022;fdac003. https://doi.org/10.1093/pubmed/fdac003

**What it does.** Describes lateral flow and PCR testing among 5 to 18 year olds in Cheshire and Merseyside between 6 November 2020 and 31 July 2021 from the pillar 2 testing extract: daily counts of LFTs and PCRs with positivity and void percentages (Figures 1 and 2); for primary (5 to 11) and secondary (12 to 18) school ages separately, whether a positive LFT was followed by a confirmatory PCR within five days and what that PCR showed; logistic regression models (`glm`, binomial) of confirmatory-PCR uptake and of agreement between LFT and PCR by age, sex, ethnicity, deprivation decile, number of previous LFTs and self-test versus assisted test; false positive proportions by month with exact binomial confidence intervals, overall and for self-tests versus test-centre tests; Ct values by age and by LFT–PCR agreement; and monthly tables of tests, positives and confirmations against the registered population of each age group.

**Source.** Sent by David Hughes on 12 October 2026 as `SchoolsTesting.R`, his working script for the analysis. MIT licence, copyright (c) 2021 David M Hughes (`LICENSE`).

**Changes made here.** Network-drive paths replaced by relative `data/<extract date>/<file>` paths (lines 6, 68, 76, 77, 551, 871 and 872); Windows line endings normalised; two lines of notes pasted into the script (lines 714 and 715) commented out with a `## [pasted console output ...]` marker so the file parses. Nothing else was changed. The script was written to be run interactively, section by section.

## Script

`SchoolsTesting.R` (about 1,000 lines), in order: dataset construction and restriction to 5 to 18 year olds in Cheshire and Merseyside (lines 1 to 95); daily LFT and PCR plots (96 to 206); confirmatory PCR follow-up for secondary (207 to 354) and primary (355 to 502) school ages; positive LFTs per day with confirmations (503 to 547); deprivation merge and the three logistic models (548 to 640); accuracy by days between tests and false positive rates over time (641 to 816); self-test false positive rates by month (817 to 868); registered population and monthly tables (869 to end).

## Data

| Input | Description |
|---|---|
| `data/2021-08-06/COVIDPILLAR2_VCam_MastP2.RData` | Pillar 2 testing extract for Cheshire and Merseyside residents |
| `data/LiverpoolLSOAcodes.csv` | LSOA codes used to restrict to Cheshire and Merseyside |
| `data/imd2019.csv` | Index of Multiple Deprivation 2019 by LSOA (public, from MHCLG) |
| `data/2021-03-05/Demographics.txt`, `data/2021-03-29/Demographics.txt` | Registered population by age for Liverpool (CIPHA), for the denominators in the monthly tables |
| `data/2021-06-15/PHE_Positive.txt`, `PHE_Negative.RData` | PHE positive and negative test files (loaded in a commented-out section) |

Pseudonymised data are accessible via CIPHA, now the NHS Cheshire and Merseyside Data into Action programme, on application to its data access group; they cannot be released openly.

## R packages

KernSmooth, PropCIs, RcppRoll, devEMF, dplyr, gplots.
