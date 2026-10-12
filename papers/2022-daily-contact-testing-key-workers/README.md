# Daily testing of contacts as an alternative to quarantine for key workers in Liverpool

**Paper.** Marsden L, Hughes DM, Corcoran R, Cheyne CP, Ashton M, Buchan I, Coffey E, García-Fiñana M. Daily testing of contacts of SARS-CoV-2 infected cases as an alternative to quarantine for key workers in Liverpool: A prospective cohort study. *EClinicalMedicine* 2022;50:101519. https://doi.org/10.1016/j.eclinm.2022.101519

**What it does.** Analyses the returns from the organisations that took part in the Liverpool daily contact testing pilot (Merseyside Police, Merseyside Fire and Rescue Service, Alder Hey Children’s Hospital and a fourth smaller organisation), in which close contacts of cases took a daily lateral flow test for seven days instead of quarantining, with a PCR test at the end. For each organisation the script recodes the daily LFT and end-of-period PCR columns from the returns, counts tests per person, works out the gap between exposure and identification as a contact and the expected versus observed number of tests, cross-tabulates any positive LFT against the PCR result (the accuracy tables), records the reasons for missing PCRs, plots the distribution of exposure-to-identification gaps for the three main organisations, and draws the timeline figure of daily LFT results and PCR date for every PCR-positive contact, from which the day of first positive LFT relative to identification is derived.

**Source.** Sent by David Hughes on 12 October 2026 as `KeyWorkerReleaseV3.R`, his working script for the analysis. MIT licence, copyright (c) 2021 David M Hughes (`LICENSE`).

**Changes made here.** Network-drive paths replaced by relative `data/<extract date>/<file>` paths (lines 98 and 175, each of which is followed by the original local-file alternative); Windows line endings normalised. Nothing else was changed. Two as-sent quirks worth knowing: line 252 has an empty argument in a `levels<-` call, which errors if run as written, and line 289 writes the Merseyside Fire table a second time where the fourth organisation’s table was presumably intended. Comments in the script query individual records by their study ID; these are the organisations’ pseudonymous participant numbers, not personal data.

## Script

`KeyWorkerReleaseV3.R` (about 400 lines): one block per organisation (police, fire, Alder Hey, fourth organisation), each loading the return, recoding the day-by-day test columns, counting tests and building the accuracy table; then the exposure-gap bar charts, the timeline figure of PCR-positive contacts (`svglite`), and the day-of-first-positive-LFT summary.

## Data

| Input | Description |
|---|---|
| `MerseyPolice.csv`, `data/2021-09-01/MerseyFire.csv`, `data/2021-09-01/AlderHey.csv`, `WingsV3.csv` | Organisation returns: one row per contact with exposure date, date identified as a contact, daily LFT taken and result for days 1 to 7, PCR date and result, age, sex, ethnicity |

The returns contain participant-level data collected by the employers for the pilot and cannot be shared. Requests for extracts of the larger-scale pilot data are considered by the CIPHA (now NHS Cheshire and Merseyside Data into Action) data access group.

## R packages

devEMF, dplyr, lubridate, svglite.
