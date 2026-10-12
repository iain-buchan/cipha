# SARS-CoV-2 viral load distributions by demographics and suspected variant, Liverpool community testing, November 2020 to September 2021

**Paper.** Hughes DM, Cheyne CP, Ashton M, Coffey E, Crozier A, Semple MG, Buchan I, García-Fiñana M. Association of SARS-CoV-2 viral load distributions with individual demographics and suspected variant type: results from the Liverpool community testing pilot, England, 6 November 2020 to 8 September 2021. *Euro Surveillance* 2023;28(4):2200129. https://doi.org/10.2807/1560-7917.ES.2023.28.4.2200129

**What it does.** Uses RT-qPCR cycle threshold (Ct) values from positive pillar 2 tests in Cheshire and Merseyside as a proxy for viral load, and asks how their distribution varies by age, sex, ethnicity, symptom status, test setting, processing laboratory, swab administration, time period, number of recent lateral flow tests, vaccination and suspected variant (wild type, Alpha or Delta, inferred from S-gene target failure and calendar period). After building the analysis dataset (first positive PCR per person with Ct values, laboratories that do not report the S gene excluded, vaccination doses merged from the CIPHA immunisation extract), it compares Ct distributions with kernel density plots, Kruskal–Wallis and Wilcoxon tests with Dunn post-hoc tests, fits univariable and multivariable quantile regression models (`quantreg::rq`) for median Ct, examines age by variant and age by vaccination interactions with contrasts, plots median Ct and the share of S-gene dropout over time, produces the demographics table, and runs sensitivity analyses restricted to the period of confident Delta calling and to confirmatory PCRs after a positive LFT.

**Source.** Sent by David Hughes on 12 October 2026 as `ViralLoadComparisionsAugust2022.R` (file name as supplied), his working script for the analysis. MIT licence, copyright (c) 2022 David M Hughes (`LICENSE`).

**Changes made here.** Network-drive paths replaced by relative `data/<extract date>/<file>` paths (lines 6, 60 and 106); Windows line endings normalised; 40 lines of console output that had been pasted into the script (frequency tables after `summary()` and `table()` calls) commented out with a `## [pasted console output ...]` marker so the file parses; a missing closing bracket added at the end of the `SymptomsYes` block (line 1357), where the script as sent ended with a comma. Nothing else was changed. **Known issue:** that `SymptomsYes` block (lines 1347 to 1357) has two fewer rows than the neighbouring blocks of the demographics table (the 24.4 to 30.5 and 30.5 and above Ct bands are absent), so the table it builds is ragged at that row; the author may wish to restore them.

## Script

`ViralLoadComparisionsAugust2022.R` (about 2,200 lines), in order: dataset construction (lines 1 to 225); Ct distributions by each factor with `KernSmooth::bkde` density plots (227 to 424); multivariable linear and quantile regression, univariable quantile regressions and the results tables (425 to 482); median Ct over time (483 to 530); age effects by sampling type, symptoms, testing history, variant and vaccination (531 to 1111); the demographics table (1112 to 1615); Kruskal–Wallis, Wilcoxon and Dunn tests (1616 to 1700); longitudinal and variant-over-time plots, S-gene dropout and effect-size plots (1701 to 1931); sensitivity analyses and the supplementary contrasts table (1932 to end).

## Data

| Input | Description |
|---|---|
| `data/2021-09-08/COVIDPILLAR2_VCam_MastP2.RData` | Pillar 2 testing extract for Cheshire and Merseyside residents (PCR and LFT records with Ct values per target, test reason and symptoms, test type, laboratory, administration method, LSOA, age, sex, ethnicity) |
| `data/LiverpoolLSOAcodes.csv` | LSOA codes used to restrict to Cheshire and Merseyside |
| `data/2021-10-20/Immunisations_20OCt2021.RData` | CIPHA immunisation extract (vaccine doses and dates by patient link ID) |

Pseudonymised data are accessible via CIPHA, now the NHS Cheshire and Merseyside Data into Action programme, on application to its data access group; they cannot be released openly. Outputs written by the script (results CSVs, TIFF and EMF figures) are listed in its `write.csv`, `tiff` and `emf` calls.

## R packages

FSA, KernSmooth, PropCIs, devEMF, dplyr, gplots, quantreg, rms.
