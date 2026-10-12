# Performance of the Innova lateral flow test in the Liverpool asymptomatic testing pilot

**Paper.** García-Fiñana M, Hughes DM, Cheyne CP, Burnside G, Stockbridge M, Fowler TA, Fowler VL, Wilcox MH, Semple MG, Buchan I. Performance of the Innova SARS-CoV-2 antigen rapid lateral flow test in the Liverpool asymptomatic testing pilot: population based cohort study. *BMJ* 2021;374:n1637. https://doi.org/10.1136/bmj.n1637

**What it does.** Analyses the paired lateral flow (Innova) and PCR samples collected for the Department of Health and Social Care quality assurance of the Liverpool community testing pilot (November to December 2020). The script links the QA records to the pillar 2 testing extract by LFD barcode (for age, sex, ethnicity, dates and test centre), resolves duplicate barcodes, and computes sensitivity, specificity and predictive values against PCR with exact binomial confidence intervals, overall and by RT-qPCR cycle threshold band (mean Ct across the N, S and ORF1ab targets, and each target separately) with cumulative-sensitivity figures. It also compares the site readings with the QA-team readings and the study team’s re-appraisal (Cohen’s kappa and agreement by test centre), tabulates daily test counts, positives over time and median Ct, describes the tested population by age, sex and ethnicity, maps QA test counts by test site (adapting Mark Green’s code), and runs best-case, worst-case and full-data sensitivity analyses for how repeat samples with void results are resolved.

**Source.** Sent by David Hughes on 12 October 2026 as `QA_Accuracy_3rdFeb2021.R`, his working script for the analysis. MIT licence, copyright (c) 2021 David M Hughes (`LICENSE`).

**Changes made here.** Network-drive paths replaced by relative `data/<extract date>/<file>` paths (lines 36 and 40); Windows line endings normalised; line 21, a diagnostic `length(unique())` call on a column name carrying a byte-order-mark artefact, commented out so the file parses. Nothing else was changed. The script was written to be run interactively, section by section, in R 3.6 or 4.0 era code (factor recoding by `levels<-`); figures are written with `tiff()` and `devEMF::emf()`.

## Script

`QA_Accuracy_3rdFeb2021.R` (about 1,000 lines), in order: load and correct the QA data; link to the testing extract; de-duplicate; accuracy tables with exact CIs (`PropCIs::exactci`); sensitivity by Ct band and by gene, with cumulative sensitivity plots (Figure 1); demographics and linkage of age, sex, ethnicity, dates and test centre; daily counts and positives over time; ethnicity and age-sex summaries; inter-reader agreement (`irr::kappa2`); agreement by test centre and a `leaflet` map; re-appraisal results; void-handling sensitivity analyses.

## Data

| Input | Description |
|---|---|
| `Liverpool_MAST_LFD_QA.txt` | QA records: LFD and PCR barcodes, site LFT result, QA-team LFT result, study-team re-appraisal, PCR result, N, S and ORF1ab Ct values, test date and site |
| `data/14-12-20/CorrectedMAST13Jan2021.RData` | Pillar 2 testing extract for the pilot (pseudonymised: PseudoID, specimen ID, appointment and processing dates, age, sex, ethnicity, test centre, result) |
| `TestSiteCodes_location.csv` | Test site codes with coordinates, for the map |

None of these can be shared: the QA records and the testing extract are pseudonymised NHS Test and Trace data held under the pilot’s data sharing agreements. Pseudonymised data are accessible via CIPHA, now the NHS Cheshire and Merseyside Data into Action programme, on application to its data access group. The script also contains the two LFD barcodes whose re-appraisal results were corrected; these are specimen identifiers, not personal data.

## R packages

PropCIs, devEMF, dplyr, gplots, irr, leaflet, mapview, plotrix, sf.
