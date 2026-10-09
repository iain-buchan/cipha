# Related repositories

Code held by other groups and consortia that the papers in `publications.md` depend on. These are linked rather than copied: they belong to those groups, and several carry no licence.

## Source repositories for code in this repository

| Repository | Used for | Licence |
|---|---|---|
| https://github.com/markagreen/asymptomatic_testing_evaluation | `papers/2021-lft-inequalities-liverpool/` | MIT |
| https://github.com/markagreen/mapping_test_accessibility | `papers/2021-test-site-accessibility/` | MIT |
| https://github.com/markagreen/social_flip_COVID-19 | `papers/2022-delta-omicron-cohort/` | MIT |
| https://github.com/civicdatacoop/COVID-SMART-in-Liverpool | `papers/2022-asymptomatic-testing-admissions/` | none stated |
| https://github.com/civicdatacoop/mobileVaxUnit | `papers/2023-mobile-vaccination-units/` | none stated |
| https://github.com/civicdatacoop/tier3_covid_hospitalisation | `papers/2025-tier3-hospitalisations/` | none stated |

## Consortium and national infrastructure code

| Repository | Relation to this work | Licence |
|---|---|---|
| https://github.com/SurgicalInformatics/4C_mortality_score | Development code for the ISARIC4C 4C Mortality Score (Knight et al., BMJ 2020): data preparation, imputation, GAMs, LASSO, score derivation, XGBoost comparison, decision curves | none stated |
| https://github.com/SurgicalInformatics/cocin_ccp | REDCap pull and preparation scripts for the ISARIC4C CCP data used by the above | none stated |
| https://github.com/jamesscottbrown/deterioration-calculator | The calculator behind https://isaric4c.net/risk for the 4C Deterioration model (Gupta et al., Lancet Respir Med 2021) | none stated |
| https://github.com/resplab/isaric4c | Independent R package implementing both 4C models | GPL-3 |
| https://github.com/isaric4c/isaric4c.github.io | ISARIC4C consortium site, including the risk tools | MIT |
| https://github.com/nhsx/covid-chest-imaging-database | NHS AI Lab warehouse, infrastructure and dashboard for the National COVID-19 Chest Imaging Database (Jacob et al., ERJ 2020); see also `nhsx/nccid-cleaning` and `nhsx/nccid-validation-infrastructure` | MIT |
| https://github.com/KCL-BMEIS/ExeTera | Data preparation framework for the COVID Symptom Study app data (Macfarlane et al., PLoS One 2021) | Apache-2.0 |
| https://github.com/artic-network/civet | Genomic cluster investigation tool used in the Events Research Programme analysis (Burnside et al., J R Soc Med 2023) | GPL-3 |

## Platform and data infrastructure

| Repository | Relation to this work | Licence |
|---|---|---|
| https://github.com/civicdatacoop/cipha-idcr | A 2022 copy of the University of Manchester’s Greater Manchester Care Record extraction repository (https://github.com/rw251/gm-idcr): documentation, SQL templating and over 900 clinical code sets (including COVID-19 positive tests and vaccination) for the Graphnet integrated care record that CIPHA also runs on. Useful as the pattern for reproducible extraction from Graphnet | none stated in the copy |
| https://github.com/civicdatacoop/AzureTRE and `AzureTRE_manual` | Trusted research environment deployment used by the Civic Data Cooperative | see repositories |
| https://pldr.org | Place-based Longitudinal Data Resource, home of the Small Area Vulnerability Index open data (`data/savi/`) | OGL v3 |

## The Civic Data Cooperative organisation

https://github.com/civicdatacoop holds 17 public repositories of the Liverpool City Region Civic Data Cooperative, including the three synthetic control repositories above and `NorthSouthMortality` (the North-South mortality divide analysis, whose Stata counterpart is at https://github.com/iain-buchan/north-south-mortality).
