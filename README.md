# CIPHA: Combined Intelligence for Population Health Action

Code, data and study documents from the COVID-19 research built on CIPHA, the Cheshire and Merseyside population health data platform, gathered into one place so that the methods can be reused.

## About CIPHA

CIPHA was built at pace in 2020 as the Liverpool City Region’s data science response to COVID-19. It links health and care records for the 2.7 million people of Cheshire and Merseyside on the Graphnet platform, and during the pandemic it underpinned the evaluation of the world’s first voluntary mass rapid antigen testing of people without symptoms (Liverpool, November 2020), the Events Research Programme pilots in the Liverpool City Region, weekly intelligence reports to the Chief Medical Officer for England, and a series of natural-experiment evaluations of testing, restrictions and vaccination. It is now sustained by NHS Cheshire and Merseyside’s Data into Action programme as system infrastructure for care, planning and research, with data science delivered by the University of Liverpool’s Civic Health Innovation Labs (CHIL).

## What is here

| Folder | Paper or output | Origin |
|---|---|---|
| `papers/2021-lft-inequalities-liverpool/` | Green et al., *Lancet Reg Health Eur* 2021: social and spatial inequalities in lateral flow testing | Mark Green, MIT |
| `papers/2021-test-site-accessibility/` | Green, *People, Place and Policy* 2021 and the Liverpool pilot evaluation: test-site accessibility and location-allocation | Mark Green, MIT |
| `papers/2022-asymptomatic-testing-admissions/` | Zhang et al., *BMJ* 2022: synthetic control of hospital admissions during community testing | Civic Data Cooperative, licence to confirm |
| `papers/2022-delta-omicron-cohort/` | Green et al., *BMC Infect Dis* 2022: 2.7M-person cohort through Delta and Omicron | Mark Green, MIT |
| `papers/2023-mobile-vaccination-units/` | Zhang et al., *BMJ Open* 2023: mobile vaccination units, synthetic control | Civic Data Cooperative, licence to confirm |
| `papers/2024-smart-release-return/` | Zhang et al., *BMC Infect Dis* 2024: daily dual lateral flow self-testing for NHS staff (protocol, SAP, anonymised data at the repository root) | This repository |
| `papers/2025-tier3-hospitalisations/` | Zhang et al., *BMJ Open* 2025: December 2020 tiers and hospitalisations, synthetic control | Civic Data Cooperative, licence to confirm |
| `methods/lft-sensitivity-recalibration/` | Petersen et al., *Clin Epidemiol* 2021: recalibrating PCR-relative to absolute lateral flow test sensitivity (R and Python, tested against the paper) | This repository |
| `data/savi/` | Daras et al., *JECH* 2021: Small Area Vulnerability Index, pointers to the open data | PLDR, OGL v3 |
| `docs/publications.md` | Every COVID-19 publication from this group, with the status of its code | |
| `docs/related-repositories.md` | Code held by other groups and consortia that these papers depend on, to link rather than copy | |
| `docs/paper-folder-template.md` | How to add the code for another paper | |
| `ATTRIBUTION.md` | Where every imported file came from: source repository, commit, authors, licence, changes made | |

The four `SMART_*` files at the root are the SMART Release & Return study documents and anonymised dataset. They stay at the root, unrenamed, because the published paper links to those exact paths.

## Data access

The analyses use record-level health and care data that cannot be released openly. Pseudonymised data are accessible via CIPHA, now the NHS Cheshire and Merseyside Data into Action programme (https://www.cipha.nhs.uk), and the Cheshire and Merseyside node of the NHS North West Secure Data Environment; requests for extracts of the larger-scale data are considered by its data access group. Small-area hospital admissions used in the synthetic control studies were supplied by NHS Digital under data sharing agreement DARS-NIC-16656-D9B5T-v3.10 and are available by application to NHS Digital. Each paper folder lists the inputs its scripts expect.

## Reusing the code

Each paper folder is self-contained: a README with the citation, what the analysis does, a script-by-script guide, the data each script expects and the R packages it loads. Scripts are kept as published, so they document exactly what was run; where a change was needed (removing an author’s local file paths) it is recorded in the folder README and in `ATTRIBUTION.md`. The synthetic control scripts are a worked pattern for evaluating natural experiments with small-area data (see Barr et al., *BMJ* 2022;379:o2712), the spatial inequality scripts a pattern for BYM models with expected counts, and the recalibration method a tested function ready to drop into other work.

## Licence

Files written for this repository, and the folders imported from Mark Green’s repositories, are released under the MIT licence (see `LICENSE`; each imported folder keeps its original copyright notice). The three folders imported from the Liverpool City Region Civic Data Cooperative’s repositories carried no licence; until their authors confirm, they remain under the authors’ copyright and are reproduced here as published, for scrutiny and reproduction of the papers. The SMART Release & Return anonymised data are shared as described in the paper’s data availability statement.

## Citing

Cite the paper behind the analysis you use (each folder README gives the reference) and, for the repository as a whole, see `CITATION.cff`.

## Adding a paper

Copy `docs/paper-folder-template.md` into a new `papers/<year>-<topic>/` folder, fill it in, add the scripts, and record the provenance in `ATTRIBUTION.md`. `docs/publications.md` lists the papers whose code is still to be added.
