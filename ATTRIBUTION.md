# Attribution and provenance

Every file imported from another repository is listed here with where it came from, the commit it was taken from, who wrote it, its licence, and any change made. Files not listed were written for this repository.

| Folder in this repository | Source repository | Commit (date) | Authors | Licence | Changes made |
|---|---|---|---|---|---|
| `papers/2021-lft-inequalities-liverpool/scripts/` | https://github.com/markagreen/asymptomatic_testing_evaluation | `3fe789f6` (2021-03-23) | Mark Green (lead), Ben Barr, David Hughes | MIT, copyright (c) 2021 Mark A Green; licence text kept in the folder | Original `README.md` and `scripts/README.md` replaced by the folder README, which reproduces their content |
| `papers/2021-test-site-accessibility/scripts/`, `Data/ltc.csv` | https://github.com/markagreen/mapping_test_accessibility | `3c5770f2` (2021-03-23) | Mark Green | MIT, copyright (c) 2021 Mark A Green; licence text kept in the folder | Original README files replaced by the folder README |
| `papers/2022-delta-omicron-cohort/Code/` | https://github.com/markagreen/social_flip_COVID-19 | `b1f13dbf` (2022-11-28) | Mark Green | MIT, copyright (c) 2022 Mark A Green; licence text kept in the folder | Original README files replaced by the folder README |
| `papers/2022-asymptomatic-testing-admissions/Liverpool_COVID_SMART_Hospitalisation.R` | https://github.com/civicdatacoop/COVID-SMART-in-Liverpool | `357cea13` (2022-06-22) | Xingna Zhang, Ben Barr | **None in source**; authors’ copyright | Lines 57 to 62 (the authors’ local and network drive paths) replaced by environment variables with placeholder defaults, with a comment explaining the change |
| `papers/2023-mobile-vaccination-units/*.R` | https://github.com/civicdatacoop/mobileVaxUnit | `53cadb6c` (2023-08-06) | Xingna Zhang | **None in source**; authors’ copyright | None (paths already masked by the author) |
| `papers/2025-tier3-hospitalisations/tier3_hospitalisation_github.R` | https://github.com/civicdatacoop/tier3_covid_hospitalisation | `6563c393` (2024-01-30) | Xingna Zhang | **None in source**; authors’ copyright | None (paths already masked by the author) |
| `SMART_*.pdf`, `SMART_RR_Anonymised_Data.zip` (root) | This repository, 2024 | | SMART Release & Return study team | As described in the paper’s data availability statement | None |

## Outstanding licence confirmations

The three folders from the Liverpool City Region Civic Data Cooperative (`civicdatacoop`) organisation were published without a licence file. Adding an MIT (or other open) licence to those source repositories, or recording the authors’ agreement here, is needed before they can be treated as covered by this repository’s `LICENSE`. Until then they are reproduced as published, for scrutiny and reproduction of the papers, and remain under their authors’ copyright.

## How provenance is recorded

Commits are the short SHA of the source repository’s default branch at the time of copying (9 October 2026). To check a file against its source, clone the source repository at that commit and diff against the folder here.
