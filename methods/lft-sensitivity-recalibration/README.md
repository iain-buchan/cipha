# Recalibrating lateral flow test sensitivity

Reusable implementation of the formula in:

> Petersen I, Crozier A, Buchan I, Mina M, Bartlett J. Recalibrating SARS-CoV-2 antigen rapid lateral flow test relative sensitivity from validation studies to absolute sensitivity for indicating individuals shedding transmissible virus. *Clin Epidemiol* 2021;13:935-940. https://doi.org/10.2147/CLEP.S311977

Validation studies report the sensitivity of an antigen lateral flow test (LFT) relative to PCR. Because PCR stays positive after people stop shedding antigen, that relative figure understates how well the LFT detects people who are shedding antigen and therefore likely to be infectious. The paper derives the correction:

```
absolute sensitivity = apparent (PCR-relative) sensitivity / pi
```

where `pi` is the proportion of PCR-positive people who are shedding antigen. The paper’s Liverpool example (28 LFT positives among 70 PCR positives, apparent sensitivity 40.0%) gives an absolute sensitivity of 80% when `pi` = 0.5, and higher if `pi` is lower.

## Files

| File | Purpose |
|---|---|
| `recalibrate_lft_sensitivity.R` | `recalibrate_lft_sensitivity()`, `recalibrate_lft_sensitivity_ci()` (exact binomial interval, each limit recalibrated), `implied_pi()`, `recalibration_table()` (Table 1 of the paper). Base R only. |
| `test_recalibration.R` | Reproduces all 121 published cells of Table 1 and the worked example. Run `Rscript test_recalibration.R`. |
| `recalibrate_lft_sensitivity.py` | Python port of the core function and table, standard library only. Run `python3 recalibrate_lft_sensitivity.py` to self-test. |

## Usage

```r
source("recalibrate_lft_sensitivity.R")
recalibrate_lft_sensitivity(0.40, pi = 0.5)        # 0.8
recalibrate_lft_sensitivity_ci(28, 70, pi = 0.5)  # point estimate and 95% CI, apparent and absolute
recalibration_table()                              # Table 1 as a matrix of proportions
```

Combinations where the apparent sensitivity exceeds `pi` are returned as `NA` (a dash in the paper), because the model treats the LFT as never positive in post-infectious, antigen-negative people. For the same reason the upper confidence limit of the Liverpool example (52.4% / 0.5) is returned as `NA`.

## Assumptions

PCR is positive for everyone shedding antigen and for post-infectious RNA shedders, never for the uninfected; the LFT is never positive in post-infectious people; results depend only on infection state; the brief period in which PCR turns positive before the LFT is treated as negligible; sampling error and operator effects are not modelled. The paper does not estimate `pi`; it has to be supplied from viral kinetics and the local epidemic phase.
