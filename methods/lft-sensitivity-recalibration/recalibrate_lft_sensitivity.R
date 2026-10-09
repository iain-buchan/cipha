# Recalibrating SARS-CoV-2 antigen lateral flow test (LFT) sensitivity
# ======================================================================
#
# Implements the formula derived in:
#
#   Petersen I, Crozier A, Buchan I, Mina MJ, Bartlett JW. Recalibrating
#   SARS-CoV-2 antigen rapid lateral flow test relative sensitivity from
#   validation studies to absolute sensitivity for indicating individuals
#   shedding transmissible virus. Clin Epidemiol 2021;13:935-940.
#   https://doi.org/10.2147/CLEP.S311977
#
# Model (notation as in the paper)
#   D = 0  never infected, or previously infected with no detectable viral RNA
#   D = 1  infected and shedding SARS-CoV-2 antigen (likely infectious)
#   D = 2  post-infectious: still PCR positive but no longer shedding antigen
#
#   pi = P(D = 1) / (P(D = 1) + P(D = 2))
#      = proportion of PCR-positive people who are shedding antigen
#
#   Apparent (PCR-relative) sensitivity:  P(LFT+ | PCR+) = pi * P(LFT+ | D = 1)
#   Absolute sensitivity:                 P(LFT+ | D = 1) = P(LFT+ | PCR+) / pi
#
# Assumptions stated in the paper: PCR is positive for D = 1 and D = 2 and
# never for D = 0; the LFT is never positive for D = 2; test results depend
# only on D; the short window in which PCR turns positive before the LFT is
# treated as negligible; sampling error and operator effects are not modelled.
#
# Functions
#   recalibrate_lft_sensitivity(apparent_sensitivity, pi)
#   recalibrate_lft_sensitivity_ci(positives, pcr_positives, pi, conf.level)
#   implied_pi(apparent_sensitivity, absolute_sensitivity)
#   recalibration_table(apparent, pi)   -> reproduces Table 1 of the paper
#
# No package dependencies. See test_recalibration.R for the checks run
# against the published table and worked example.

#' Absolute sensitivity from apparent (PCR-relative) sensitivity
#'
#' @param apparent_sensitivity P(LFT+ | PCR+) as a proportion in [0, 1]
#'   (the sensitivity reported by validation studies that use PCR as the
#'   reference standard). Vectorised.
#' @param pi proportion of PCR-positive individuals who are shedding antigen,
#'   in (0, 1]. Vectorised; recycled against apparent_sensitivity.
#' @return P(LFT+ | D = 1) as a proportion. NA where apparent_sensitivity
#'   exceeds pi, because that combination is not possible under the model
#'   (the paper's Table 1 shows these cells as a dash).
recalibrate_lft_sensitivity <- function(apparent_sensitivity, pi) {
  if (any(apparent_sensitivity < 0 | apparent_sensitivity > 1, na.rm = TRUE))
    stop("apparent_sensitivity must be a proportion between 0 and 1")
  if (any(pi <= 0 | pi > 1, na.rm = TRUE))
    stop("pi must be a proportion greater than 0 and at most 1")
  out <- apparent_sensitivity / pi
  out[apparent_sensitivity > pi + 1e-9] <- NA_real_
  out
}

#' Absolute sensitivity with an exact (Clopper-Pearson) confidence interval
#'
#' Recalibrates a validation-study count, e.g. 28 LFT positives among 70 PCR
#' positives in the Liverpool pilot (apparent sensitivity 40.0%, 95% CI
#' 28.5% to 52.4%), by dividing the point estimate and both interval limits
#' by pi.
#'
#' @param positives number of PCR-positive individuals who were LFT positive
#' @param pcr_positives number of PCR-positive individuals tested with the LFT
#' @param pi proportion of PCR-positive individuals shedding antigen
#' @param conf.level confidence level for the binomial interval (default 0.95)
#' @return a named numeric vector: apparent, apparent_lower, apparent_upper,
#'   absolute, absolute_lower, absolute_upper (all proportions; NA where the
#'   value exceeds 1 and is therefore not possible under the model)
recalibrate_lft_sensitivity_ci <- function(positives, pcr_positives, pi,
                                           conf.level = 0.95) {
  bt <- stats::binom.test(positives, pcr_positives, conf.level = conf.level)
  apparent <- c(unname(bt$estimate), bt$conf.int[1], bt$conf.int[2])
  absolute <- recalibrate_lft_sensitivity(apparent, pi)
  c(apparent = apparent[1], apparent_lower = apparent[2],
    apparent_upper = apparent[3],
    absolute = absolute[1], absolute_lower = absolute[2],
    absolute_upper = absolute[3])
}

#' The value of pi implied by a pair of apparent and absolute sensitivities
#'
#' Useful for reading Table 1 the other way round: given a validation-study
#' sensitivity and an assumed true sensitivity for antigen shedders, what
#' proportion of PCR positives must have been shedding antigen?
implied_pi <- function(apparent_sensitivity, absolute_sensitivity) {
  if (any(absolute_sensitivity <= 0 | absolute_sensitivity > 1, na.rm = TRUE))
    stop("absolute_sensitivity must be a proportion greater than 0 and at most 1")
  apparent_sensitivity / absolute_sensitivity
}

#' Reproduce Table 1 of Petersen et al. (2021)
#'
#' Rows are values of pi, columns are apparent relative sensitivities; cells
#' are the calibrated absolute sensitivity as a proportion (NA = not possible).
#' Multiply by 100 and round half up to recover the printed percentages.
recalibration_table <- function(apparent = seq(0.30, 0.80, by = 0.05),
                                pi = seq(0.300, 0.800, by = 0.025)) {
  m <- outer(pi, apparent,
             function(p, a) recalibrate_lft_sensitivity(apparent_sensitivity = a, pi = p))
  dimnames(m) <- list(pi = formatC(pi, format = "f", digits = 3),
                      apparent = paste0(round(apparent * 100), "%"))
  m
}

#' Whole-number percentages, rounded half up as in the paper's Table 1
#'
#' Several cells fall exactly on a .5 boundary (for example 0.35 / 0.40 =
#' 0.875), where binary floating point would otherwise round the wrong way,
#' so the value is first rounded to 8 decimal places.
percent_half_up <- function(x) floor(round(x * 100, 8) + 0.5)

if (sys.nframe() == 0L) {
  # Worked example from the paper: Liverpool pilot, 28/70, pi assumed 0.5
  print(recalibrate_lft_sensitivity_ci(28, 70, pi = 0.5))
  # Printed form of Table 1
  tab <- recalibration_table()
  print(noquote(ifelse(is.na(tab), "-", paste0(percent_half_up(tab), "%"))))
}
