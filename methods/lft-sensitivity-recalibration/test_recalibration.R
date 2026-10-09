# Tests for recalibrate_lft_sensitivity.R
# Run from this folder:  Rscript test_recalibration.R
# Checks the implementation against Table 1 and the worked example in
# Petersen et al. (2021) Clin Epidemiol 13:935-940.

source("recalibrate_lft_sensitivity.R")

# Table 1 of the paper, transcribed: rows pi = 0.300 to 0.800 by 0.025,
# columns apparent sensitivity 30% to 80% by 5%. NA = dash (not possible).
paper_table <- rbind(
  c(100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),     # 0.300
  c( 92, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),     # 0.325
  c( 86,100, NA, NA, NA, NA, NA, NA, NA, NA, NA),     # 0.350
  c( 80, 93, NA, NA, NA, NA, NA, NA, NA, NA, NA),     # 0.375
  c( 75, 88,100, NA, NA, NA, NA, NA, NA, NA, NA),     # 0.400
  c( 71, 82, 94, NA, NA, NA, NA, NA, NA, NA, NA),     # 0.425
  c( 67, 78, 89,100, NA, NA, NA, NA, NA, NA, NA),     # 0.450
  c( 63, 74, 84, 95, NA, NA, NA, NA, NA, NA, NA),     # 0.475
  c( 60, 70, 80, 90,100, NA, NA, NA, NA, NA, NA),     # 0.500
  c( 57, 67, 76, 86, 95, NA, NA, NA, NA, NA, NA),     # 0.525
  c( 55, 64, 73, 82, 91,100, NA, NA, NA, NA, NA),     # 0.550
  c( 52, 61, 70, 78, 87, 96, NA, NA, NA, NA, NA),     # 0.575
  c( 50, 58, 67, 75, 83, 92,100, NA, NA, NA, NA),     # 0.600
  c( 48, 56, 64, 72, 80, 88, 96, NA, NA, NA, NA),     # 0.625
  c( 46, 54, 62, 69, 77, 85, 92,100, NA, NA, NA),     # 0.650
  c( 44, 52, 59, 67, 74, 81, 89, 96, NA, NA, NA),     # 0.675
  c( 43, 50, 57, 64, 71, 79, 86, 93,100, NA, NA),     # 0.700
  c( 41, 48, 55, 62, 69, 76, 83, 90, 97, NA, NA),     # 0.725
  c( 40, 47, 53, 60, 67, 73, 80, 87, 93,100, NA),     # 0.750
  c( 39, 45, 52, 58, 65, 71, 77, 84, 90, 97, NA),     # 0.775
  c( 38, 44, 50, 56, 63, 69, 75, 81, 88, 94,100)      # 0.800
)

computed <- recalibration_table()
computed_pct <- percent_half_up(computed)   # round half up, as printed

stopifnot(
  all(dim(computed) == dim(paper_table)),
  all(is.na(computed_pct) == is.na(paper_table)),
  all(computed_pct[!is.na(paper_table)] == paper_table[!is.na(paper_table)])
)
cat("Table 1: all", sum(!is.na(paper_table)), "published cells reproduced;",
    sum(is.na(paper_table)), "dash cells returned as NA\n")

# Worked example: Liverpool pilot, 28 LFT positives among 70 PCR positives,
# apparent sensitivity 40.0% (95% CI 28.5% to 52.4%); with pi = 0.5 the
# calibrated absolute sensitivity is 80%.
ex <- recalibrate_lft_sensitivity_ci(28, 70, pi = 0.5)
stopifnot(
  abs(ex[["apparent"]] - 0.400) < 1e-3,
  round(ex[["apparent_lower"]], 3) == 0.285,
  round(ex[["apparent_upper"]], 3) == 0.524,
  abs(ex[["absolute"]] - 0.80) < 1e-9
)
cat("Worked example: 28/70 -> apparent 40.0% (28.5% to 52.4%), absolute 80% at pi = 0.5\n")

# Upper bound: apparent 50% with pi = 0.5 gives 100%; anything above is NA.
stopifnot(
  recalibrate_lft_sensitivity(0.50, 0.5) == 1,
  is.na(recalibrate_lft_sensitivity(0.55, 0.5))
)

# implied_pi inverts the calibration
stopifnot(abs(implied_pi(0.40, 0.80) - 0.5) < 1e-12)

# Input validation
stopifnot(inherits(try(recalibrate_lft_sensitivity(1.2, 0.5), silent = TRUE), "try-error"))
stopifnot(inherits(try(recalibrate_lft_sensitivity(0.4, 0), silent = TRUE), "try-error"))

cat("All tests passed\n")
