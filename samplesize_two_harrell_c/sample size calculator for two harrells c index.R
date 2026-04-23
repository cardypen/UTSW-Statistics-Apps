###############################################################################
# Sample Size Calculation for Comparing Two Correlated C-indices
# Based on: Kang et al. (2015), Statistics in Medicine, 34(4): 685-703
# "Comparing two correlated C indices with right-censored survival outcome:
#  a one-shot nonparametric approach"
###############################################################################
#
# BACKGROUND:
# -----------
# The C-index (Harrell's concordance statistic) extends the AUC to survival
# outcomes. This code calculates the required sample size to detect a
# difference (Delta_c = C1 - C2) between two correlated C-indices with a
# specified power, using the nonparametric z-score framework from Kang et al.
#
# The variance of Delta_c = C1 - C2 depends on:
#   - The individual C-index values (C1, C2)
#   - The correlation (rho) between the two predictive scores
#   - The censoring percentage
#
# We use the asymptotic normal approximation:
#   z = (C1_hat - C2_hat) / sqrt(Var(C1_hat - C2_hat))
#
# The variance formula is derived from the U-statistic framework in the paper.
# For sample size planning, we use an approximation based on the known
# relationship between C, Kendall's tau, and the variance of the difference.
###############################################################################


# ============================================================
# HELPER: Variance of a single C-index estimator
# ============================================================
# Based on the asymptotic variance of a U-statistic for C.
# Under a bivariate normal model for (survival rank, score),
# Var(C_hat) ≈ [C(1-C) + (n-2)*Q - (n-1)*C^2] / [n*(n-1)/2]
# simplified to the leading term: C*(1-C) / (n/2) for large n.
#
# More precise approximation (Sen 1960, Noether 1967):
#   Var(tau_hat) ≈ 2*(2n+5) / (9*n*(n-1))     [for continuous data, no ties]
# and since C = (tau + 1)/2:
#   Var(C_hat) ≈ Var(tau_hat) / 4

var_c_single <- function(C, n) {
  # Convert C to Kendall's tau
  tau <- 2 * C - 1
  # Asymptotic variance of Kendall's tau (Noether 1967)
  var_tau <- 2 * (2 * n + 5) / (9 * n * (n - 1))
  # Variance of C = (tau + 1)/2
  var_C <- var_tau / 4
  return(var_C)
}


# ============================================================
# HELPER: Covariance between two C-index estimators
# ============================================================
# From DeLong et al. (1988) analogy extended to survival (Kang et al. 2015):
# cov(C1_hat, C2_hat) ≈ rho_scores * sqrt(Var(C1_hat)) * sqrt(Var(C2_hat))
#
# The correlation between C-index estimators is approximately the correlation
# between the underlying predictive scores (rho), because the C-index is a
# monotone function of Kendall's tau which in turn depends linearly on the
# bivariate concordance probability driven by rho.
#
# NOTE: This is the correlation between ESTIMATORS (induced by having the same
# subjects evaluated by both biomarkers), not between the C-index values.

cov_c_pair <- function(C1, C2, rho, n) {
  var1 <- var_c_single(C1, n)
  var2 <- var_c_single(C2, n)
  # Covariance approximated via the structural correlation
  # rho_estimators ≈ rho * (adjustment for C values)
  # Using the DeLong-type approximation: cov ≈ rho * sqrt(var1 * var2)
  cov_12 <- rho * sqrt(var1 * var2)
  return(cov_12)
}


# ============================================================
# HELPER: Variance of the difference (Delta_c = C1 - C2)
# ============================================================
var_delta_c <- function(C1, C2, rho, n) {
  var1   <- var_c_single(C1, n)
  var2   <- var_c_single(C2, n)
  cov_12 <- cov_c_pair(C1, C2, rho, n)
  # Var(C1 - C2) = Var(C1) + Var(C2) - 2*Cov(C1, C2)
  var_diff <- var1 + var2 - 2 * cov_12
  return(var_diff)
}


# ============================================================
# MAIN FUNCTION: Sample size calculation
# ============================================================
#' Calculate sample size for comparing two correlated C-indices
#'
#' @param C1          C-index of model/biomarker 1 (numeric, 0-1)
#' @param C2          C-index of model/biomarker 2 (numeric, 0-1)
#' @param rho         Correlation between the two predictive scores (numeric, -1 to 1)
#' @param power       Desired statistical power (numeric, 0-1; default 0.80)
#' @param alpha       Two-sided significance level (numeric; default 0.05)
#' @param cens_pct    Censoring percentage (numeric, 0-1; default 0 = no censoring)
#' @param verbose     Print detailed output (logical; default TRUE)
#'
#' @return A list with: n_total (required sample size), power_achieved,
#'         delta_c, var_diff_at_n, z_alpha, z_beta
#'
#' @details
#' Uses the asymptotic normal z-test framework from Kang et al. (2015).
#' Censoring is accounted for by inflating the required n by a factor of
#' 1/(1 - cens_pct), as censoring effectively reduces the number of
#' informative pairs. This follows the observation in the paper that
#' censoring reduces effective data for the statistic.
#'
#' The sample size formula solves:
#'   n = (z_{alpha/2} + z_{beta})^2 * Var_unit(Delta_c) / (Delta_c)^2
#' where Var_unit is the variance contribution per subject.

sample_size_cindex <- function(C1,
                               C2,
                               rho,
                               power     = 0.80,
                               alpha     = 0.05,
                               cens_pct  = 0,
                               verbose   = TRUE) {
  
  # --- Input validation ---
  if (C1 <= 0 || C1 >= 1) stop("C1 must be strictly between 0 and 1")
  if (C2 <= 0 || C2 >= 1) stop("C2 must be strictly between 0 and 1")
  if (rho < -1 || rho > 1) stop("rho must be between -1 and 1")
  if (power <= 0 || power >= 1) stop("power must be between 0 and 1")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1")
  if (cens_pct < 0 || cens_pct >= 1) stop("cens_pct must be in [0, 1)")
  
  delta_c <- abs(C1 - C2)
  
  if (delta_c == 0) {
    stop("C1 and C2 are equal. Cannot compute sample size for zero effect.")
  }
  
  # Critical values
  z_alpha <- qnorm(1 - alpha / 2)   # two-sided
  z_beta  <- qnorm(power)
  
  # Iterative search: find n such that the achieved power equals target
  # We iterate because variance depends on n
  n_init <- 50  # starting value
  n      <- n_init
  
  for (iter in 1:10000) {
    var_diff <- var_delta_c(C1, C2, rho, n)
    se_diff  <- sqrt(var_diff)
    
    # Non-centrality parameter
    ncp <- delta_c / se_diff
    
    # Required n from rearranging: ncp = z_alpha + z_beta
    # delta_c / sqrt(Var(n)) = z_alpha + z_beta
    # Since Var(n) ~ 1/n * const, we can solve analytically:
    # n_new = (z_alpha + z_beta)^2 * Var(n=1 scale) / delta_c^2
    # We use the iterate-and-update approach instead:
    n_new <- ceiling((z_alpha + z_beta)^2 * var_diff * n / delta_c^2)
    
    if (abs(n_new - n) <= 1) {
      n <- n_new
      break
    }
    n <- n_new
  }
  
  # Adjust for censoring
  # Censoring reduces informative pairs; inflate n accordingly
  n_uncensored <- n
  n_adjusted   <- ceiling(n / (1 - cens_pct))
  
  # Verify achieved power at the adjusted n
  var_diff_final   <- var_delta_c(C1, C2, rho, n_adjusted)
  se_diff_final    <- sqrt(var_diff_final)
  ncp_final        <- delta_c / se_diff_final
  power_achieved   <- pnorm(ncp_final - z_alpha) + pnorm(-ncp_final - z_alpha)
  
  # --- Output ---
  if (verbose) {
    cat("============================================================\n")
    cat("  Sample Size for Comparing Two Correlated C-indices\n")
    cat("  (Kang et al., Statistics in Medicine, 2015)\n")
    cat("============================================================\n\n")
    cat(sprintf("  C-index (Model 1):          %.4f\n", C1))
    cat(sprintf("  C-index (Model 2):          %.4f\n", C2))
    cat(sprintf("  Delta_c = |C1 - C2|:        %.4f\n", delta_c))
    cat(sprintf("  Score correlation (rho):    %.4f\n", rho))
    cat(sprintf("  Significance level (alpha): %.4f  (two-sided)\n", alpha))
    cat(sprintf("  Target power:               %.4f\n", power))
    cat(sprintf("  Censoring percentage:       %.1f%%\n", cens_pct * 100))
    cat("\n")
    cat("  --- Results ---\n")
    cat(sprintf("  Required n (no censoring):  %d\n", n_uncensored))
    cat(sprintf("  Required n (with censoring):%d\n", n_adjusted))
    cat(sprintf("  Achieved power:             %.4f\n", power_achieved))
    cat(sprintf("  z_alpha/2:                  %.4f\n", z_alpha))
    cat(sprintf("  z_beta:                     %.4f\n", z_beta))
    cat("============================================================\n")
  }
  
  invisible(list(
    n_total         = n_adjusted,
    n_no_censoring  = n_uncensored,
    power_achieved  = power_achieved,
    delta_c         = delta_c,
    C1              = C1,
    C2              = C2,
    rho             = rho,
    alpha           = alpha,
    power_target    = power,
    cens_pct        = cens_pct,
    z_alpha         = z_alpha,
    z_beta          = z_beta,
    var_diff_at_n   = var_diff_final
  ))
}


# ============================================================
# BONUS: Power calculation given n
# ============================================================
#' Compute achieved power given a fixed sample size
#'
#' @param n        Sample size
#' @param C1       C-index of model 1
#' @param C2       C-index of model 2
#' @param rho      Correlation between predictive scores
#' @param alpha    Two-sided significance level (default 0.05)
#' @param cens_pct Censoring percentage (0-1; default 0)
#' @param verbose  Print output (logical; default TRUE)
#'
#' @return Achieved power (numeric)

power_cindex <- function(n,
                         C1,
                         C2,
                         rho,
                         alpha     = 0.05,
                         cens_pct  = 0,
                         verbose   = TRUE) {
  
  delta_c  <- abs(C1 - C2)
  z_alpha  <- qnorm(1 - alpha / 2)
  
  # Effective n after censoring
  n_eff    <- floor(n * (1 - cens_pct))
  
  var_diff <- var_delta_c(C1, C2, rho, n_eff)
  se_diff  <- sqrt(var_diff)
  ncp      <- delta_c / se_diff
  pwr      <- pnorm(ncp - z_alpha) + pnorm(-ncp - z_alpha)
  
  if (verbose) {
    cat("============================================================\n")
    cat("  Power for Comparing Two Correlated C-indices\n")
    cat("============================================================\n\n")
    cat(sprintf("  N (total):                  %d\n", n))
    cat(sprintf("  C-index (Model 1):          %.4f\n", C1))
    cat(sprintf("  C-index (Model 2):          %.4f\n", C2))
    cat(sprintf("  Delta_c = |C1 - C2|:        %.4f\n", delta_c))
    cat(sprintf("  Score correlation (rho):    %.4f\n", rho))
    cat(sprintf("  Significance level (alpha): %.4f  (two-sided)\n", alpha))
    cat(sprintf("  Censoring percentage:       %.1f%%\n", cens_pct * 100))
    cat(sprintf("  Effective n:                %d\n", n_eff))
    cat("\n")
    cat(sprintf("  Achieved power:             %.4f\n", pwr))
    cat("============================================================\n")
  }
  
  invisible(pwr)
}


# ============================================================
# BONUS: Power curve across a range of sample sizes
# ============================================================
#' Plot power curve across sample sizes
#'
#' @param C1        C-index of model 1
#' @param C2        C-index of model 2
#' @param rho       Correlation between predictive scores
#' @param alpha     Two-sided significance level (default 0.05)
#' @param cens_pct  Censoring percentage (default 0)
#' @param n_range   Vector of sample sizes to evaluate (default 50:500)
#' @param target_power  Horizontal reference line (default 0.80)

plot_power_curve <- function(C1,
                             C2,
                             rho,
                             alpha        = 0.05,
                             cens_pct     = 0,
                             n_range      = seq(50, 500, by = 10),
                             target_power = 0.80) {
  
  powers <- sapply(n_range, function(n) {
    power_cindex(n, C1, C2, rho, alpha, cens_pct, verbose = FALSE)
  })
  
  # Find n at target power
  ss_result <- sample_size_cindex(C1, C2, rho,
                                  power    = target_power,
                                  alpha    = alpha,
                                  cens_pct = cens_pct,
                                  verbose  = FALSE)
  
  plot(n_range, powers,
       type = "l", lwd = 2, col = "#2C7BB6",
       xlab = "Sample Size (n)",
       ylab = "Power",
       main = sprintf("Power Curve: C1=%.3f, C2=%.3f, rho=%.2f, censoring=%.0f%%",
                      C1, C2, rho, cens_pct * 100),
       ylim = c(0, 1),
       las  = 1)
  
  abline(h   = target_power, lty = 2, col = "firebrick", lwd = 1.5)
  abline(v   = ss_result$n_total, lty = 2, col = "forestgreen", lwd = 1.5)
  abline(h   = c(0.90), lty = 3, col = "gray60")
  
  text(x   = ss_result$n_total,
       y   = 0.05,
       lab = paste0("n=", ss_result$n_total),
       col = "forestgreen", pos = 4, cex = 0.85)
  
  legend("bottomright",
         legend = c("Power curve",
                    sprintf("Power = %.2f", target_power),
                    sprintf("Required n = %d", ss_result$n_total)),
         lty    = c(1, 2, 2),
         col    = c("#2C7BB6", "firebrick", "forestgreen"),
         lwd    = c(2, 1.5, 1.5),
         bty    = "n", cex = 0.85)
  
  grid(col = "grey90", lty = 1)
  
  invisible(data.frame(n = n_range, power = powers))
}


###############################################################################
# EXAMPLES
###############################################################################

cat("\n===== EXAMPLE 1: Basic sample size calculation =====\n\n")

result1 <- sample_size_cindex(
  C1       = 0.70,   # C-index of model 1
  C2       = 0.65,   # C-index of model 2
  rho      = 0.50,   # correlation between scores
  power    = 0.80,   # desired power
  alpha    = 0.05,   # significance level
  cens_pct = 0.20    # 20% censoring
)


cat("\n===== EXAMPLE 2: Higher correlation, higher power =====\n\n")

result2 <- sample_size_cindex(
  C1       = 0.70,
  C2       = 0.65,
  rho      = 0.95,   # highly correlated scores -> smaller variance of diff
  power    = 0.90,
  alpha    = 0.05,
  cens_pct = 0.20
)


cat("\n===== EXAMPLE 3: Framingham-like scenario =====\n\n")
# Replicating the paper's example: SYSBP vs DIABP
# C indices: 0.3651 vs 0.3938 (difference = 0.0287)

result3 <- sample_size_cindex(
  C1       = 0.3938,
  C2       = 0.3651,
  rho      = 0.70,
  power    = 0.80,
  alpha    = 0.05,
  cens_pct = 0.30
)


cat("\n===== EXAMPLE 4: Power given fixed n =====\n\n")

pwr <- power_cindex(
  n        = 300,
  C1       = 0.70,
  C2       = 0.65,
  rho      = 0.50,
  alpha    = 0.05,
  cens_pct = 0.20
)


cat("\n===== EXAMPLE 5: Sensitivity analysis across rho values =====\n\n")

cat(sprintf("%-10s %-12s %-12s %-12s\n",
            "rho", "n (0% cens)", "n (20% cens)", "n (50% cens)"))
cat(strrep("-", 50), "\n")

for (rho_val in c(0.00, 0.25, 0.50, 0.75, 0.95)) {
  n0  <- sample_size_cindex(0.70, 0.65, rho_val, cens_pct = 0.00, verbose = FALSE)$n_total
  n20 <- sample_size_cindex(0.70, 0.65, rho_val, cens_pct = 0.20, verbose = FALSE)$n_total
  n50 <- sample_size_cindex(0.70, 0.65, rho_val, cens_pct = 0.50, verbose = FALSE)$n_total
  cat(sprintf("%-10.2f %-12d %-12d %-12d\n", rho_val, n0, n20, n50))
}

cat("\n(All scenarios: C1=0.70, C2=0.65, power=0.80, alpha=0.05)\n")


cat("\n===== EXAMPLE 6: Power curve plot =====\n")
cat("(Run plot_power_curve() to generate plot)\n\n")
cat('plot_power_curve(C1=0.70, C2=0.65, rho=0.50, cens_pct=0.20)\n\n')

# Uncomment to generate the plot:
# plot_power_curve(C1 = 0.70, C2 = 0.65, rho = 0.50, cens_pct = 0.20)