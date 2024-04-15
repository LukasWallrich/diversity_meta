

#' Extracts the confidence interval from bootstrapped results
#'
#' This function takes the output from boot.ci and extracts the confidence interval.
#'
#' @param boot_ci The bootstrapped confidence interval (from `boot::boot.ci()``)
#' @return A list containing the lower and upper bounds of the confidence interval


extract_boot_ci <- function(boot_ci) {
  ci <- list()
  boot_ci <- boot_ci[[4]]
  ci["CI_L"] <- boot_ci[length(boot_ci) - 1]
  ci["CI_U"] <- boot_ci[length(boot_ci)]
  ci
}

#' Calculate the p-value for equivalence testing from a bootstrap sample
#'
#' This function calculates the p-value for equivalence testing based on bootstrap resampling.
#' It takes as input a bootstrap sample (bs) and several optional parameters including the desired precision of the p-value (p_precision),
#' the minimum p-value (min_p), the smallest effect size of interest (sesoi), the type of confidence interval (ci_type),
#' and whether the test is one-tailed or two-tailed (one_tailed).
#'
#' @param bs A bootstrap sample (result of `boot::boot()`)
#' @param p_precision The desired precision of the p-value (default is 0.001)
#' @param min_p The minimum p-value (default is 0.0001)
#' @param sesoi The smallest effect size of interest (in absolute terms)
#' @param ci_type The type of confidence interval (default is "bca")
#' @param one_tailed Whether the test is one-tailed or two-tailed (default is FALSE)
#'
#' @return The p-value for equivalence testing


boot_get_equiv_p <- function(bs, sesoi, p_precision = .0001, min_p = .0001, ci_type = c("bca", "perc"), one_tailed = FALSE) {

  cli::cli_progress_bar(
    format = paste0(
      "{pb_spin} Estimating equivalence tests - done {status} ",
      "[{pb_current} steps] in {pb_elapsed}"
    )
  )

  status <- "setting up"
  cli::cli_progress_update(set = 0)

  # Usually, equivalence tests are based on two one-sides tests - but here, R2 can only be positive, so that this does not seem appropriate
  # Therefore, default to two-sided tests

  alternative_multiplier <- if (one_tailed) 2 else 1

  ci_type <- ci_type[1]
  ci <- try(boot.ci(bs, 1 - alternative_multiplier*min_p, type = ci_type))
  status <- "estimating minimum"
  cli::cli_progress_update()
  if (inherits(ci, "try-error")) {
    if (ci_type[1] != "perc") {
      warning("Specified CI-calculation failed, so falling back to 'perc'")
      ci_type = "perc"
      ci <- try(boot.ci(bs, type = ci_type))
    }
    if (inherits(ci, "try-error")) {
      stop("CI calculation failed - check output of `boot.ci()`")
    }
  }

  confidence_interval <- extract_boot_ci(ci)

  # Identify whether we care about lower or upper bound
  direction <- if (bs$t0 < 0) "CI_L" else "CI_U"

  # Test boundary conditions
  # Estimate outside interval
  if (abs(bs$t0) >= sesoi) return(1)


  # p < min_p
  if (abs(confidence_interval[[direction]]) < sesoi) {
    message("Test significant at min_p, so returning 0. Make sure to report as <= ", format(min_p, scientific = FALSE))
    return(0)
  }

  if (one_tailed) {
    # p >= .5
    confidence_interval <- extract_boot_ci(boot.ci(bs, .001, type = ci_type))
    status <- "estimating maximum"
    cli::cli_progress_update()
    if (abs(confidence_interval[[direction]]) > sesoi) {
      warning("Test not significant at *p* = .495. One-sided tests cannot identify two-sided p-value above .5, so returning NA")
      return(NA)
    }

    alpha_high <- .495
  } else {
    alpha_high <- .995
  }
  alpha_low <- min_p

  # Accelerate search by first splitting at .05
  initial_split <- 0.05
  confidence_interval <- extract_boot_ci(boot.ci(bs, 1 - alternative_multiplier * initial_split, type = ci_type))
  status <- glue::glue("estimating p = {initial_split}")

  if (abs(confidence_interval[[direction]]) < sesoi) {
    alpha_high <- initial_split
  } else {
    alpha_low <- initial_split
  }

  while (alpha_high - alpha_low > p_precision) {
    alpha_mid <- (alpha_high + alpha_low) / 2
    confidence_interval <- extract_boot_ci(boot.ci(bs, 1 - alternative_multiplier * alpha_mid, type = ci_type))

    status <- glue::glue("estimating p = {alpha_mid}")
    cli::cli_progress_update()

    if (abs(confidence_interval[[direction]]) < sesoi) {
      alpha_high <- alpha_mid
    } else {
      alpha_low <- alpha_mid
    }
  }

  status = "with the test"
  cli_progress_done()
  return(alpha_high)

}


#' Calculate p-value for equivalence testing
#'
#' This function calculates the p-value for equivalence testing based on the method of inverting confidence intervals.
#' The null hypothesis (H0) is that the absolute coefficient (beta) is greater than or equal to a specified threshold (sesoi) for the given model.
#' The function searches for the smallest p-value for which the confidence interval for the coefficient fits entirely within the range between -/+ sesoi,
#' so that this null-hypothesis can be rejected.
#' NB:
#' - In line with earlier analyses, the function uses confidence intervals based on the robust variance estimator (RVE) for the confidence intervals.
#' - As this is based on one-sided tests, two-sides p-values above .5 cannot be identified and will return NA.
#'
#' @param model The model object.
#' @param coef The coefficient of interest.
#' @param sesoi The threshold for equivalence testing (symmetric over 0).
#' @param p_precision The precision for the p-value calculation.
#' @param min_p The minimum p-value for significance.
#'
#' @return The calculated p-value.
#'
#' @references
#' Campbell, 2023: https://arxiv.org/abs/2004.01757 show that equivalence testing can be implemented by inverting confidence intervals.
#' "We will reject the [...] null hypothesis (H0: βk ≤ ∆k, lower or βk ≥ ∆k, upper), at a α significance level,
#' whenever a (1-2α)% CI for βk fits entirely within (∆k, lower, ∆k, upper).

get_p_value <- function(model, coef, sesoi, p_precision = .001, min_p = .0001) {

  confidence_interval <- conf_int(model, vcov = "CR2", coefs = coef, level = 1 - 2 * min_p)

  # Identify whether we care about lower or upper bound
  direction <- if (confidence_interval$beta < 0) "CI_L" else "CI_U"

  # Test boundary conditions
  # Estimate outside interval
  if (abs(confidence_interval$beta) >= sesoi) return(1)

  # p < min_p
  if (abs(confidence_interval[[direction]]) < sesoi) {
    message("Test significant at min_p, so returning 0. Make sure to report as <= ", format(min_p, scientific = FALSE))
    return(0)
  }

  # p >= .5
  confidence_interval <- conf_int(model, vcov = "CR2", coefs = coef, level = .001)
  if (abs(confidence_interval[[direction]]) > sesoi) {
    warning("Test not significant at *p* = .495. One-sided tests cannot identify two-sided p-value above .5, so returning NA")
    return(NA)
  }

  alpha_high <- .495
  alpha_low <- min_p

  while (alpha_high - alpha_low > p_precision) {
    alpha_mid <- (alpha_high + alpha_low) / 2
    confidence_interval <- conf_int(model, vcov = "CR2", coefs = coef, level = 1 - 2 * alpha_mid)

    if (abs(confidence_interval[[direction]]) < sesoi) {
            alpha_high <- alpha_mid
    } else {
      alpha_low <- alpha_mid
    }
    }

    return(alpha_high)

}


#' Pseudo R-squared
#'
#' This function calculates the pseudo R-squared value (in %, i.e. ranging from 0-100), which represents the proportion of the variance in effect sizes
#' (across both study and sample level) that is explained by (a) moderator(s).
#'
#' @param mod0 The rma.mv model with fewer moderators.
#' @param mod1 The rma.mv model with more moderators.
#'
#' @return The pseudo R-squared value.
#' @references Wolfgang Viechtbauer
# https://www.metafor-project.org/doku.php/tips:ci_for_r2
# https://gist.github.com/wviechtb/6fbfca40483cb9744384ab4572639169

pseudo_R2 <- function(mod0, mod1) {
  max(0, 100 * (sum(mod0$sigma2) - sum(mod1$sigma2)) / sum(mod0$sigma2))
}
