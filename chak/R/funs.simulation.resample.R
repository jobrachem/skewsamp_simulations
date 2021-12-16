#' Runs the resampling simulation for a single scenario
#'
#' @param dist_fun function object. The function to use for drawing
#'   pilot data from a probability density distribution, for example \code{rnorm}.
#' @param nsim numeric, number of iterations for the simulation
#' @param m numeric, sample size of pilot sample
#' @param n_resamples numeric, number of new pilot samples to draw from
#'   the empirical distribution function created on the basis of the
#'   pilot samples.
#' @param delta numeric, size of the location shift delta
#' @param alpha numeric, alpha level to use in sample size estimation
#' @param power numeric, power level to use in sample size estimation
#' @param ... additional arguments that are passed on to \code{dist_fun}
#'
#' @return list of length \code{nsim}. Each entry in the list is a numeric
#'   vector of length \code{n_resamples}, holding the sample size estimates
#'   obtained from individual resampling steps.
sim_resampling <- function(dist_fun, nsim, m, n_resamples, delta, alpha = 0.05, power = 0.9, ...) {

  results <- vector(mode = "list", length = nsim)

  for (i in seq(nsim)) {
    if (i %% 10 == 0) cat("Iteration", i, "/", nsim, "\n")
    s1 <- dist_fun(m, ...)
    s2 <- dist_fun(m, ...) - delta

    ndist <- skewsamp::resample_n_locshift(s1, s2,
                                           n_resamples = n_resamples,
                                           delta = delta, power = power, alpha = alpha)

    results[[i]] <- ndist
  }

  cat("\n")
  results
}

#' Runs the resampling simulation for a list of scenarios
#'
#' @param nsim numeric, number of simulations per scenario
#' @param scenarios list oft scenarios. Each entry must contain a
#'   \code{dist_fun}, an \code{m}, and a \code{delta} parameter. These
#'   are explained in \code{sim_resampling}.
#' @param n_resamples numeric, number of new pilot samples to draw from
#'   the empirical distribution function created on the basis of the
#'   pilot samples.
#' @param alpha numeric, alpha level to use in sample size estimation
#' @param power numeric, power level to use in sample size estimation
#'
#' @return list of lists - each scenario is an entry on the top level
#'   and contains a list of numeric vectors, which hold the sample
#'   size estimates from individual resampling steps.
run_sim <- function(nsim, scenarios, n_resamples, alpha = 0.05, power = 0.9) {
  results <- scenarios

  for (i in seq_along(scenarios)) {
    cat("\n------------\nScenario", i, "/", length(scenarios), "\n")
    scen <- scenarios[[i]]
    fn <- scen$dist_fun
    m <- scen$m
    delta <- scen$delta

    results[[i]] <- sim_resampling(
      dist_fun = fn,
      nsim = nsim,
      m = m,
      n_resamples = n_resamples,
      delta = delta,
      alpha = alpha,
      power = power
    )
  }

  results
}

