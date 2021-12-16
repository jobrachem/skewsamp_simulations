#' Runs the simulation for a single scenario
#'
#' @param dist_fun function object. The function to use for drawing
#'   pilot data from a probability density distribution, for example \code{rnorm}.
#' @param nsim numeric, number of iterations for the simulation
#' @param m numeric, sample size of pilot sample
#' @param delta numeric, size of the location shift delta
#' @param alpha numeric, alpha level to use in sample size estimation
#' @param power numeric, power level to use in sample size estimation
#' @param ... additional arguments that are passed on to \code{dist_fun}
#'
#' @return numeric vector of \code{nsim} sample size estimates
sim_locshift <- function(dist_fun, nsim, m, delta, alpha = 0.05, power = 0.9, ...) {
  results <- vector(mode = "numeric", length = nsim)

  for (i in seq(nsim)) {
    if (i %% 1000 == 0) cat("Iteration", i, "/", nsim, "\n")
    s1 <- dist_fun(m, ...)
    s2 <- dist_fun(m, ...) - delta

    results[i] <- skewsamp::n_locshift(s1, s2, delta, alpha, power)$n / 2
  }

  results
}


#' Runs the simulation for multiple scenarios
#'
#' @param scenarios list of scenarios. A single scenario must contain all
#'   necessary arguments for a call to \code{sim_locshift}.
#'
#' @return list of numeric vectors. Each vector contains \code{nsim}
#'   sample size estimates based on the corresponding scenario.
run_sim_locshift <- function(scenarios) {
  results <- scenarios
  for (i in seq_along(scenarios)) {
    cat("\n------------\nScenario", i, "/", length(scenarios), "\n")
    arguments <- scenarios[[i]]

    results[[i]] <- do.call(sim_locshift, args = arguments)
  }

  results
}


#' Gently curates the results of a call to \code{run_sim_locshift}
#'
#' @param scenarios list of scenarios, used to produce \code{results}.
#' @param results list of results, output of \code{run_sim_locshift}.
#'
#' @return list of scenarios, now including the sample size estimates
#'   but excluding the \code{dist_fun} function objects.
postprocess_sim_results_distributions <- function(scenarios, results) {
  for (i in seq_along(results)) {
    scenarios[[i]]$dist_fun <- NULL
    scenarios[[i]][["n_estimates"]] <- results[[i]]
  }

  scenarios
}


#' Curates the results of a call to \code{run_sim_locshift}, producing a data frame
#'
#' @param scenarios list of scenarios, used to produce \code{results}.
#' @param results list of results, output of \code{run_sim_locshift}.
#'
#' @return A data.frame, summarizing the results.
postprocess_sim_results <- function(scenarios, results) {
  mean_n <- sapply(results, function(x) mean(x))
  q10_n <- sapply(results, quantile, type = 3, probs = 0.1)
  q50_n <- sapply(results, median)
  q90_n <- sapply(results, quantile, type = 3, probs = 0.9)
  sd_n <- sapply(results, sd)

  # Process resulting data to get a nice data frame
  for (i in seq_along(scenarios)) {
    scenarios[[i]][["n_mean"]] <- mean_n[i]
    scenarios[[i]][["n_sd"]] <- sd_n[i]
    scenarios[[i]][["n_q10"]] <- q10_n[i]
    scenarios[[i]][["n_median"]] <- q50_n[i]
    scenarios[[i]][["n_q90"]] <- q90_n[i]
    scenarios[[i]]$dist_fun <- NULL
    scenarios[[i]] <- as.data.frame(scenarios[[i]])
  }

  df <- do.call(rbind, scenarios)

  df$title <- rownames(df)
  rownames(df) <- NULL

  df
}
