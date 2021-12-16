source("interval.R")

#' Runs a power simulation
#'
#' @param dist_fun function object. The function to use for drawing
#'   pilot data from a probability density distribution, for example \code{rnorm}.
#' @param n numeric, size of a single group
#' @param delta numeric, size of the location shift delta
#' @param nsim numeric, number of iterations for the simulation
#' @param alpha numeric, alpha level to use in sample size estimation
#' @param ... additional arguments that are passed on to \code{dist_fun}
#'
#' @return numeric, the share of significant test results
pwr_sim <- function(dist_fun, n, delta, nsim, alpha = 0.05, ...) {
  results <- vector(mode = "logical", length = nsim)

  n <- ceiling(n)

  for (i in seq(nsim)) {
    if (i %% 1000 == 0) cat("Power sim i =", i, "/", nsim, "\n")
    s1 <- dist_fun(n, ...)
    s2 <- dist_fun(n, ...) - delta
    test <- wilcox.test(x = s1, y = s2, alternative = "greater")
    results[i] <- test$p.value <= alpha
  }

  sum(results) / nsim
}

#' Runs power simulations for a list of scenarios.
#'
#' @param scenarios list of scenarios. A single scenario must contain all
#'   necessary arguments for a call to \code{pwr_sim}.
#'
#' @return list of numeric values. For each scenario, the list contains
#'   the empirically estimated share of significant test results.
pwr_sim_scenarios <- function(scenarios) {
  results <- scenarios

  for (i in seq_along(scenarios)) {
    cat("\n------------\nScenario", i, "/", length(scenarios), "\n")
    scenarios[[i]]$m <- NULL
    scenarios[[i]]$power <- NULL
    arguments <- scenarios[[i]]
    results[[i]] <- do.call(pwr_sim, args = arguments)
  }

  results
}

#' Curates the results of a call to \code{pwr_sim_scenarios}
#'
#' @param scenarios list of scenarios, used to produce \code{results}.
#' @param results list of results, output of \code{pwr_sim_scenarios}.
#'
#' @return A data.frame, summarizing the results.
postprocess_pwr_results <- function(scenarios, results) {
  # Process resulting data to get a nice data frame

  for (i in seq_along(scenarios)) {
    scenarios[[i]][["emp_pwr"]] <- results[[i]]
    scenarios[[i]]$dist_fun <- NULL
    scenarios[[i]] <- as.data.frame(scenarios[[i]])
  }

  df <- do.call(rbind, scenarios)
  df$title <- rownames(df)
  df$ci_lower <- lower(df$emp_pwr, df$nsim)
  df$ci_upper <- upper(df$emp_pwr, df$nsim)
  rownames(df) <- NULL

  df
}
