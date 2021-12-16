# DATA CURATION

#' Helper function, computes the power for a single scenario's result
#'
#' @param result list, output of \code{run_scenario}
#' @param alpha numeric, alpha level to use
#'
#' @return numeric vector of power values. Each element is the share of
#'   significant results corresponding to one effect size.
compute_power <- function(result, alpha = 0.05) {
  apply(result$p, 2, function(x) sum(x <= alpha) / result$nsim)
}

#' Helper function, curates results to return a nice data frame
#'
#' @param result list, output of \code{run_scenario}
#' @param alpha numeric, alpha level to use
#'
#' @return data.frame of results
curate <- function(result, alpha = 0.05) {
  result$power <- compute_power(result, alpha)
  result$p <- NULL

  as.data.frame(result, row.names = NULL)
}


