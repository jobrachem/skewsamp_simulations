# MAIN SIMULATION FUNCTIONS

#' Runs a power simulation for a single effect size in a single scenario
#'
#' @param nsim numeric, number of simulation iterations
#' @param n numeric, sample size to use in the simulation
#' @param ef numeric, efficacy to use in data simulation
#' @param scenario list, contains further information for the simulation.
#'   See \code{run_scenario} for details.
#'
#' @return A numeric vector of length \code{nsim}, containing the
#'   p-values of the tests.
run_simulation <- function(nsim, n, ef, scenario) {
  p <- vector(mode = "numeric", length = nsim)

  for (i in seq(nsim)) {
    if (i %% 1000 == 0) cat("Iteration", i, "/", nsim, "\n")

    # generate data
    simargs <- c(list(n = n, efficacy = ef), scenario$simargs)
    data <- do.call(scenario$simfun, args = simargs)

    # analyse data
    p[i] <- scenario$analyse(data = data)
  }
  p
}

#' Runs power simulations for a single scenario, using a vector of
#' different effect sizes.
#'
#' @param nsim numeric, number of simulation iterations
#' @param ef numeric, vector of efficacies to use in data simulation
#' @param scenario list of scenario specifications
#'   The elements of a scenario must be (1) \code{nfun}, (2) \code{nargs},
#'   (3) \code{simfun}, (4) \code{simargs}, and (5) \code{analyse}.
#'   \code{nfun} is the function to call for getting an estimate of the
#'   required sample size.
#'   \code{nargs} is a list of arguments to pass to \code{nfun}.
#'   \code{simfun} is a function for simulating data for the power
#'   analysis. Must return a data.frame with an "y" and an "x" column,
#'   where y is the dependent variable and x is an indicator that gives
#'   the group relations.
#'   \code{simargs} is a list of arguments for \code{simfun}
#'   \code{analyse} is a function that is used to analyse the data. The
#'   function receives the generated data and must return the p-value for
#'   the test of group differences in the dependent variable.
#'
#' @return A list of results, containing information about the scenario
#'  and a matrix of p-values in \code{p}. In this matrix, each columns
#'  contains \code{nsim} p-values obtained based on a power simulation
#'  with one element of \code{ef}.
run_scenario <- function(nsim, ef, scenario) {
  nvec <- vector(mode = "numeric", length = length(ef))
  names(nvec) <- paste("ef =", round(ef, 2))

  pmat <- matrix(nrow = nsim, ncol = length(ef))
  colnames(pmat) <- paste("ef =", round(ef, 2))

  for (i in seq_along(ef)) {
    cat("Starting simulation for\t ef =", ef[i], "\n")

    # calculate sample size
    nargs <- c(list(efficacy = ef[i]), scenario$nargs)
    n <- do.call(scenario$nfun, args = nargs)
    n <- n$n # because skewsamp functions return a list
    nvec[i] <- n

    # analyse data
    pmat[,i] <- run_simulation(nsim, n, ef[i], scenario)
    cat("Finished simulation for\t ef =", ef[i], "\n\n")
  }

  results <- list(nsim = nsim, n = nvec, p = pmat, ef = ef,
                  dist = scenario$distribution, linkfun = scenario$linkfun)

  c(results, scenario$simargs)
}
