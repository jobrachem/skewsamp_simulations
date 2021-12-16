#' Helper function, group comparison via GLM
#'
#' @param family family object
#'
#' @return A function that takes in a data frame and fits a GLM of the
#'   specified family, returning the p-value for the group comparison
get_glm <- function(family) {
  analyse <- function(data) {
    m <- glm(y ~ x, data = data, family = family)
    m.sum <- summary(m)
    p <- m.sum$coefficients[2,4]

    p
  }

  analyse
}


# GAMMA SPECIFIC

#' Helper function, simulates a data frame using the gamma distribution
#'
#' @param n numeric, number of observations to generate
#' @param mean0 numeric, mean in the control group
#' @param shape numeric, shape parameter of the gamma distribution
#' @param efficacy numeric, effect size of group difference (1 - mean1/mean0)
#' @param q proportion of observations in control group
#'
#' @return data.frame with the columns "y" (dependent observations) and
#'   "x" (a vector of 0 and 1, indication group membership, 0 means
#'   control group).
sim_gamma <- function(n, mean0, shape, efficacy, q = 0.5) {
  n0 <- ceiling(n * q)
  n1 <- ceiling(n * (1 - q))

  mean1 <- mean0 * (1 - efficacy)

  y1 <- rgamma(n0, shape, scale = mean0 / shape)
  y2 <- rgamma(n1, shape, scale = mean1 / shape)

  y <- c(y1, y2)
  x <- c(rep(0, n0), rep(1, n1))

  data.frame(y = y, x = x)
}

#' Helper function for generating a gamma scenario
#'
#' @param mean0 numeric, mean in the control group
#' @param shape numeric, shape parameter of the gamma distribution
#' @param linkfun string, indicates which link function to use for sample size
#'   estimation
#' @param alpha numeric, alpha level to use
#' @param power numeric, power level to use
#'
#' @return list, a scenario that can be used in \code{run_scenario}
gamma_scenario <- function(
  mean0, shape,
  linkfun = c("log", "identity"),
  alpha = 0.05, power = 0.9
) {

  linkfun <- match.arg(linkfun)

  nargs <- list(mean0 = mean0, shape0 = shape, alpha = alpha, power = power, link = linkfun)

  scen <- list(
    nfun = skewsamp::n_gamma,
    nargs = nargs,

    simfun = sim_gamma,
    simargs = list(mean0 = mean0, shape = shape),

    analyse = get_glm(Gamma(link = "log")),

    linkfun = linkfun,
    distribution = "gamma"
  )

  scen
}

# NEGBINOM SPECIFIC

#' Helper function, group comparison via GLM for negative binomal distribution
#'
#' @param data data.frame, must contain a "y" and an "x" column
#'
#' @return numeric, p-value of the group comparison
glm.nb <- function(data) {
  m <- MASS::glm.nb(y ~ x, data = data, link = log)
  m.sum <- summary(m)
  p <- m.sum$coefficients[2, 4]
  p
}


#' Helper function, simulates a data frame using the negative binomial distribution
#'
#' @param n numeric, number of observations to generate
#' @param mean0 numeric, mean in the control group
#' @param dispersion numeric, dispersion parameter for the negative binomial distribution
#' @param efficacy numeric, effect size of group difference (1 - mean1/mean0)
#' @param q proportion of observations in control group
#'
#' @return data.frame with the columns "y" (dependent observations) and
#'   "x" (a vector of 0 and 1, indication group membership, 0 means
#'   control group).
sim_negbinom <- function(n, mean0, dispersion, efficacy, q = 0.5) {
  n0 <- ceiling(n * q)
  n1 <- ceiling(n * (1 - q))

  mean1 <- mean0 * (1 - efficacy)

  y1 <- rnbinom(n0, size = dispersion, mu = mean0)
  y2 <- rnbinom(n1, size = dispersion, mu = mean1)

  y <- c(y1, y2)
  x <- c(rep(0, n0), rep(1, n1))

  data.frame(y = y, x = x)
}

#' Helper function for generating a negative binomial scenario
#'
#' @param mean0 numeric, mean in the control group
#' @param dispersion numeric, dispersion parameter for the negative binomial distribution
#' @param linkfun string, indicates which link function to use for sample size
#'   estimation
#' @param alpha numeric, alpha level to use
#' @param power numeric, power level to use
#'
#' @return list, a scenario that can be used in \code{run_scenario}
negbinom_scenario <- function(
  mean0, dispersion,
  linkfun = c("log", "identity"),
  alpha = 0.05, power = 0.9
) {

  linkfun <- match.arg(linkfun)

  nargs <- list(mean0 = mean0, dispersion0 = dispersion, alpha = alpha, power = power, link = linkfun)

  scen <- list(
    nfun = skewsamp::n_negbinom,
    nargs = nargs,

    simfun = sim_negbinom,
    simargs = list(mean0 = mean0, dispersion = dispersion),

    analyse = glm.nb,

    linkfun = linkfun,
    distribution = "negbinom"
  )

  scen
}

#' Helper function, simulates a data frame using the negative binomial distribution
#'
#' @param n numeric, number of observations to generate
#' @param mean0 numeric, mean in the control group
#' @param efficacy numeric, effect size of group difference (1 - mean1/mean0)
#' @param q proportion of observations in control group
#'
#' @return data.frame with the columns "y" (dependent observations) and
#'   "x" (a vector of 0 and 1, indication group membership, 0 means
#'   control group).
sim_poisson <- function(n, mean0, efficacy, q = 0.5) {
  n0 <- ceiling(n * q)
  n1 <- ceiling(n * (1 - q))

  mean1 <- mean0 * (1 - efficacy)

  y1 <- rpois(n0, lambda = mean0)
  y2 <- rpois(n1, lambda = mean1)

  y <- c(y1, y2)
  x <- c(rep(0, n0), rep(1, n1))

  data.frame(y = y, x = x)
}


#' Helper function for generating a negative binomial scenario
#'
#' @param mean0 numeric, mean in the control group
#' @param linkfun string, indicates which link function to use for sample size
#'   estimation
#' @param alpha numeric, alpha level to use
#' @param power numeric, power level to use
#'
#' @return list, a scenario that can be used in \code{run_scenario}
poisson_scenario <- function(
  mean0,
  linkfun = c("log", "identity"),
  alpha = 0.05, power = 0.9
) {

  linkfun <- match.arg(linkfun)

  nargs <- list(mean0 = mean0, alpha = alpha, power = power, link = linkfun)

  scen <- list(
    nfun = skewsamp::n_poisson,
    nargs = nargs,

    simfun = sim_poisson,
    simargs = list(mean0 = mean0),

    analyse = get_glm(poisson(link = "log")),

    linkfun = linkfun,
    distribution = "poisson"
  )

  scen
}

#' Helper function, simulates a data frame using the negative binomial distribution
#'
#' @param n numeric, number of observations to generate
#' @param p probability of success in control group
#' @param size number of trials (greater than zero)
#' @param efficacy numeric, effect size of group difference (1 - mean1/mean0)
#' @param q proportion of observations in control group
#'
#' @return data.frame with the columns "y" (dependent observations) and
#'   "x" (a vector of 0 and 1, indication group membership, 0 means
#'   control group).
sim_binom <- function(n, p0, size, efficacy, q = 0.5) {
  n0 <- ceiling(n * q)
  n1 <- ceiling(n * (1 - q))

  odds1 <- p0 / (1 - p0) / (1 - efficacy)
  p1 <- odds1 / (1 + odds1)

  y1 <- rbinom(n0, size = size, prob = p0)
  y2 <- rbinom(n1, size = size, prob = p1)

  y <- c(y1, y2)
  x <- c(rep(0, n0), rep(1, n1))

  data.frame(y = y, x = x)
}


#' Helper function for generating a binomial scenario
#'
#' @param p probability of success in control group
#' @param size number of trials (greater than zero)
#' @param linkfun string, indicates which link function to use for sample size
#'   estimation
#' @param alpha numeric, alpha level to use
#' @param power numeric, power level to use
#'
#' @return list, a scenario that can be used in \code{run_scenario}
binom_scenario <- function(
  p0,
  size,
  linkfun = c("logit", "identity"),
  alpha = 0.05, power = 0.9
) {

  linkfun <- match.arg(linkfun)

  nargs <- list(p0 = p0, size = size, alpha = alpha, power = power, link = linkfun)

  scen <- list(
    nfun = skewsamp::n_binom,
    nargs = nargs,

    simfun = sim_binom,
    simargs = list(p0 = p0, size = size),

    analyse = get_glm(binomial(link = "logit")),

    linkfun = linkfun,
    distribution = "binomial"
  )

  scen
}
