interval <- function(p, n, coverage = 0.95) {
  coverage <- coverage + (1 - coverage) / 2
  z <- stats::qnorm(coverage)
  shift <- z * sqrt(p * (1 - p) / n)

  lower <- p - shift
  upper <- p + shift

  cbind("lower" = lower, "upper" = upper)
}

shift <- function(p, n, coverage) {
  coverage <- coverage + (1 - coverage) / 2
  z <- stats::qnorm(coverage)
  shift <- z * sqrt(p * (1 - p) / n)
}

lower <- function(p, n, coverage = 0.95) {

  lower <- p - shift(p, n, coverage)

  lower
}

upper <- function(p, n, coverage = 0.95) {

  upper <- p + shift(p, n, coverage)

  upper
}
