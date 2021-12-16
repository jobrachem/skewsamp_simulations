library(tidyverse)
source("cundill/R/funs.scenario.R")
source("cundill/R/funs.simulation.R")
source("cundill/R/funs.curate.R")
source("interval.R")

cat("\n\nRunning additional simulations\n")

scenarios <- list()

shape <- c(0.5, 1, 2, 3, 7.5)
scale <- c(0.5, 1, 2, 3, 4, 5, 6)

for (i in seq_along(shape)) {
  for (j in seq_along(scale)) {
    sh <- shape[i]
    sc <- scale[j]
    m <- sh * sc

    s1 <- gamma_scenario(m, sh)
    s2 <- gamma_scenario(m, sh, linkfun = "identity")

    scen_name1 <- glue::glue("gamma.shape{sh}.scale{sc}.log")
    scen_name2 <- glue::glue("gamma.shape{sh}.scale{sc}.identity")

    scenarios[[scen_name1]] <- s1
    scenarios[[scen_name2]] <- s2
  }
}

ef <- seq(from = 0.3, to = 0.7, length.out = 20)
set.seed(20210702)

data_raw <- map(scenarios, run_scenario, ef = ef, nsim = 10000)
data_curated <- map_dfr(data_raw, curate, .id = "scenario") |> as_tibble()

# compute confidence intervals
data_curated <- data_curated |>
  mutate(ci_lower = lower(power, nsim),
         ci_upper = upper(power, nsim))

saveRDS(data_raw, "cundill/data/additional_raw.RDS")
write_csv(data_curated, "cundill/data/additional_curated.csv")
