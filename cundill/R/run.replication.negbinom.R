library(tidyverse)
source("cundill/R/funs.scenario.R")
source("cundill/R/funs.simulation.R")
source("cundill/R/funs.curate.R")
source("interval.R")

cat("\n\nRunning replication of varying dispersion in negbinom dist.\n")

scenarios <- list()

k <- seq(from = 0.1, to = 20, length.out = 20)

for (i in seq_along(k)) {

  s1 <- negbinom_scenario(71.4, k[i])
  s2 <- negbinom_scenario(71.4, k[i], linkfun = "identity")

  s1_name <- glue::glue("negbinom.k{i}.log")
  s2_name <- glue::glue("negbinom.k{i}.identity")

  scenarios[[s1_name]] <- s1
  scenarios[[s2_name]] <- s2
}


ef <- 1 - (50 / 71.4)
set.seed(20210702)

data_raw <- map(scenarios, run_scenario, ef = ef, nsim = 10000)
data_curated <- map_dfr(data_raw, curate, .id = "scenario") |> as_tibble()

# compute confidence intervals
data_curated <- data_curated |>
  mutate(ci_lower = lower(power, nsim),
         ci_upper = upper(power, nsim))

saveRDS(data_raw, "cundill/data/replication_ngebinom_raw.RDS")
write_csv(data_curated, "cundill/data/replication_negbinom_curated.csv")
