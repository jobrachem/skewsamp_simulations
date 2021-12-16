library(tidyverse)
source("cundill/R/funs.scenario.R")
source("cundill/R/funs.simulation.R")
source("cundill/R/funs.curate.R")
source("interval.R")

cat("\n\nRunning replication\n")

scenarios <- list(
  cundill.binom1.log = binom_scenario(p0 = 0.1, size = 1),
  cundill.binom1.identity = binom_scenario(p0 = 0.1, size = 1, linkfun = "identity"),

  cundill.binom4.log = binom_scenario(p0 = 0.5, size = 1),
  cundill.binom4.identity = binom_scenario(p0 = 0.5, size = 1, linkfun = "identity")
)

ef <- seq(from = 0.3, to = 0.7, length.out = 20)
set.seed(20210702)

data_raw <- map(scenarios, run_scenario, ef = ef, nsim = 10000)
data_curated <- map_dfr(data_raw, curate, .id = "scenario") |> as_tibble()

# compute confidence intervals
data_curated <- data_curated |>
  mutate(ci_lower = lower(power, nsim),
         ci_upper = upper(power, nsim))

saveRDS(data_raw, "cundill/data/replication_binom_raw.RDS")
write_csv(data_curated, "cundill/data/replication_binom_curated.csv")

