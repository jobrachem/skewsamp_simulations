source("chak/R/funs.simulation.resample.R")
source("chak/R/funs.simulation.n.R")
source("chak/R/funs.power.R")

cat("\n\nRunning replication | resampling.\n")

scenarios <- list(
  norm10 = list(m = 10, delta = 0.5, dist_fun = rnorm),
  norm20 = list(m = 20, delta = 0.5, dist_fun = rnorm),
  unif10 = list(m = 10, delta = 0.2, dist_fun = runif),
  unif20 = list(m = 20, delta = 0.2, dist_fun = runif),
  exp10 = list(m = 10, delta = 0.35, dist_fun = rexp),
  exp20 = list(m = 20, delta = 0.35, dist_fun = rexp),
  log10 = list(m = 10, delta = 0.8, dist_fun = rlogis),
  log20 = list(m = 20, delta = 0.8, dist_fun = rlogis)
)


set.seed(20210714)
nsim <- 500
n_resamples <- 500
results <- run_sim(nsim = nsim, n_resamples = n_resamples, scenarios = scenarios)
sim_results <- postprocess_sim_results_distributions(scenarios, results)


qfun <- function(results) sapply(results, quantile, type = 3, probs = 0.9)
q90 <- lapply(results, qfun)
sim_results_df <- postprocess_sim_results(scenarios, q90) |>
  dplyr::rename(n_q90_mean = n_mean,
         n_q90_sd = n_sd) |>
  dplyr::select(-c(n_q10, n_median, n_q90)) |>
  dplyr::mutate(nsim = nsim, n_resamples = n_resamples)


set.seed(13)
pwr_base <- list(nsim = 10000, alpha = 0.05)
pwr_scenarios <- lapply(scenarios, function(scen) c(scen, pwr_base))
for (i in seq_along(results)) {
  pwr_scenarios[[i]][["n"]] <- sim_results_df$n_q90_mean[i]
}
pwr_results <- pwr_sim_scenarios(pwr_scenarios)
df <- postprocess_pwr_results(pwr_scenarios, pwr_results)
df$n_description = "q90_mean"


saveRDS(results, "chak/data/locshift.chak.resample.RDS")
write.csv(sim_results_df, "chak/data/locshift.chak.resample.q90.csv", row.names = FALSE)
write.csv(df, "chak/data/locshift.chak.resample.q90.pwr.csv", row.names = FALSE)
