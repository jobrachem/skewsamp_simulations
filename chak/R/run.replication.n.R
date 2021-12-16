source("chak/R/funs.simulation.n.R")
source("chak/R/funs.power.R")

cat("\n\nRunning replication.\n")

sim_base <- pwr_base <- list(
  nsim = 10000,
  alpha = 0.05
)

sim_base$power <- 0.9

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

# Get N estimates
sim_scenarios <- lapply(scenarios, function(x) c(x, sim_base))

set.seed(20210714)
results <- run_sim_locshift(sim_scenarios)

n <- sapply(results, function(x) ceiling(mean(x)))

sim_results_df <- postprocess_sim_results(sim_scenarios, results)
sim_results <- postprocess_sim_results_distributions(sim_scenarios, results)


# Estimate empirical power
set.seed(20210714)
pwr_scenarios <- lapply(scenarios, function(scen) c(scen, pwr_base))
for (i in seq_along(n)) {
  pwr_scenarios[[i]][["n"]] <- sim_results_df$n_mean[i]
}
pwr_results <- pwr_sim_scenarios(pwr_scenarios)
df <- postprocess_pwr_results(pwr_scenarios, pwr_results)
df$n_description = "mean"

# Estimate empirical power for quantiles of N distribution
probs <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
nquant <- lapply(results, function(x) ceiling(quantile(x, type = 3, probs = probs)))

for (j in seq_along(probs)) {
  pwr_scenarios <- lapply(scenarios, function(scen) c(scen, pwr_base))
  for (i in seq_along(n)) {
    pwr_scenarios[[i]][["n"]] <- nquant[[i]][j]
  }
  pwr_results <- pwr_sim_scenarios(pwr_scenarios)
  df_quant <- postprocess_pwr_results(pwr_scenarios, pwr_results)
  df_quant$n_description = paste0("q", probs[j])

  df <- dplyr::bind_rows(df, df_quant)
}

write.csv(sim_results_df, "chak/data/locshift.chak.n.csv", row.names = FALSE)
saveRDS(sim_results, "chak/data/locshift.chak.n.estimates.RDS")
write.csv(df, "chak/data/locshift.chak.pwr.csv", row.names = FALSE)
