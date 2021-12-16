source("chak/R/funs.simulation.n.R")
source("chak/R/funs.power.R")


cat("\n\nRunning additional simulations for exponential dist.\n")

sim_base <- pwr_base <- list(
  nsim = 10000,
  alpha = 0.05
)

sim_base$power <- 0.9

scenarios <- list()

rate <- c(0.2, 1:5)
delta <- c(0.1, 0.35, 0.5, 1, 1.5, 2)

for (i in seq_along(rate)) {
  for (j in seq_along(delta)) {
    m10 <- list(m = 10, delta = delta[j], rate = rate[i], dist_fun = rexp)
    m20 <- list(m = 20, delta = delta[j], rate = rate[i], dist_fun = rexp)
    scenarios[[glue::glue("exp10.r{i}.d{j}")]] <- m10
    scenarios[[glue::glue("exp20.r{i}.d{j}")]] <- m20
  }
}

# Get N estimates
sim_scenarios <- lapply(scenarios, function(x) c(x, sim_base))

set.seed(12)
results <- run_sim_locshift(sim_scenarios)

n <- sapply(results, function(x) ceiling(mean(x)))

sim_results_df <- postprocess_sim_results(sim_scenarios, results)
sim_results <- postprocess_sim_results_distributions(sim_scenarios, results)

write.csv(sim_results_df, "chak/data/locshift.exp.n.csv", row.names = FALSE)
saveRDS(sim_results, "chak/data/locshift.exp.n.estimates.RDS")

# Estimate empirical power
pwr_scenarios <- lapply(scenarios, function(scen) c(scen, pwr_base))
for (i in seq_along(n)) {
  pwr_scenarios[[i]][["n"]] <- sim_results_df$n_mean[i]
}


set.seed(13)
pwr_results <- pwr_sim_scenarios(pwr_scenarios)

df <- postprocess_pwr_results(pwr_scenarios, pwr_results)
write.csv(df, "chak/data/locshift.exp.pwr.csv", row.names = FALSE)
