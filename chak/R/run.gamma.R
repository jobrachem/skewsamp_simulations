source("chak/R/funs.simulation.n.R")
source("chak/R/funs.power.R")

cat("\n\nRunning additional simulations for gamma dist.\n")

sim_base <- pwr_base <- list(
  dist_fun = rgamma,
  nsim = 10000,
  alpha = 0.05,
  power = 0.9
)

sim_scenarios <- list()
shape <- c(0.5, 1, 2, 3, 7.5)
scale <- c(0.5, 1, 2, 3, 6)
delta <- c(0.1, 0.5, 1, 1.5, 2)
mvec <- c(10, 20)

for (i in seq_along(shape)) {
  for (j in seq_along(scale)) {
    for (k in seq_along(mvec)) {
      for (l in seq_along(delta)) {
        sh <- shape[i]
        sc <- scale[j]
        m <- mvec[k]
        d <- delta[l]

        scen <- list(m = m, delta = d, shape = sh, scale = sc)
        scen_name <- glue::glue("gamma.m{m}.d{d}.shape{sh}.scale{sc}")

        sim_scenarios[[scen_name]] <- c(sim_base, scen)
      }
    }
  }
}

set.seed(12)
results <- run_sim_locshift(sim_scenarios)

n <- sapply(results, function(x) ceiling(mean(x)))

sim_results_df <- postprocess_sim_results(sim_scenarios, results)
sim_results <- postprocess_sim_results_distributions(sim_scenarios, results)


# Estimate empirical power
pwr_scenarios <- sim_scenarios
for (i in seq_along(pwr_scenarios)) pwr_scenarios[[i]][["n"]] <- sim_results_df$n_mean[i]

set.seed(13)
pwr_results <- pwr_sim_scenarios(pwr_scenarios)

df <- postprocess_pwr_results(pwr_scenarios, pwr_results)

write.csv(sim_results_df, "chak/data/test.locshift.gamma.n.csv", row.names = FALSE)
saveRDS(sim_results, "chak/data/test.locshift.gamma.n.estimates.RDS")
write.csv(df, "chak/data/test.locshift.gamma.pwr.csv", row.names = FALSE)
