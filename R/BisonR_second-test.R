#########

df_dyads <- readRDS("./data/processed/DF_dyads.rds") %>% 
  dplyr::mutate(duration = 1)
head(df_dyads)

# Define priors

priors <- get_default_priors("binary")
priors

prior_check(priors, "binary")

# Fit bison model
fit_edge <- bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = df_dyads, 
  model_type= "binary",
  priors=priors)
# saveRDS(fit_edge, file = "./data/processed/fit_edge.rds")
fit_edge <- readRDS("./data/processed/fit_edge.rds")

# Check if MCMC algorithm has behaved correctly
plot_trace(fit_edge, par_ids=2)

# Check the predictions from the fitted model against the real data
plot_predictions(fit_edge, num_draws=20, type="density")
plot_predictions(fit_edge, num_draws=20, type="point")

#####
summary(fit_edge)

# Plot the network
plot_network(fit_edge, lwd = 5)


# Bayesian version of the Bejder et al. 1998 test for non-random association
fit_null <- bison_model(
  (social_event | duration) ~ 1, 
  data = df_dyads, 
  model_type = "binary",
  priors = priors)
# saveRDS(fit_null, file = "./data/processed/fit_null.rds")

model_comparison(list(non_random_model=fit_edge, random_model=fit_null))


# NETWORK METRICS ----

# Global ----
# Coefficient of variation (CV) in edge weights
cv_samples <- extract_metric(fit_edge, "global_cv")
head(cv_samples)
# Visualise the posterior distribution of social differentiation
plot(density(cv_samples))


# Node ----
# Node eigenvector centrality
cent_samples <- extract_metric(fit_edge, "node_eigen")
cent_samples[1:6, 1:5]

# Node betweenness
bet_samples <- extract_metric(fit_edge, "node_betweenness")
bet_samples[1:6, 1:5]































