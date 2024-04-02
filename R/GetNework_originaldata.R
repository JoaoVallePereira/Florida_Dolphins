########### Network exploration / analysis

# Load dyads data

# fit_edge <- readRDS("./data/processed/fit_edge.rds")
# fit_null <- readRDS("./data/processed/fit_null.rds")
# fit_brm <- readRDS("./data/processed/fit_brm.rds")

df_dyads <- readRDS(file = "./data/processed/DF_dyads.rds") %>% 
  dplyr::mutate(duration = 1)
head(df_dyads)

# Define priors

priors <- bisonR::get_default_priors("binary")
priors

bisonR::prior_check(priors, "binary")

# Fit bison model
fit_edge <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = df_dyads, 
  model_type = "binary",
  priors = priors)

# Check if MCMC algorithm has behaved correctly
bisonR::plot_trace(fit_edge, par_ids=2)

# Check the predictions from the fitted model against the real data
bisonR::plot_predictions(fit_edge, num_draws=20, type="density")
bisonR::plot_predictions(fit_edge, num_draws=20, type="point")
summary(fit_edge)

# Plot the network
bisonR::plot_network(fit_edge, lwd = 5)


# Bayesian version of the Bejder et al. 1998 test for non-random association
fit_null <- bisonR::bison_model(
  (social_event | duration) ~ 1, 
  data = df_dyads, 
  model_type = "binary",
  priors = priors)

bisonR::model_comparison(list(non_random_model = fit_edge, random_model = fit_null))


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
plot(density(cent_samples))

# Node betweenness
bet_samples <- extract_metric(fit_edge, "node_betweenness")
bet_samples[1:6, 1:5]

# Regression test
df_dyadic <- df_dyads %>%
  dplyr::filter(foraging == "Yes" |
                  foraging == "Prob") %>% 
  dplyr::mutate(tactic_mrf = as.factor(ifelse(tactic == "MRF", "MRF", "OTHER"))) %>% 
  dplyr::distinct(node_1, node_2, tactic_mrf)
df_dyadic

fit_dyadic <- bisonR::bison_brm(
  bison(edge_weight(node_1, node_2)) ~ tactic_mrf,
  fit_edge,
  df_dyadic,
  num_draws = 5, # Small sample size for demonstration purposes
  refresh = 0)
summary(fit_dyadic)



fit_dyads_tactic <- bisonR::bison_brm(
  bison(edge_weight(node_1, node_2)) ~ tactic,
  fit_edge,
  df_dyads,
  num_draws = 5, # Small sample size for demonstration purposes
  refresh = 0)
summary(fit_dyadic)

saveRDS(fit_edge, file = "./data/processed/fit_edge.rds")
saveRDS(fit_dyads_tactic, file = "./data/processed/fit_brm.rds")
saveRDS(fit_null, file = "./data/processed/fit_null.rds")
