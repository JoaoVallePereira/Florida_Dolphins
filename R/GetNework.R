########### Network exploration / analysis

# Load dyads data

df_dyads <- readRDS("./dataAW/processedAW/dfDyads.rds") 
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
bisonR::plot_network(fit_edge, lwd = 3)


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














