########### Network exploration / analysis

# Load dyads data

df_dyads <- readRDS(file = "./data/processed/DF_dyads.rds") %>% 
  dplyr::mutate(duration = 1)
head(df_dyads)

# Define priors

priors <- bisonR::get_default_priors("binary")
priors

bisonR::prior_check(priors, "binary")

# Fit bison model
fit_edgeALL <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = df_dyads, 
  model_type = "binary",
  priors = priors)
saveRDS(fit_edgeALL, file = "./data/processed/fit_edgeALL.rds")

################################
dfDyads_Nofor <- readRDS(file = "./data/processed/DF_dyadsNofor.rds") %>% 
  dplyr::mutate(duration = 1)
head(dfDyads_Nofor)

# Define priors

priors <- bisonR::get_default_priors("binary")
priors

bisonR::prior_check(priors, "binary")

# Fit bison model
fit_edgeNoFor <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_Nofor, 
  model_type = "binary",
  priors = priors)
saveRDS(fit_edgeNoFor, file = "./data/processed/fit_edgeNoFor.rds")

################################
dfDyads_For <- readRDS(file = "./data/processed/DF_dyadsFor.rds") %>% 
  dplyr::mutate(duration = 1)
head(dfDyads_For)

# Define priors

priors <- bisonR::get_default_priors("binary")
priors

bisonR::prior_check(priors, "binary")

# Fit bison model
fit_edgeFor <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_For, 
  model_type = "binary",
  priors = priors)
saveRDS(fit_edgeFor, file = "./data/processed/fit_edgeFor.rds")

################################
dfDyads_MRF <- readRDS(file = "./data/processed/DF_dyadsMRF.rds") %>% 
  dplyr::mutate(duration = 1)
head(dfDyads_MRF)

# Define priors

priors <- bisonR::get_default_priors("binary")
priors

bisonR::prior_check(priors, "binary")

# Fit bison model
fit_edgeMRF <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_MRF, 
  model_type = "binary",
  priors = priors)
saveRDS(fit_edgeMRF, file = "./data/processed/fit_edgeMRF.rds")

####

summary.fit_edgeALL <- summary(fit_edgeALL)
edgelist.fit_edgeALL <- summary.fit_edgeALL$edgelist %>% 
  dplyr::left_join(dfDyads_ALL, by = c("node_1", "node_2")) %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE)

summary.fit_edgeNoFor <- summary(fit_edgeNoFor)
edgelist.fit_edgeNoFor <- summary.fit_edgeNoFor$edgelist %>% 
  dplyr::left_join(dfDyads_NoFor, by = c("node_1", "node_2")) %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE)

summary.fit_edgeFor <- summary(fit_edgeFor)
edgelist.fit_edgeFor <- summary.fit_edgeFor$edgelist  %>% 
  dplyr::left_join(dfDyads_For, by = c("node_1", "node_2")) %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE)

summary.fit_edgeMRF <- summary(fit_edgeMRF)
edgelist.fit_edgeMRF <- summary.fit_edgeMRF$edgelist %>% 
  dplyr::left_join(dfDyads_MRF, by = c("node_1", "node_2")) %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE)

edgelist.fit_edgeALL
edgelist.fit_edgeNoFor
edgelist.fit_edgeFor
edgelist.fit_edgeMRF



















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
