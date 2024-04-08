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
  dplyr::left_join(df_dyads, by = c("node_1", "node_2")) %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE)

summary.fit_edgeNoFor <- summary(fit_edgeNoFor)
edgelist.fit_edgeNoFor <- summary.fit_edgeNoFor$edgelist %>% 
  dplyr::left_join(dfDyads_Nofor, by = c("node_1", "node_2")) %>% 
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


##################
edgesALL <- edgelist.fit_edgeALL %>% 
  dplyr::select(node_1ID, node_2ID, weight = median)

nodesALL <- data.frame(c(edgelist.fit_edgeALL$node_1ID[1], unique(edgelist.fit_edgeALL$node_2ID))) %>% 
  dplyr::rename(IDs = 1) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(IDs, FpMRF), by = "IDs") %>% 
  dplyr::mutate(FpMRF = ifelse(is.na(FpMRF), 0, FpMRF))

graphALL <- igraph::graph_from_data_frame(d = edgesALL, vertices = nodesALL, directed = FALSE)
V(graphALL)$size <- nodesALL$FpMRF * 5 + 5
V(graphALL)$label <- round(nodesALL$FpMRF, digits = 2)
plot(graphALL)
title("ALL", line = -0.9)



#################
edgesNoFor <- edgelist.fit_edgeNoFor %>% 
  dplyr::select(node_1ID, node_2ID, weight = median)

nodesNoFor <- data.frame(c(edgelist.fit_edgeNoFor$node_1ID[1], unique(edgelist.fit_edgeNoFor$node_2ID))) %>% 
  dplyr::rename(IDs = 1) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(IDs, FpMRF), by = "IDs") %>% 
  dplyr::mutate(FpMRF = ifelse(is.na(FpMRF), 0, FpMRF))

graphNoFor <- igraph::graph_from_data_frame(d = edgesNoFor, vertices = nodesNoFor, directed = FALSE)
V(graphNoFor)$size <- nodesNoFor$FpMRF * 5 + 5
V(graphNoFor)$label <- round(nodesNoFor$FpMRF, digits = 2)
plot(graphNoFor)
title("No Foraging", line = -0.9)


#############
edgesFor <- edgelist.fit_edgeFor %>% 
  dplyr::select(node_1ID, node_2ID, weight = median)

nodesFor <- data.frame(c(edgelist.fit_edgeFor$node_1ID[1], unique(edgelist.fit_edgeFor$node_2ID))) %>% 
  dplyr::rename(IDs = 1) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(IDs, FpMRF), by = "IDs") %>% 
  dplyr::mutate(FpMRF = ifelse(is.na(FpMRF), 0, FpMRF))

graphFor <- igraph::graph_from_data_frame(d = edgesFor, vertices = nodesFor, directed = FALSE)
V(graphFor)$size <- nodesFor$FpMRF * 5 + 5
V(graphFor)$label <- round(nodesFor$FpMRF, digits = 2)
plot(graphFor)
title("Foraging", line = -0.5)


####################
edgesMRF <- edgelist.fit_edgeMRF %>% 
  dplyr::select(node_1ID, node_2ID, weight = median)

nodesMRF <- data.frame(c(edgelist.fit_edgeMRF$node_1ID[1], unique(edgelist.fit_edgeMRF$node_2ID))) %>% 
  dplyr::rename(IDs = 1) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(IDs, FpMRF), by = "IDs") %>% 
  dplyr::mutate(FpMRF = ifelse(is.na(FpMRF), 0, FpMRF))

graphMRF <- igraph::graph_from_data_frame(d = edgesMRF, vertices = nodesMRF, directed = FALSE)
V(graphMRF)$size <- nodesMRF$FpMRF * 5 + 5
V(graphMRF)$label <- round(nodesMRF$FpMRF, digits = 2)
plot(graphMRF)
title("MRF", line = -0.5)


###
dyadicALL <- df_dyads %>% 
  dplyr::select(node_1, node_2, Fpdiff) %>% 
  dplyr::distinct()

fit_dyadicALL <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeALL,
  dyadicALL,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
saveRDS(fit_dyadicALL, file = "./data/processed/fit_dyadicALL.rds")

###
dyadicNoFor <- dfDyads_Nofor %>% 
  dplyr::select(node_1, node_2, Fpdiff) %>% 
  dplyr::distinct()

fit_dyadicNoFor <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeNoFor,
  dyadicNoFor,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
saveRDS(fit_dyadicNoFor, file = "./data/processed/dyadicNoFor.rds")

###
dyadicFor <- dfDyads_For %>% 
  dplyr::select(node_1, node_2, Fpdiff) %>% 
  dplyr::distinct()

fit_dyadicFor <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeFor,
  dyadicFor,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
saveRDS(fit_dyadicFor, file = "./data/processed/fit_dyadicFor.rds")

###
dyadicMRF <- dfDyads_MRF %>% 
  dplyr::select(node_1, node_2, Fpdiff) %>% 
  dplyr::distinct()

fit_dyadicMRF <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeMRF,
  dyadicMRF,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
saveRDS(fit_dyadicMRF, file = "./data/processed/fit_dyadicMRF.rds")






###### NETWORK METRICS

par(mfrow=c(2,2))
# Node eigenvector centrality
NodeEigen_ALL <- bisonR::extract_metric(fit_edgeALL, "node_eigen")
NodeEigen_ALL
plot(density(NodeEigen_ALL))
title("ALL", line = 0.5)

NodeEigen_NoFor <- bisonR::extract_metric(fit_edgeNoFor, "node_eigen")
NodeEigen_NoFor
plot(density(NodeEigen_NoFor))
title("No Foraging", line = 0.5)

NodeEigen_For <- bisonR::extract_metric(fit_edgeFor, "node_eigen")
NodeEigen_For
plot(density(NodeEigen_For))
title("Foraging", line = 0.5)

NodeEigen_MRF <- bisonR::extract_metric(fit_edgeMRF, "node_eigen")
NodeEigen_MRF
plot(density(NodeEigen_MRF))
title("MRF", line = 0.5)







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
