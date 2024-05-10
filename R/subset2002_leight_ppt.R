########### Data pre-processing

# If you are running and you see this message
# Go for the GetNetwork.R script. I created a randomized
# dyads data frame from the original data (here DF_dyads.rds)
# to run the analysis while data is not public

# Load raw data ----

data.raw <- read.csv("./data/raw/data_for_SOCPROG.csv") %>% 
  dplyr::select(date = DATE,
                daily_sighting = Sighting,
                foraging = Forage,
                tactic = Tactic,
                zone = Zone,
                lat = LAT,
                long = LONG,
                IDs = Dolphins) %>% 
  dplyr::mutate(IDs = stringr::str_squish(IDs),
                IDs = ifelse(IDs == "", NA, IDs),
                IDs_list = strsplit(IDs, split = " "),
                identities = lapply(IDs_list, unlist),
                date = as.POSIXct(date, format = "%m/%d/%y"),
                tactic = ifelse(tactic == "UK", "DD", tactic)) %>% ###################################### CHECK WITH LEIGH 
  tidyr::drop_na(IDs) %>% 
  dplyr::mutate(year = lubridate::year(date)) %>% 
  dplyr::filter(year == "2002") %>% 
  dplyr::select(-year)
head(data.raw)

dfINFO <- data.raw %>% 
  dplyr::select(date, foraging, tactic, zone, IDs, lat, long) 

# Long data frame with groups column

dfINFO_long <- dfINFO %>% 
  dplyr::mutate(group = rownames(.)) %>% 
  tidyr::separate_rows(IDs, sep = " ") %>%
  data.frame()

dfGroupSize <- dfINFO_long %>% 
  dplyr::group_by(group) %>% 
  dplyr::count()

dfINFO_long <- dfINFO_long %>% 
  dplyr::left_join(dfGroupSize, by = "group") 
head(dfINFO_long)

# Get Fp MRF

df_FpMRF <- dfINFO_long %>% 
  dplyr::filter(foraging == "Yes" |
                  foraging == "Prob") %>%
  dplyr::mutate(MRF = ifelse(tactic == "MRF", "MRF", "other")) %>% 
  dplyr::group_by(IDs, MRF) %>% 
  dplyr::count() %>% 
  tidyr::spread(MRF, n) %>% 
  dplyr::mutate(MRF = ifelse(is.na(MRF), 0, MRF),
                other = ifelse(is.na(other), 0, other),
                all = MRF + other) %>% 
  dplyr::mutate(FpMRF = MRF/all)

# Merge df info long + df FP MRF

dfINFO_long <- dfINFO_long %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(IDs, FpMRF),
                   by = "IDs") %>% 
  dplyr::mutate(FpMRF = ifelse(is.na(FpMRF), 0, FpMRF))

# Filter IDs > 1
dfCount <- dfINFO_long %>% 
  dplyr::group_by(IDs) %>% 
  dplyr::count() %>% 
  dplyr::filter(n > 1)
dfIDs <- dfCount$IDs

dfINFO_long_filter <- dfINFO_long %>% 
  dplyr::filter(IDs %in% dfIDs) %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarize(identities = list(IDs)) %>% 
  dplyr::inner_join(dfINFO_long, by = "group") %>% 
  dplyr::distinct(group, .keep_all = TRUE) %>% 
  dplyr::select(- IDs)

head(dfINFO_long_filter)


###############################################
# Get GBI all ----

dataForGBI <- dfINFO_long_filter %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))


rawGBI <- asnipe::get_group_by_individual(dataForGBI$identities, 
                                          data_format = "groups")  
obsALL <- rawGBI


# Get GBI no forag----

dataForGBI_Nofor <- dfINFO_long_filter %>% 
  dplyr::filter(foraging == "No") %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

Nofor_GBI <- asnipe::get_group_by_individual(dataForGBI_Nofor$identities, 
                                             data_format = "groups")
obsNoFor <- Nofor_GBI


# Get GBI forag----

dataForGBI_For <- dfINFO_long_filter %>% 
  dplyr::filter(foraging == "Yes" |
                  foraging == "Prob") %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

For_GBI <- asnipe::get_group_by_individual(dataForGBI_For$identities, 
                                           data_format = "groups")
obsFor <- For_GBI

# Get GBI MRF----

dataForGBI_MRF <- dfINFO_long_filter %>% 
  dplyr::filter(tactic == "MRF") %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

MRF_GBI <- asnipe::get_group_by_individual(dataForGBI_MRF$identities, 
                                           data_format = "groups")
obsMRF <- MRF_GBI



## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
obs <- obsALL

for (obs_id in 1:nrow(obs)) {
  for (i in which(obs[obs_id, ] == 1)) {
    for (j in 1:ncol(obs)) {
      if (i != j) {
        # Swap i and j if necessary to make sure node_1 < node_2, not essential but makes things a bit easier when assigning dyad IDs.
        if (i < j) {
          node_1 <- i
          node_2 <- j
        } else {
          node_1 <- j
          node_2 <- i
        }
        df[nrow(df) + 1, ] <- list(node_1 = node_1, node_2 = node_2, social_event=(obs[obs_id, i] == obs[obs_id, j]), obs_id = obs_id)
      }
    }
  }
}
head(df)

IDs_ALL_node1 <- data.frame(IDs = rownames(t(obsALL))) %>% 
  dplyr::mutate(node_1 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node1 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

IDs_ALL_node2 <- data.frame(IDs = rownames(t(obsALL))) %>% 
  dplyr::mutate(node_2 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node2 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

# Get dyads with observations information
dfDyads_full <- df %>%  
  dplyr::left_join(dataForGBI, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id)) %>% 
  dplyr::left_join(IDs_ALL_node1, by = "node_1") %>% 
  dplyr::left_join(IDs_ALL_node2, by = "node_2") %>% 
  dplyr::rename(node_1ID = IDs.x, node_2ID = IDs.y) %>% 
  dplyr::mutate(Fpdiff = FpMRF_node1 - FpMRF_node2)
# saveRDS(dfDyads_full, file = "./data/processed/DF_dyads.rds")

## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
obs   <- obsNoFor


for (obs_id in 1:nrow(obs)) {
  for (i in which(obs[obs_id, ] == 1)) {
    for (j in 1:ncol(obs)) {
      if (i != j) {
        # Swap i and j if necessary to make sure node_1 < node_2, not essential but makes things a bit easier when assigning dyad IDs.
        if (i < j) {
          node_1 <- i
          node_2 <- j
        } else {
          node_1 <- j
          node_2 <- i
        }
        df[nrow(df) + 1, ] <- list(node_1 = node_1, node_2 = node_2, social_event=(obs[obs_id, i] == obs[obs_id, j]), obs_id = obs_id)
      }
    }
  }
}
head(df)

IDs_NoFor_node1 <- data.frame(IDs = rownames(t(obsNoFor))) %>% 
  dplyr::mutate(node_1 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node1 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

IDs_NoFor_node2 <- data.frame(IDs = rownames(t(obsNoFor))) %>% 
  dplyr::mutate(node_2 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node2 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

# Get dyads with observations information
dfDyads_Nofor <- df %>% 
  dplyr::left_join(dataForGBI_Nofor, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id)) %>% 
  dplyr::left_join(IDs_NoFor_node1, by = "node_1") %>% 
  dplyr::left_join(IDs_NoFor_node2, by = "node_2") %>% 
  dplyr::rename(node_1ID = IDs.x, node_2ID = IDs.y) %>% 
  dplyr::mutate(Fpdiff = FpMRF_node1 - FpMRF_node2)
# saveRDS(dfDyads_Nofor, file = "./data/processed/DF_dyadsNofor.rds")

## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
obs <- obsFor

for (obs_id in 1:nrow(obs)) {
  for (i in which(obs[obs_id, ] == 1)) {
    for (j in 1:ncol(obs)) {
      if (i != j) {
        # Swap i and j if necessary to make sure node_1 < node_2, not essential but makes things a bit easier when assigning dyad IDs.
        if (i < j) {
          node_1 <- i
          node_2 <- j
        } else {
          node_1 <- j
          node_2 <- i
        }
        df[nrow(df) + 1, ] <- list(node_1 = node_1, node_2 = node_2, social_event=(obs[obs_id, i] == obs[obs_id, j]), obs_id = obs_id)
      }
    }
  }
}
head(df)

IDs_For_node1 <- data.frame(IDs = rownames(t(obsFor))) %>% 
  dplyr::mutate(node_1 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node1 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

IDs_For_node2 <- data.frame(IDs = rownames(t(obsFor))) %>% 
  dplyr::mutate(node_2 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node2 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

# Get dyads with observations information
dfDyads_For <- df %>% 
  dplyr::left_join(dataForGBI_For, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id)) %>% 
  dplyr::left_join(IDs_For_node1, by = "node_1") %>% 
  dplyr::left_join(IDs_For_node2, by = "node_2") %>% 
  dplyr::rename(node_1ID = IDs.x, node_2ID = IDs.y) %>% 
  dplyr::mutate(Fpdiff = FpMRF_node1 - FpMRF_node2)
# saveRDS(dfDyads_For, file = "./data/processed/DF_dyadsFor.rds")

## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
obs <- obsMRF

for (obs_id in 1:nrow(obs)) {
  for (i in which(obs[obs_id, ] == 1)) {
    for (j in 1:ncol(obs)) {
      if (i != j) {
        # Swap i and j if necessary to make sure node_1 < node_2, not essential but makes things a bit easier when assigning dyad IDs.
        if (i < j) {
          node_1 <- i
          node_2 <- j
        } else {
          node_1 <- j
          node_2 <- i
        }
        df[nrow(df) + 1, ] <- list(node_1 = node_1, node_2 = node_2, social_event=(obs[obs_id, i] == obs[obs_id, j]), obs_id = obs_id)
      }
    }
  }
}
head(df)

IDs_MRF_node1 <- data.frame(IDs = rownames(t(obsMRF))) %>% 
  dplyr::mutate(node_1 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node1 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

IDs_MRF_node2 <- data.frame(IDs = rownames(t(obsMRF))) %>% 
  dplyr::mutate(node_2 = as.factor(1:nrow(.))) %>% 
  dplyr::left_join(df_FpMRF %>% 
                     dplyr::select(FpMRF, IDs), by = "IDs") %>% 
  dplyr::mutate(FpMRF_node2 = ifelse(is.na(FpMRF), 0, FpMRF)) %>% 
  dplyr::select(-FpMRF)

# Get dyads with observations information
dfDyads_MRF <- df %>% 
  dplyr::left_join(dataForGBI_MRF, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id)) %>% 
  dplyr::left_join(IDs_MRF_node1, by = "node_1") %>% 
  dplyr::left_join(IDs_MRF_node2, by = "node_2") %>% 
  dplyr::rename(node_1ID = IDs.x, node_2ID = IDs.y) %>% 
  dplyr::mutate(Fpdiff = FpMRF_node1 - FpMRF_node2)
# saveRDS(dfDyads_MRF, file = "./data/processed/DF_dyadsMRF.rds")
#####################################################################################################################################

########### Network exploration / analysis

# Load dyads data
head(dfDyads_full)
dfDyads_full <- dfDyads_full %>% 
  dplyr::mutate(duration = 1)

# Define priors

priors <- bisonR::get_default_priors("binary")
priors

bisonR::prior_check(priors, "binary")

# Fit bison model
fit_edgeALL <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_full, 
  model_type = "binary",
  priors = priors)
# saveRDS(fit_edgeALL, file = "./data/processed/fit_edgeALL.rds")


################################
head(dfDyads_Nofor)
dfDyads_Nofor <- dfDyads_Nofor %>% 
  dplyr::mutate(duration = 1)
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
# saveRDS(fit_edgeNoFor, file = "./data/processed/fit_edgeNoFor.rds")

################################
head(dfDyads_For)
dfDyads_For <- dfDyads_For %>% 
  dplyr::mutate(duration = 1)

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


################################
head(dfDyads_MRF)
dfDyads_MRF <- dfDyads_MRF %>% 
  dplyr::mutate(duration = 1)


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

####

summary.fit_edgeALL <- summary(fit_edgeALL)
edgelist.fit_edgeALL <- summary.fit_edgeALL$edgelist %>% 
  dplyr::left_join(dfDyads_full, by = c("node_1", "node_2")) %>% 
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
dyadicALL <- dfDyads_full %>% 
  dplyr::select(node_1, node_2, Fpdiff) %>% 
  dplyr::distinct()

fit_dyadicALL <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeALL,
  dyadicALL,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
# saveRDS(fit_dyadicALL, file = "./data/processed/fit_dyadicALL.rds")
performance::r2_bayes(fit_dyadicALL)
tidybayes::get_variables(fit_dyadicALL)
a <- as.matrix(fit_dyadicALL)
bayesplot::mcmc_intervals(a,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()



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
# saveRDS(fit_dyadicNoFor, file = "./data/processed/dyadicNoFor.rds")
summary(fit_dyadicNoFor)
performance::r2_bayes(fit_dyadicNoFor)
tidybayes::get_variables(fit_dyadicNoFor)
b <- as.matrix(fit_dyadicNoFor)
bayesplot::mcmc_intervals(b,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()



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
# saveRDS(fit_dyadicFor, file = "./data/processed/fit_dyadicFor.rds")
tidybayes::get_variables(fit_dyadicFor)
c <- as.matrix(fit_dyadicFor)
bayesplot::mcmc_intervals(c,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()

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
# saveRDS(fit_dyadicMRF, file = "./data/processed/fit_dyadicMRF.rds")
d <- as.matrix(fit_dyadicMRF)
bayesplot::mcmc_intervals(d,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()

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

# saveRDS(fit_edge, file = "./data/processed/fit_edge.rds")
# saveRDS(fit_dyads_tactic, file = "./data/processed/fit_brm.rds")
# saveRDS(fit_null, file = "./data/processed/fit_null.rds")














