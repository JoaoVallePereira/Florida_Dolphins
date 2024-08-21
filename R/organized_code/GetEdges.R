################################
dfDyads_all <- readRDS(file = "./data/processed/DF_dyads.rds") %>% 
  dplyr::mutate(duration = 1)
head(dfDyads_all)

dfDyads_all_join <- dfDyads_all %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) 

dfDyads_all_joined <- dfDyads_all %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) %>% 
  dplyr::group_by(dyad_id) %>% 
  dplyr::summarise(event_sum = sum(social_event),
                   duration_sum = sum(duration))
head(dfDyads_all_joined)

dfDyads_all_count <- dfDyads_all_joined %>% 
  dplyr::left_join(dfDyads_all_join, by = "dyad_id") %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE) %>% 
  dplyr::select(node_1, node_2, social_event = event_sum, duration = duration_sum, Fpdiff)
head(dfDyads_all_count)


# Fit bison model count
# Define priors

priors <- bisonR::get_default_priors("count")
priors

bisonR::prior_check(priors, "count")

# Fit model
fit_edgeall_c <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_all_count, 
  model_type = "count",
  priors = priors)

# Plot the network
bisonR::plot_network(fit_edgeall_c, lwd = 5)


################################
dfDyads_Nofor <- readRDS(file = "./data/processed/DF_dyadsNofor.rds") %>% 
  dplyr::mutate(duration = 1)
head(dfDyads_Nofor)

dfDyads_Nofor_join <- dfDyads_Nofor %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) 

dfDyads_Nofor_joined <- dfDyads_Nofor %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) %>% 
  dplyr::group_by(dyad_id) %>% 
  dplyr::summarise(event_sum = sum(social_event),
                   duration_sum = sum(duration))
head(dfDyads_Nofor_joined)

dfDyads_Nofor_count <- dfDyads_Nofor_joined %>% 
  dplyr::left_join(dfDyads_Nofor_join, by = "dyad_id") %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE) %>% 
  dplyr::select(node_1, node_2, social_event = event_sum, duration = duration_sum, Fpdiff)
head(dfDyads_Nofor_count)


# Fit bison model count
# Define priors

priors <- bisonR::get_default_priors("count")
priors

bisonR::prior_check(priors, "count")

# Fit model
fit_edgeNoFor_c <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_Nofor_count, 
  model_type = "count",
  priors = priors)

# Plot the network
bisonR::plot_network(fit_edgeNoFor_c, lwd = 5)

################################
dfDyads_for <- readRDS(file = "./data/processed/DF_dyadsFor.rds") %>% 
  dplyr::mutate(duration = 1)
head(dfDyads_for)

dfDyads_for_join <- dfDyads_for %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) 

dfDyads_for_joined <- dfDyads_for %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) %>% 
  dplyr::group_by(dyad_id) %>% 
  dplyr::summarise(event_sum = sum(social_event),
                   duration_sum = sum(duration))
head(dfDyads_for_joined)

dfDyads_for_count <- dfDyads_for_joined %>% 
  dplyr::left_join(dfDyads_for_join, by = "dyad_id") %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE) %>% 
  dplyr::select(node_1, node_2, social_event = event_sum, duration = duration_sum, Fpdiff)
head(dfDyads_for_count)


# Fit bison model count
# Define priors

priors <- bisonR::get_default_priors("count")
priors

bisonR::prior_check(priors, "count")

# Fit model
fit_edgeFor_c <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_for_count, 
  model_type = "count",
  priors = priors)

# Plot the network
bisonR::plot_network(fit_edgeFor_c, lwd = 5)


################################
dfDyads_MRF <- readRDS(file = "./data/processed/DF_dyadsMRF.rds") %>% 
  dplyr::mutate(duration = 1)
head(dfDyads_MRF)

dfDyads_MRF_join <- dfDyads_MRF %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) 

dfDyads_MRF_joined <- dfDyads_MRF %>% 
  dplyr::filter(social_event != 0) %>% 
  dplyr::mutate(dyad_id = paste(node_1, node_2, sep = "")) %>% 
  dplyr::group_by(dyad_id) %>% 
  dplyr::summarise(event_sum = sum(social_event),
                   duration_sum = sum(duration))
head(dfDyads_MRF_joined)

dfDyads_MRF_count <- dfDyads_MRF_joined %>% 
  dplyr::left_join(dfDyads_MRF_join, by = "dyad_id") %>% 
  dplyr::distinct(node_1, node_2, .keep_all = TRUE) %>% 
  dplyr::select(node_1, node_2, social_event = event_sum, duration = duration_sum, Fpdiff)
head(dfDyads_MRF_count)


# Fit bison model count
# Define priors

priors <- bisonR::get_default_priors("count")
priors

bisonR::prior_check(priors, "count")

# Fit model
fit_edgeMRF_c <- bisonR::bison_model(
  (social_event | duration) ~ dyad(node_1, node_2), 
  data = dfDyads_MRF_count, 
  model_type = "count",
  priors = priors)

# Plot the network
bisonR::plot_network(fit_edgeMRF_c, lwd = 5)



# Plot networks togheter

par(mfrow = c(2, 2))
bisonR::plot_network(fit_edgeall_c, lwd = 5)
title("ALL", line = -0.9)
bisonR::plot_network(fit_edgeNoFor_c, lwd = 5)
title("No For", line = -0.9)
bisonR::plot_network(fit_edgeFor_c, lwd = 5)
title("For", line = -0.9)
bisonR::plot_network(fit_edgeMRF_c, lwd = 5)
title("MRF", line = -0.9)