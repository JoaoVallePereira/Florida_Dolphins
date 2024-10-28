fit_edgeALL <- readRDS("./data/processed/processed_count/fit_edgeall_c.rds")
fit_edgeNoFor <- readRDS("./data/processed/processed_count/fit_edgeNoFor_c.rds")
fit_edgeFor <- readRDS("./data/processed/processed_count/fit_edgeFor_c.rds")
fit_edgeMRF <-readRDS("./data/processed/processed_count/fit_edgeMRF_c.rds")


# Bayesian version of the Bejder et al. 1998 test for non-random association
fit_null_all <- bisonR::bison_model(
  (social_event | duration) ~ 1, 
  data = dfDyads_all_count, 
  model_type = "count")

bisonR::model_comparison(list(non_random_model = fit_edgeALL, random_model = fit_null_all))

# Bayesian version of the Bejder et al. 1998 test for non-random association
fit_null_NoFor <- bisonR::bison_model(
  (social_event | duration) ~ 1, 
  data = dfDyads_Nofor_count, 
  model_type = "count")

bisonR::model_comparison(list(non_random_model = fit_edgeNoFor, random_model = fit_null_NoFor))

# Bayesian version of the Bejder et al. 1998 test for non-random association
fit_null_For <- bisonR::bison_model(
  (social_event | duration) ~ 1, 
  data = dfDyads_for_count, 
  model_type = "count")

bisonR::model_comparison(list(non_random_model = fit_edgeFor, random_model = fit_null_For))

# Bayesian version of the Bejder et al. 1998 test for non-random association
fit_null_MRF <- bisonR::bison_model(
  (social_event | duration) ~ 1, 
  data = dfDyads_MRF_count, 
  model_type = "count")

bisonR::model_comparison(list(non_random_model = fit_edgeMRF, random_model = fit_null_MRF))

