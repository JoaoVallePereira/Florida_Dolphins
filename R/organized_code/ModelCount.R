###
fit_dyadicALL_c <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeall_c,
  dfDyads_all_count,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
# saveRDS(fit_dyadicALL, file = "./data/processed/fit_dyadicALL.rds")

brms::conditional_effects(fit_dyadicALL_c)

performance::r2_bayes(fit_dyadicALL_c)
tidybayes::get_variables(fit_dyadicALL_c)
a <- as.matrix(fit_dyadicALL_c)
bayesplot::mcmc_areas(a,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()



###
fit_dyadicNoFor_c <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeNoFor_c,
  dfDyads_Nofor_count,
  num_draws = 1000, # Small sample size for demonstration purposes
  refresh = 0)
# saveRDS(fit_dyadicNoFor, file = "./data/processed/dyadicNoFor.rds")
summary(fit_dyadicNoFor_c)

brms::conditional_effects(fit_dyadicNoFor_c)


performance::r2_bayes(fit_dyadicNoFor_c)
tidybayes::get_variables(fit_dyadicNoFor_c)
b <- as.matrix(fit_dyadicNoFor_c)
bayesplot::mcmc_areas(b,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()



###
fit_dyadicFor_c <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeFor_c,
  dfDyads_for_count,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
# saveRDS(fit_dyadicFor, file = "./data/processed/fit_dyadicFor.rds")
summary(fit_dyadicFor_c)

brms::conditional_effects(fit_dyadicFor_c)

tidybayes::get_variables(fit_dyadicFor_c)
c <- as.matrix(fit_dyadicFor_c)
bayesplot::mcmc_areas(c,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()

###
fit_dyadicMRF_c <- bison_brm(
  bison(edge_weight(node_1, node_2)) ~ Fpdiff,
  fit_edgeMRF_c,
  dfDyads_MRF_count,
  num_draws = 10, # Small sample size for demonstration purposes
  refresh = 0)
# saveRDS(fit_dyadicMRF, file = "./data/processed/fit_dyadicMRF.rds")
summary(fit_dyadicMRF_c)

brms::conditional_effects(fit_dyadicMRF_c)

d <- as.matrix(fit_dyadicMRF_c)
bayesplot::mcmc_areas(d,
                          pars = c("b_Intercept", "b_Fpdiff")) +
  theme_test()
