brmALL <- readRDS("./data/processed/fit_dyadicALL.rds")
brmNoFor <- readRDS("./data/processed/dyadicNoFor.rds")
brmFor <- readRDS("./data/processed/fit_dyadicFor.rds")
brmMRF <-readRDS("./data/processed/fit_dyadicMRF.rds")


###########
plot(brmALL)
summary(brmALL)
tidybayes::get_variables(brmALL)
a <- as.matrix(brmALL)
bayesplot::mcmc_areas(a,
                      pars = c("b_Intercept", "b_Fpdiff"),
                      prob = 0.8) 
###########
plot(brmNoFor)
summary(brmNoFor)
tidybayes::get_variables(brmNoFor)
b <- as.matrix(brmNoFor)
bayesplot::mcmc_areas(b,
                      pars = c("b_Intercept", "b_Fpdiff"),
                      prob = 0.8) 

###########
plot(brmFor)
summary(brmFor)
tidybayes::get_variables(brmFor)
c <- as.matrix(brmFor)
bayesplot::mcmc_areas(c,
                      pars = c("b_Intercept", "b_Fpdiff"),
                      prob = 0.8) 

###########
plot(brmMRF)
summary(brmMRF)
tidybayes::get_variables(brmMRF)
d <- as.matrix(brmMRF)
bayesplot::mcmc_areas(d,
                      pars = c("b_Intercept", "b_Fpdiff"),
                      prob = 0.8) 
###############
