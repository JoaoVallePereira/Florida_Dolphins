fit_edgeALL <- readRDS("./data/processed/fit_edgeALL.rds")
fit_edgeNoFor <- readRDS("./data/processed/fit_edgeNoFor.rds")
fit_edgeFor <- readRDS("./data/processed/fit_edgeFor.rds")
fit_edgeMRF <-readRDS("./data/processed/fit_edgeMRF.rds")


##
fit_mixtureALL <- bisonR::bison_mixture(fit_edgeALL, num_components=5, verbose=FALSE)
summary(fit_mixtureALL)

##
fit_mixtureNoFor <- bisonR::bison_mixture(fit_edgeNoFor, num_components=5, verbose=FALSE)
summary(fit_mixtureNoFor)

##
fit_mixtureFor <- bisonR::bison_mixture(fit_edgeFor, num_components=5, verbose=FALSE)
summary(fit_mixtureFor)

##
fit_mixtureMRF <- bisonR::bison_mixture(fit_edgeMRF, num_components=5, verbose=FALSE)
summary(fit_mixtureMRF)

##
saveRDS(fit_mixtureALL, file = "./data/processed/fit_mixtureALL.rds")
saveRDS(fit_mixtureNoFor, file = "./data/processed/fit_mixtureNoFor.rds")
saveRDS(fit_mixtureFor, file = "./data/processed/fit_mixtureFor.rds")
saveRDS(fit_mixtureMRF, file = "./data/processed/fit_mixtureMRF.rds")




##
fit_mixtureALL_c <- bisonR::bison_mixture(fit_edgeall_c, num_components=5, verbose=FALSE)
summary(fit_mixtureALL_c)

##
fit_mixtureNoFor_c <- bisonR::bison_mixture(fit_edgeNoFor_c, num_components=5, verbose=FALSE)
summary(fit_mixtureNoFor_c)

##
fit_mixtureFor_c <- bisonR::bison_mixture(fit_edgeFor_c, num_components=5, verbose=FALSE)
summary(fit_mixtureFor_c)

##
fit_mixtureMRF_c <- bisonR::bison_mixture(fit_edgeMRF_c, num_components=5, verbose=FALSE)
summary(fit_mixtureMRF_c)

