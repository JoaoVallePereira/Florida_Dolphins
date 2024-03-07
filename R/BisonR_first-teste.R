library(bisonR)

sim_data <- simulate_bison_model("binary", aggregated = TRUE)
df3 <- sim_data$df_sim

priors <- get_default_priors("binary")

fit_edge <- bison_model(
  (event | duration) ~ dyad(node_1_id, node_2_id), 
  data=df3, 
  model_type="binary",
  priors=priors
)

summary(fit_edge)
plot_network(fit_edge)



sim_data2 <- simulate_bison_model("binary", aggregated = FALSE)
df2 <- sim_data2$df_sim

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
                tactic = ifelse(tactic == "UK", "DD", tactic)) %>% ############################################## CHECK WITH LEIGH 
  tidyr::drop_na(IDs)
head(data.raw)


###########

# Get GBI ----

dataForGBI <- data.raw %>% 
  dplyr::group_by(date, daily_sighting) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  dplyr::filter(group_size > 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(obs_id = rownames(.))

rawGBI <- asnipe::get_group_by_individual(dataForGBI$identities, 
                                          data_format = "groups")

obs <- rawGBI

## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!!
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
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
# saveRDS(df, file = "./data/processed/DyadLong.rds")
df <- readRDS(file = "./data/processed/DyadLong.rds")

# Behavior, tactic and zone status ----
rawINFO <- dataForGBI %>% 
  dplyr::select(date, foraging, tactic, zone, obs_id) %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))


dfDyads_full2 <- df %>% 
  dplyr::left_join(rawINFO, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>% 
  dplyr::filter(social_event == 1 |
                social_event == 2)


# 
df4 <- dfDyads_full2 %>%
  group_by(node_1, node_2) %>%
  mutate(dyad_id=cur_group_id()) %>%
  mutate(obs_id=as.integer(obs_id))

model_data <- list(
  N=nrow(df4), # Number of observations
  M=length(unique(df4$dyad_id)), # Number of dyads
  G=nrow(obs), # Number of groupings
  dyad_ids=df4$dyad_id, # Vector of dyad IDs corresponding to each observation
  group_ids=df4$obs_id, # Vector of group IDs corresponding to each observation
  event=df4$social_event # Vector of binary values (0/1, presence/absence) corresponding to each observation
)

model <- rstan::stan_model("./man/models/group_model.stan")

fit <- readRDS(file = "./data/processed/fit.rds")
# fit <- rstan::sampling(model, model_data, cores=4)
summary(fit)
# saveRDS(fit, file = "./data/processed/fit.rds")

df_inla <- df4
df_inla$dyad_id <- as.factor(df4$dyad_id)
df_inla$obs_id <- as.factor(df4$obs_id)

prior.fixed <- list(mean=0, prec=1)
prior.random <- list(prec=list(prior="normal", param=c(0, 1)))

fit_inla <- INLA::inla(social_event ~ 0 + dyad_id + f(obs_id, model="iid", hyper=prior.random), 
                 family = "binomial", 
                 data = df_inla,
                 control.fixed = prior.fixed,
                 control.compute = list(config = TRUE))
saveRDS(fit_inla, file = "./data/processed/fit_inla.rds")

summary(fit_inla)$fixed

est_inla <- summary(fit_inla)$fixed[1:28, 1]
est_stan <- summary(fit)$summary[1:28, 1]

plot(est_stan, est_inla)



##########

# How many samples to draw from posteriors. 4000 to match Stan, but samples are really cheap here so no reason not to increase this.
num_samples <- 4000
logit_p_samples <- matrix(0, nrow=num_samples, ncol=model_data$M)

param_names <- names(fit_inla$marginals.fixed)
for (i in 1:model_data$M) {
  param_zmarg <- inla.zmarginal(fit_inla$marginals.fixed[[param_names[i]]], silent=TRUE)
  
  mu <- param_zmarg[[1]]
  sigma <- param_zmarg[[2]]
  
  logit_p_samples[, i] <- rnorm(num_samples, mu, sigma)
}
logit_p_samples





































