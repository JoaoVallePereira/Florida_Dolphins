## Prepare dataframe for Stan model
df <- df %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))

## Prepare data list for model
model_data <- list(
  N=nrow(df), # Number of observations
  M=length(unique(df$dyad_id)), # Number of dyads
  G=nrow(obs), # Number of groupings
  dyad_ids=df$dyad_id, # Vector of dyad IDs corresponding to each observation
  group_ids=df$obs_id, # Vector of group IDs corresponding to each observation
  event=df$social_event # Vector of binary values (0/1, presence/absence) corresponding to each observation
)


model <- rstan::stan_model("./man/models/group_model.stan")

fit <- rstan::sampling(model, model_data, cores=4)


# Do a quick visualisation of the parameter values.
plot(fit, pars = "logit_p")

####################
df_inla <- df
df_inla$dyad_id <- as.factor(df$dyad_id)
df_inla$obs_id <- as.factor(df$obs_id)

prior.fixed <- list(mean=0, prec=1)
prior.random <- list(prec=list(prior="normal", param=c(0, 1)))

fit_inla <- INLA::inla(social_event ~ 0 + 
                         dyad_id + 
                         INLA::f(obs_id, model = "iid", hyper = prior.random), 
                       family = "binomial", 
                       data = df_inla,
                       control.fixed = prior.fixed,
                       control.compute = list(config = TRUE)
)

summary(fit_inla)$fixed
##################################################################