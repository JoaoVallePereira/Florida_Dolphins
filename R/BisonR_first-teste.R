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
# saveRDS(df4, file = "./data/processed/DF_dyads.rds")


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

est_inla <- summary(fit_inla)$fixed[1:6636, 1]
est_stan <- summary(fit)$summary[1:6636, 1]

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
# saveRDS(logit_p_samples, file = "./data/processed/logit_p_samples.rds")



p_samples <- plogis(logit_p_samples)
adj_tensor <- array(0, c(427, 427, num_samples))
for (dyad_id in 1:model_data$M) {
  dyad_row <- df4[df4$dyad_id == dyad_id, ]
  adj_tensor[dyad_row$node_1, dyad_row$node_2, ] <- p_samples[, dyad_id]
}
adj_tensor[, , 1] # Print the first sample of the posterior distribution over adjacency matrices



# Calculate lower, median, and upper quantiles of edge weights. Lower and upper give credible intervals.
adj_quantiles <- apply(adj_tensor, c(1, 2), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
adj_lower <- adj_quantiles[1, , ]
adj_mid <- adj_quantiles[2, , ]
adj_upper <- adj_quantiles[3, , ]


# Calculate width of credible intervals.
adj_range <- adj_upper - adj_lower
adj_range[is.nan(adj_range)] <- 0

# Generate two igraph objects, one form the median and one from the standardised width.
g_mid <- graph_from_adjacency_matrix(adj_mid, mode = "undirected", weighted=TRUE)
g_range <- graph_from_adjacency_matrix(adj_range, mode = "undirected", weighted=TRUE)

# Plot the median graph first and then the standardised width graph to show uncertainty over edges.
coords <- igraph::layout_nicely(g_mid)

plot(simplify(g_mid, remove.multiple = F), edge.width = 3 * E(g_mid)$weight, edge.color="black",  layout=coords, vertex.size = 5, label.cex = 5)

plot(g_mid, edge.width=20 * E(g_range)$weight, edge.color=rgb(0, 0, 0, 0.25), 
     vertex.label=c("Rey", "Leia", "Obi-Wan", "Luke", "C-3PO", "BB-8", "R2-D2", "D-O"), 
     vertex.label.dist=4, vertex.label.color="black", layout=coords, add=TRUE)





extract_metric(fit_inla, "node_eigen")



























