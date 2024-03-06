library(bisonR)

sim_data <- simulate_bison_model("binary", aggregated = TRUE)
df <- sim_data$df_sim

priors <- get_default_priors("binary")

fit_edge <- bison_model(
  (event | duration) ~ dyad(node_1_id, node_2_id), 
  data=df, 
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


# Behavior, tactic and zone status ----
rawINFO <- data.raw %>% 
  dplyr::select(date, foraging, tactic, zone)

dfINFO <- data.raw %>% 
  dplyr::select(date, foraging, tactic, zone, IDs) 
rownames(dfINFO) <- paste("G",1:nrow(rawGBI), sep = "")

# Long data frame with groups column

dfINFO_long <- dfINFO %>% 
  dplyr::mutate(group = rownames(.)) %>% 
  tidyr::separate_rows(IDs, sep = " ") %>%
  data.frame()
group_size <- dfINFO_long %>% 
  dplyr::group_by(group) %>% 
  dplyr::count()

dfINFO_long <- dfINFO_long %>% 
  dplyr::left_join(group_size, by = "group") %>% 
  dplyr::rename(group_size = n)


#### Get dyads ####

dfDyads_full <- dfINFO_long %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(dyads = ifelse(n() >= 2, list(combn(IDs, 2, simplify = FALSE)), NA)) %>%
  dplyr::filter(!is.na(dyads)) %>% 
  tidyr::unnest(dyads) %>% 
  tidyr::separate(col = dyads, into = c("x", "node_1_id", "node_2_id")) %>% 
  dplyr::group_by(group) %>%
  dplyr::distinct(node_1_id, node_2_id, .keep_all = TRUE) %>%
  dplyr::select(-IDs, -x) %>% 
  data.frame() 

# Fit first model

priors <- bisonR::get_default_priors("binary_conjugate")
priors

fit_edge <- bisonR::bison_model(
  (event | duration) ~ dyad(node_1_id, node_2_id), 
  data= dfDyads_full, 
  model_type = "binary",
  priors = priors
)

summary(fit_edge)
plot_network(fit_edge)



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

## Convert to long format (dyad)
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

# Behavior, tactic and zone status ----
rawINFO <- dataForGBI %>% 
  dplyr::select(date, foraging, tactic, zone, obs_id) %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))


dfDyads_full2 <- df %>% dplyr::left_join(rawINFO, by = "obs_id")








































