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
  tidyr::drop_na(IDs)
head(data.raw)

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

## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
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

# Get dyads with observations information
dfDyads_full2 <- df %>% 
  dplyr::left_join(rawINFO, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
# saveRDS(df4, file = "./data/processed/DF_dyads.rds")





