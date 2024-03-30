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

dfINFO <- data.raw %>% 
  dplyr::select(date, foraging, tactic, zone, IDs) 

# Long data frame with groups column

dfINFO_long <- dfINFO %>% 
  dplyr::mutate(group = rownames(.)) %>% 
  tidyr::separate_rows(IDs, sep = " ") %>%
  data.frame()
dfGroupSize <- dfINFO_long %>% 
  dplyr::group_by(group) %>% 
  dplyr::count()

dfINFO_long <- dfINFO_long %>% 
  dplyr::left_join(dfGroupSize, by = "group") %>% 
  dplyr::filter(n > 1)
  
head(dfINFO_long)

# Filter IDs > 1
dfCount <- dfINFO_long %>% 
  dplyr::group_by(IDs) %>% 
  dplyr::count() %>% 
  dplyr::filter(n > 1)
dfIDs <- dfCount$IDs

dfINFO_long_filter <- dfINFO_long %>% 
  dplyr::filter(IDs %in% dfIDs)

dfIdentities <- dfINFO_long_filter %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarise(identities = paste(IDs, collapse=" ")) %>% 
  dplyr::ungroup()

###############################################
# Get GBI all ----

dataForGBI <- data.raw %>% 
  dplyr::group_by(date, daily_sighting) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  # dplyr::filter(group_size > 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(obs_id = rownames(.))

rawGBI <- asnipe::get_group_by_individual(dataForGBI$identities, 
                                          data_format = "groups")  
# rawGBI <- rawGBI[,colSums(rawGBI) > 1]  
obsALL <- rawGBI


# Get GBI no forag----

dataForGBI_Nofor <- data.raw %>% 
  dplyr::filter(foraging == "No") %>% 
  dplyr::group_by(date, daily_sighting) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  # dplyr::filter(group_size > 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(obs_id = rownames(.))

Nofor_GBI <- asnipe::get_group_by_individual(dataForGBI_Nofor$identities, 
                                          data_format = "groups")
# Nofor_GBI <- Nofor_GBI[,colSums(Nofor_GBI) > 1]  
obsNoFor <- Nofor_GBI


# Get GBI forag----

dataForGBI_For <- data.raw %>% 
  dplyr::filter(foraging == "Yes" |
                foraging == "Prob") %>% 
  dplyr::group_by(date, daily_sighting) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  # dplyr::filter(group_size > 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(obs_id = rownames(.))

For_GBI <- asnipe::get_group_by_individual(dataForGBI_For$identities, 
                                             data_format = "groups")
# For_GBI <- For_GBI[,colSums(For_GBI) > 1]  
obsFor <- For_GBI

# Get GBI MRF----

dataForGBI_MRF <- data.raw %>% 
  dplyr::filter(tactic == "MRF") %>% 
  dplyr::group_by(date, daily_sighting) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  # dplyr::filter(group_size > 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(obs_id = rownames(.))

MRF_GBI <- asnipe::get_group_by_individual(dataForGBI_MRF$identities, 
                                           data_format = "groups")
# MRF_GBI <- MRF_GBI[,colSums(MRF_GBI) > 1]  
obsMRF <- MRF_GBI



## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
# obs <- obsALL
obs   <- obsNoFor
# obs <- obsFor
# obs <- obsMRF

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
rawINFO_Nofor <- dataForGBI_Nofor %>% 
  dplyr::select(date, foraging, tactic, zone, obs_id) %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

# Behavior, tactic and zone status ----
rawINFO_For <- dataForGBI_For %>% 
  dplyr::select(date, foraging, tactic, zone, obs_id) %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

# Behavior, tactic and zone status ----
rawINFO_MRF <- dataForGBI_MRF %>% 
  dplyr::select(date, foraging, tactic, zone, obs_id) %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))


# Get dyads with observations information
dfDyads_full <- df %>% 
  dplyr::left_join(rawINFO, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
# saveRDS(df4, file = "./data/processed/DF_dyads.rds")


# Get dyads with observations information
dfDyads_Nofor <- df %>% 
  dplyr::left_join(rawINFO_Nofor, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
  # saveRDS(dfDyads_Nofor, file = "./data/processed/DF_dyadsNofor.rds")


# Get dyads with observations information
dfDyads_For <- df %>% 
  dplyr::left_join(rawINFO_For, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
 # saveRDS(dfDyads_For, file = "./data/processed/DF_dyadsFor.rds")

# Get dyads with observations information
dfDyads_MRF <- df %>% 
  dplyr::left_join(rawINFO_MRF, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
# saveRDS(dfDyads_MRF, file = "./data/processed/DF_dyadsMRF.rds")

