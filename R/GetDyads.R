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
  dplyr::select(date, foraging, tactic, zone, IDs, lat, long) 

# Long data frame with groups column

dfINFO_long <- dfINFO %>% 
  dplyr::mutate(group = rownames(.)) %>% 
  tidyr::separate_rows(IDs, sep = " ") %>%
  data.frame()

dfGroupSize <- dfINFO_long %>% 
  dplyr::group_by(group) %>% 
  dplyr::count()

dfINFO_long <- dfINFO_long %>% 
  dplyr::left_join(dfGroupSize, by = "group") 

head(dfINFO_long)

# Filter IDs > 1
dfCount <- dfINFO_long %>% 
  dplyr::group_by(IDs) %>% 
  dplyr::count() %>% 
  dplyr::filter(n > 1)
dfIDs <- dfCount$IDs

dfINFO_long_filter <- dfINFO_long %>% 
  dplyr::filter(IDs %in% dfIDs) %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarize(identities = list(IDs)) %>% 
  dplyr::inner_join(dfINFO_long, by = "group") %>% 
  dplyr::distinct(group, .keep_all = TRUE) %>% 
  dplyr::select(- IDs)

head(dfINFO_long_filter)


###############################################
# Get GBI all ----

dataForGBI <- dfINFO_long_filter %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))
  

rawGBI <- asnipe::get_group_by_individual(dataForGBI$identities, 
                                          data_format = "groups")  
obsALL <- rawGBI


# Get GBI no forag----

dataForGBI_Nofor <- dfINFO_long_filter %>% 
  dplyr::filter(foraging == "No") %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

Nofor_GBI <- asnipe::get_group_by_individual(dataForGBI_Nofor$identities, 
                                          data_format = "groups")
obsNoFor <- Nofor_GBI


# Get GBI forag----

dataForGBI_For <- dfINFO_long_filter %>% 
  dplyr::filter(foraging == "Yes" |
                foraging == "Prob") %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

For_GBI <- asnipe::get_group_by_individual(dataForGBI_For$identities, 
                                             data_format = "groups")
obsFor <- For_GBI

# Get GBI MRF----

dataForGBI_MRF <- dfINFO_long_filter %>% 
  dplyr::filter(tactic == "MRF") %>% 
  dplyr::group_by(group) %>%
  dplyr::mutate(group_size = sum(lengths(identities))) %>% 
  tibble::rownames_to_column('obs_id') %>% 
  dplyr::mutate(obs_id = as.numeric(obs_id))

MRF_GBI <- asnipe::get_group_by_individual(dataForGBI_MRF$identities, 
                                           data_format = "groups")
obsMRF <- MRF_GBI



## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
obs <- obsALL

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

# Get dyads with observations information
dfDyads_full <- df %>%  
  dplyr::left_join(dataForGBI, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
saveRDS(df4, file = "./data/processed/DF_dyads.rds")

## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
obs   <- obsNoFor


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


# Get dyads with observations information
dfDyads_Nofor <- df %>% 
  dplyr::left_join(dataForGBI_Nofor, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
saveRDS(dfDyads_Nofor, file = "./data/processed/DF_dyadsNofor.rds")

## Convert to long format (dyad) TAKES SOME TIME (load rds in the sequence) !!!! ----
df <- data.frame(node_1=numeric(), node_2=numeric(), social_event=numeric(), obs_id=numeric())
obs <- obsFor

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


# Get dyads with observations information
dfDyads_For <- df %>% 
  dplyr::left_join(dataForGBI_For, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
saveRDS(dfDyads_For, file = "./data/processed/DF_dyadsFor.rds")


# Get dyads with observations information
dfDyads_MRF <- df %>% 
  dplyr::left_join(dataForGBI_MRF, by = "obs_id") %>% 
  dplyr::mutate(node_1 = as.factor(node_1),
                node_2 = as.factor(node_2)) %>%
  dplyr::group_by(node_1, node_2) %>%
  dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
  dplyr::mutate(obs_id = as.integer(obs_id))
saveRDS(dfDyads_MRF, file = "./data/processed/DF_dyadsMRF.rds")

