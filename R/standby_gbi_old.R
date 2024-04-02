dfINFO <- data.raw %>% 
  dplyr::select(date, foraging, tactic, zone, IDs) 

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
head(dfINFO_long)

dfCount <- dfINFO_long %>% 
  dplyr::group_by(IDs) %>% 
  dplyr::count() %>% 
  dplyr::filter(n > 1)
dfIDs <- dfCount$IDs

dfINFO_long_filter <- dfINFO_long %>% 
  dplyr::filter(IDs %in% dfIDs) 





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



head(dfDyads_full)
