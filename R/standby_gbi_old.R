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