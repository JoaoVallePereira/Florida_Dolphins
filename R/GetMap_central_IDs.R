
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


IDs_central <- dfINFO_long %>% 
  dplyr::filter(zone == "Central") %>% 
  dplyr::select(IDs) 
  # dplyr::group_by(IDs) %>% 
  # dplyr::count() %>% 
  # dplyr::filter(n > 1)

IDs_central <- IDs_central$IDs

dfCentral <- dfINFO_long %>% 
  dplyr::filter(IDs %in% IDs_central)

dfNoCentral <- dfINFO_long %>% 
  dplyr::filter(IDs %in% IDs_central)  %>% 
  dplyr::filter(zone != "Central")


dfCentral_other <- dfCentral %>% 
  dplyr::filter(IDs %in% dfNoCentral$IDs) %>% 
  dplyr::arrange(IDs) 

# Load map from google sattelite

ggmap::register_google(key = "AIzaSyDm6vPh_NE7A5gl7d2RPWxljdpgz9X6pIk")

mapFloridaBay <- ggmap::get_map(location = c(dfCentral$long[1], dfCentral$lat[1]), 
                             zoom = 11, 
                             maptype = "satellite",
                             source = "google")
ggmap::ggmap(mapFloridaBay,
             base_layer = ggplot2::ggplot(data = dfCentral_other,
                                          aes(x = long, y = lat, color = IDs))+
               coord_map(xlim=range(dfCentral$long), ylim=range(dfCentral$lat)) ) +
  geom_point( alpha = 1, cex = 3) +
  facet_wrap(~IDs)
  # theme(legend.position = "none")





