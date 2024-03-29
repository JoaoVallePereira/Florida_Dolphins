# DOLPHIN GROUPS PRE-PROCESSING -----------------------------------------------------------------


##########################################
# Florida Bay: 2002/03/04/05
#
# Dolphin groups pre-processing
# Old sheet from Leigh
#
# Joao do Valle-Pereira - start Feb 2024
##########################################


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

#### DATA BY CONTEXT ####

# All data ----
rawGBI <- asnipe::get_group_by_individual(data.raw$identities, 
                                  data_format = "groups")
rownames(rawGBI) <- paste("G",1:nrow(rawGBI), sep = "")
rownames(rawINFO) <- paste("G",1:nrow(rawGBI), sep = "")

# Filter Numrec 5% ----

GBI_filtered <- rawGBI[,which(colSums(rawGBI) > nrow(rawGBI)*0.05)]

GBI_filtered <- GBI_filtered[which(rowSums(GBI_filtered) > 0),]

GBI_fullSET <- cbind(GBI_filtered, rawINFO[rownames(rawINFO) %in% rownames(GBI_filtered),])


# Subset by home range data ---- #####################################################
idsSUBSET <- colnames(rawGBI)[which(colnames(rawGBI) %in% 
                                      colnames(homerange.matrix))]

# filter Numrec 5% ----
GBI_fullSET <- rawGBI[,which(colnames(rawGBI) %in% idsSUBSET)]
GBI_fullSET <- GBI_fullSET[which(rowSums(GBI_fullSET) > 0),
                           which(colSums(GBI_fullSET) > nrow(rawGBI)*0.05)]

GBI_fullSET <- cbind(rawGBI, ############################################### GBI_fullSET
                     rawINFO[which(rownames(rawINFO) %in% 
                                     rownames(rawGBI)),])

GBI_fullSET <- rawGBI %>% left_join(rawINFO, by = row.names())
#####################################################################################

# 1. All Behavior ####
gbi_ALL <- GBI_fullSET %>% 
  dplyr::select(-date, -foraging, -tactic, -zone)

# 2. filter FORAGING + MRF ####
gbi_MRF <- GBI_fullSET %>% 
  dplyr::filter(foraging == "Yes" & tactic == "MRF") %>% 
  dplyr::select(-date, -foraging, -tactic, -zone)

# 3. filter FORAGING + Herd ####
gbi_Herd <- GBI_fullSET %>% 
  dplyr::filter(foraging == "Yes" & tactic == "Herd") %>% 
  dplyr::select(-date, -foraging, -tactic, -zone)

# 4. filter FORAGING + Poke ####
gbi_Poke <- GBI_fullSET %>% 
  dplyr::filter(foraging == "Yes" & tactic == "Poke") %>% 
  dplyr::select(-date, -foraging, -tactic, -zone)

# 5. filter FORAGING + DD ####
gbi_DD <- GBI_fullSET %>% 
  dplyr::filter(foraging == "Yes" & tactic == "DD") %>% 
  dplyr::select(-date, -foraging, -tactic, -zone)

# 6. filter FORAGING
gbi_FORAG <- GBI_fullSET %>% 
  dplyr::filter(foraging == "Yes") %>% 
  dplyr::select(-date, -foraging, -tactic, -zone)

# 7. filter NON-FORAGING ####
gbi_NO_forage <- GBI_fullSET %>% 
  dplyr::filter(foraging == "No") %>% 
  dplyr::select(-date, -foraging, -tactic, -zone)

# List with all gbi by context ----
GBI_by_context <- list(gbi_ALL,
                       gbi_MRF,
                       gbi_Herd,
                       gbi_Poke,
                       gbi_DD,
                       gbi_FORAG,
                       gbi_NO_forage)

namesCONTEXT <- c("All_Behavior",
                  "Mud_Ring",
                  "Herd_&_Chase",
                  "Poke_under_seagrass",
                  "Deep_Dive",
                  "Foraging",
                  "Non_Foraging")

names(GBI_by_context) <- namesCONTEXT

#### Group characteristics ====

descriptive.groups <- data.frame(
  Context = namesCONTEXT,
  N.groups = unlist(lapply(GBI_by_context, nrow)),
  Mean.SD = paste0(
    format(lapply(lapply(GBI_by_context, rowSums), mean), 
           digits = 3), " ± (",
    format(lapply(lapply(GBI_by_context, rowSums), sd),
           digits = 2), ")"
  ),
  Min.Max = paste0(
    lapply(lapply(GBI_by_context, rowSums), min),
    "-", lapply(lapply(GBI_by_context, rowSums), max))
)
pander::pander(descriptive.groups)

GBI_by_context
# RANDOM GBI 
Npermute_GBI <- 25

list_RANDOM_GBI <- list()

for(i in 1:length(GBI_by_context)){
  list_RANDOM_GBI[[i]] <- null_checkerboard(GBI_by_context[[i]],
                                            iter = Npermute_GBI)
}


#### SRI + DENMAT ###
listALL <- GBI_by_context
list_SRI_real <- list()
list_INMAT_real <- list()
list_DENMAT_real <- list()
list_SD_SRI_real <- list()

for(i in 1:length(listALL)){
  
  list_SRI_real[[i]] <- SRI(listALL[[i]])$SRI %>% 
    order_matrix() %>% 
    as.matrix()
  
  list_INMAT_real[[i]] <- SRI(listALL[[i]])$SRI.numerator %>% 
    order_matrix() %>% 
    as.matrix()
  
  list_DENMAT_real[[i]] <- SRI(listALL[[i]])$SRI.denominator %>% 
    order_matrix() %>% 
    as.matrix()
  
  list_SD_SRI_real[[i]] <-sd(matrix_unfold(list_SRI_real[[i]]))
  
}

str(list_SRI_real)



#### RANDOM SRI + INMAT + DENMAT ####
list_RANDOM_SRI <- list()
list_RANDOM_INMAT <- list()
list_RANDOM_DENMAT <- list()

for(i in 1:length(list_RANDOM_GBI)){ 
  
  list_SRI <- list()
  list_inmat <- list()
  list_denmat <- list()
  
  for(j in 6:Npermute_GBI){ # Remove the first 5000 random GBI
    tmp.list <- SRI(list_RANDOM_GBI[[i]]$perm[j][[1]])
    
    list_SRI[[j]] <- order_matrix(tmp.list$SRI)
    list_inmat[[j]] <- order_matrix(tmp.list$SRI.numerator)
    list_denmat[[j]] <- order_matrix(tmp.list$SRI.denominator)
  }
  
  list_RANDOM_SRI[[i]] <- list_SRI
  list_RANDOM_INMAT[[i]] <- list_inmat
  list_RANDOM_DENMAT[[i]] <- list_denmat
  
}

names(list_RANDOM_SRI) <- namesCONTEXT
names(list_RANDOM_INMAT) <- namesCONTEXT
names(list_RANDOM_DENMAT) <- namesCONTEXT

# Remove empty levels of the list
for(i in 1:length(list_RANDOM_SRI)){
  list_RANDOM_SRI[[i]] <- list_RANDOM_SRI[[i]][lapply(list_RANDOM_SRI[[i]],length)>0]
  list_RANDOM_INMAT[[i]] <- list_RANDOM_INMAT[[i]][lapply(list_RANDOM_INMAT[[i]],length)>0]
  list_RANDOM_DENMAT[[i]] <- list_RANDOM_DENMAT[[i]][lapply(list_RANDOM_DENMAT[[i]],length)>0]
}

# set number of permutations
Npermute <- 20

#### Modularity SRI Real ----
modularity_SRI_real <- vector()
modularity_size_SRI_real <- list()
modularity_memb_SRI_real <- list()

for(i in 1:length(list_SRI_real)){
  
  tmp1 <- igraph::graph.adjacency(list_SRI_real[[i]], 
                          mode="undirected",
                          weighted=TRUE,
                          diag=FALSE)
  
  tmpmod <- igraph::cluster_leading_eigen(graph = tmp1,
                                  weights = igraph::E(tmp1)$weight)
  
  modularity_SRI_real[[i]] <- igraph::modularity(tmpmod)
  modularity_size_SRI_real[[i]] <- table(tmpmod$membership)
  modularity_memb_SRI_real[[i]] <- data.frame(Id=row.names(list_SRI_real[[i]]), module=tmpmod$membership)
}
names(modularity_size_SRI_real) <- names(modularity_memb_SRI_real) <- namesCONTEXT

#### Modularity SRI Random ----
modularity_SRI_random <- NULL
listMOD_SRI_random <- list()

for(i in 1:length(list_SRI_real)){
  for(j in 1:Npermute){
    
    tmp.graph <- igraph::graph.adjacency(as.matrix(list_RANDOM_SRI[[i]][[j]]),
                                 mode = "undirected",
                                 diag = FALSE,
                                 weighted = TRUE)
    #tmp.graph <- giant.component(tmp.graph)
    
    tmpmod2 <-  igraph::cluster_leading_eigen(graph = tmp.graph,
                                     weights =  igraph::E(tmp.graph)$weight,
                                     options=list(maxiter=1000000))
    
    modularity_SRI_random[j] <-  igraph::modularity(tmpmod2)
    
  }
  
  listMOD_SRI_random[[i]] <- modularity_SRI_random
}

#### SD random SRI ----
SDSRI_random <- list()

for(i in 1:length(list_RANDOM_SRI)){
  
  sdtmp <- vector()
  
  for(k in 1:Npermute){
    sdtmp[k] <- sd(matrix_unfold(list_RANDOM_SRI[[i]][[k]]))
  }
  
  SDSRI_random[[i]] <- sdtmp
}

#### Assortativity SRI Random #### 

listASSORT_FP_SRI_random <- list()
listASSORT_HR_SRI_random <- list()

for(i in 1:length(list_RANDOM_SRI)){
  
  SRI_assortFP_random <- vector()
  SRI_assortHR_random <- vector()
  
  for(j in 1:Npermute){
    
    SRI.rand.tmp.fp <- assortnet::assortment.continuous(list_RANDOM_SRI[[i]][[j]], 
                                             vertex_values = ord.df$Freq_Int,
                                             weighted = TRUE, 
                                             SE = FALSE)
    SRI_assortFP_random[j] <- SRI.rand.tmp.fp[[1]]
    
    SRI.rand.tmp.hr <- assortnet::assortment.continuous(list_RANDOM_SRI[[i]][[j]],
                                             vertex_values = ord.df$HR,
                                             weighted = TRUE, 
                                             SE = FALSE)
    SRI_assortHR_random[j] <- SRI.rand.tmp.hr[[1]]
    
  }
  
  listASSORT_FP_SRI_random[[i]] <- SRI_assortFP_random
  listASSORT_HR_SRI_random[[i]] <- SRI_assortHR_random
  
}

#### Assortativity SRI Obs ####

listASSORT_FP_SRI_real <- list()
listASSORT_HR_SRI_real <- list()

for(i in 1:length(list_SRI_real)){
  
  listASSORT_FP_SRI_real[[i]] <- assortnet::assortment.continuous(list_SRI_real[[i]],
                                                       vertex_values = ord.df$Freq_Int,
                                                       weighted = TRUE,
                                                       SE = FALSE)  
  
  listASSORT_HR_SRI_real[[i]] <- assortnet::assortment.continuous(list_SRI_real[[i]],
                                                       vertex_values = ord.df$HR,
                                                       weighted = TRUE,
                                                       SE = FALSE) 
  
}


# Colorblind palette
color.mod <- c("#E69F00", # orange
               "#56B4E9", # sky blue
               "#009E73", # bluish green
               "#F0E442", # yellow
               "#D55E00", # vermilion
               "#CC79A7", # reddish purple
               "#0072B2", # blue
               "#999999") # grey

# SRI Networks 
par(mfrow = c(4,2), mar = c(0,1,0,1))

layout.net <- list()

for(i in 1:length(list_SRI_real)){
  
  SRInet <- graph.adjacency(as.matrix(list_SRI_real[[i]]),
                            mode = "undirected",
                            weighted = TRUE,
                            diag = FALSE)
  
  mod.SRI <- cluster_leading_eigen(SRInet)
  
  layout.net[[i]] <- igraph::layout.fruchterman.reingold(SRInet)
  
  if(i == 2){
    
    plot.igraph(SRInet,
                layout = layout.net[[i]],
                main = namesCONTEXT[i],
                edge.color = alpha('grey80', 0.9),
                vertex.label = NA,
                # vertex.size = rescale(node.size.fp, 
                                      # r.out = range(5,15)),
                edge.width = E(SRInet)$weight * 10,
                vertex.color = "grey50")
    
  } else {
    
    plot.igraph(SRInet,
                layout = layout.net[[i]],
                main = namesCONTEXT[i],
                edge.color = alpha('grey80', 0.9),
                vertex.label = NA,
                # vertex.size = rescale(node.size.fp, 
                                      # r.out = range(5,15)),
                edge.width = E(SRInet)$weight * 10,
                vertex.color = color.mod[mod.SRI$membership]) 
  }
}

plot.igraph(SRInet,
            layout = layout.net[["All_Behavior"]],
            main = namesCONTEXT["All_Behavior"],
            edge.color = alpha('grey80', 0.9),
            vertex.label = NA,
            # vertex.size = rescale(node.size.fp, 
            # r.out = range(5,15)),
            edge.width = E(SRInet)$weight * 10,
            vertex.color = color.mod[mod.SRI$membership]) 


# All Behavior network


tmp1 <- igraph::graph.adjacency(list_SRI_real[[i]], 
                                mode="undirected",
                                weighted=TRUE,
                                diag=FALSE)

# Get vertex color

dolphin_per_tactic <- GBI_fullSET %>% 
  dplyr::select(-date, -foraging, -zone) %>% 
  reshape2::melt(value.name = "tactic") %>% 
  dplyr::rename(y = 3, id = variable) %>% 
  dplyr::group_by(id, tactic) %>% 
  dplyr::summarise(count_tactic = sum(y))

most_common_tactic_id <- dolphin_per_tactic %>% 
  dplyr::group_by(id) %>% 
  dplyr::slice(which.max(count_tactic)) %>% 
  arrange(desc(count_tactic)) %>% 
  data.frame()


rede_full <- igraph::graph.adjacency(list_SRI_real[[1]],
                                     mode="undirected",
                                     diag=FALSE,
                                     weighted=TRUE)
plot(rede_full,
     edge.arrow.size=.5,
     vertex.color="gold",
     vertex.size=5, 
     vertex.frame.color="gray",
     vertex.label.cex=0.6, 
     vertex.label.dist=2,
     edge.curved=0.2)














