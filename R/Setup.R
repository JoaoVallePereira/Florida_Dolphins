# Load packages
if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)}
if(!require(asnipe)){install.packages('asnipe'); library(asnipe)}
if(!require(pander)){install.packages('pander'); library(pander)}
if(!require(vegan)){install.packages('vegan'); library(vegan)}
if(!require(igraph)){install.packages('igraph'); library(igraph)}
if(!require(assortnet)){install.packages('assortnet'); library(assortnet)}

# remotes::install_github("JHart96/bisonR")
library(bisonR) # CHECK JHart96/bisonR GITHUB to install CmdStanR

if(!require(INLA)){install.packages('INLA'); library(INLA)}



# Load functions
convert_gbi_to_bison <- function(gbi, group_properties=NULL, individual_properties=NULL, individual_constraints=NULL) {
  # Define empty bisonR dataframe
  df = data.frame(node_1=numeric(), node_2=numeric(), event=numeric(), group_id=numeric())
  
  # Set group and individual property columns if defined
  if (!is.null(group_properties)) {
    df$group_property <- numeric()
  }
  if (!is.null(individual_properties)) {
    df$node_1_property <- numeric()
    df$node_2_property <- numeric()
  }
  
  # If there are no individual constraints, set the constraints vector to the same value.
  if (is.null(individual_constraints)) {
    individual_constraints <- rep(0, ncol(gbi))
  }
  
  # If there are no node names on the GBI, define them numerically.
  node_names <- colnames(gbi)
  if (is.null(colnames(gbi))) {
    node_names <- 1:ncol(gbi)
  }
  
  # For each unique pair of individuals, compare their columns vector-wise
  for (i in 1:ncol(gbi)) {
    for (j in 1:ncol(gbi)) {
      if (i < j && individual_constraints[i] == individual_constraints[j]) {
        # Both individuals associated
        x <- gbi[, i] * gbi[, j]
        # At least one individual was present
        d <- apply(gbi[, c(i, j)], 1, max)
        # Get associations and group IDs from those where at least one individual was seen
        event = x[d == 1]
        group_id = (1:nrow(gbi))[d == 1]
        
        # Create empty data frame for these events
        df_new <- list(node_1=i, node_2=j, event=event, group_id=group_id)
        
        # Set group and individual properties in the new dataframe
        if (!is.null(group_properties)) {
          df_new$group_property <- group_properties[group_id]
        }
        
        if (!is.null(individual_properties)) {
          df_new$node_1_property <- individual_properties[i]
          df_new$node_2_property <- individual_properties[j]
        }
        
        # Add new dataframe to existing
        df <- dplyr::bind_rows(df, df_new)
      }
    }
  }
  df
}

