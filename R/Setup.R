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
