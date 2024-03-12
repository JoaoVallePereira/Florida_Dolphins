<img src="man/Figures/FloridaDolphinsLogo.png" align="right" width="200px"/>

# README #

## Author, maintainer and contact

**João V. S. do Valle-Pereira**[![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](http://orcid.org/0000-0002-1880-9495): joao.vallepereira@oregonstate.edu       

   
*Lab For Animal Behavioral Interaction Research in the Ocean, Oregon State University, Department of Fisheries and Wildlife, Newport, Oregon, USA.*

--------------------------------------
## Description

This repository includes the R script to reproduce the analyses and the supplementary material of the manuscript:     
## Florida Dolphins
Resilience of dolphin foraging cultures facing climate and habitat changes

Valle-Pereira, J.V.S [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](http://orcid.org/0000-0002-1880-9495); Cantor, M. [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](http://orcid.org/0000-0002-0019-5106), Torres, L. *In prep*.

ORIGINAL DATA FOLDER IS HIDDEN FOR THE MOMENT!!!!!!!!!!

## If you are reading this message that means that the available data to run the codes is a randomized subset of the original data, it has the same structure with fewer individuals and sampled nodes ##

## Contents

1. Files and directions
    * 1.1. Data and script to reproduce the analysis
    * 1.2. Instructions

### 1.1. Data and script to reproduce the analysis

- `Setup.R`: This file contains the code to install and load the packages required to run the analyses in `GetNetwork.R`.

- `GetNetwork.R`: This file contains the code to run the analyses of the manuscript.

- `GetDyads.R`: This file contains the code to manage, clean, and prepare the data to be run in the `GetNetwork.R` script. If you are here and you are reading this text you won't be able to run this code. It should produced something similar to the fake data frame contains the following information: 

| Variable        | Class              | Description                          |
|-----------------|--------------------|--------------------------------------|
| node_1          | Factor (49 levels) | Dolphin 1 ID                         |
| node_2          | Factor (49 levels) | Dolphin 2 ID                         |
| social_event    | Numeric            | Is dolphin associated?               |
| obs_id          | Integer            | Observation ID                       |
| date            | POSIXct            | Date                                 |
| foraging        | Character          | Is foraging?                         |
| tactic          | Character          | Foraging tactic performed            |
| zone            | Character          | Zone where the individuals were seen |
| dyad_id         | Integer            | Dyad identification                  |
| duration        | Numeric            | Duration of the social event         |

### 1.3. Instructions

Scripts contain relative paths to source functions and load data. Open an R session and set the working directory to the root of the project for better compatibility with relative paths. The tree below show how files were organized in the project folder:

```bash
Florida_Dolphins/
├── dataAW/                   # contains the data used in the manuscript
├── R/                        # contains the code to setup run the analyses
├── output/                   # stores tables and figures # NOTHING FOR NOW
└── man/                      # contains figures and models to either implement on the analysis or in the GitHub layout
```
**Note:** The workflow to run at the time of this message is:  `Setup.R` &rarr; `GetDyads.R` &rarr; `GetNetwork.R`. Remember that the `GetDyads.R` won't be working for now












