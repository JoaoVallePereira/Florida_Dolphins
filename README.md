# README #
<img src="man/Figures/FloridaDolphinsLogo.png" align="right" width="170px"/>

ORIGINAL DATA FOLDER IS HIDDEN FOR THE MOMENT!!!!!!!!!!

If you are reading this message that means that the available data to run the codes is a randomized subset of the original data, it has the same structure with fewer individuals and sampled nodes

## Author, maintainer and contact

**João V. S. do Valle-Pereira**[![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](http://orcid.org/0000-0002-1880-9495): joao.vallepereira@oregonstate.edu       

   
*Lab For Animal Behavioral Interaction Research in the Ocean, Oregon State University, Department of Fisheries and Wildlife, Newport, Oregon, USA.*

--------------------------------------
## Description

This repository includes the R script to reproduce the analyses and the supplementary material of the manuscript:     
## The Florida Bay dolphins prefer to stay with who eats the same way 
Valle-Pereira, J.V.S [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](http://orcid.org/0000-0002-1880-9495); Cantor, M. [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](http://orcid.org/0000-0002-0019-5106), Torres, L. *In prep*.

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
├── dataAW/processedAW        # contains the data used in the manuscript
├── R/                        # contains the code to setup run the analyses
├── output/                   # stores tables and figures # NOTHING FOR NOW
└── man/                      # contains figures and models to either implement on the analysis or in the GitHub layout
```
**Note:** The workflow to run at the time of this message is:  `Setup.R` &rarr; `GetDyads.R` &rarr; `GetNetwork.R`. Remember that the `GetDyads.R` won't be working for now

--------------------------------------

**This is a snippet from https://github.com/JHart96/bisonR to install the bisonR package:**
## Installation

### Installing CmdStanR

The bisonR package is written in R, but uses the Stan programming language to fit Bayesian models. Stan is a separate program, and interfaces with bisonR using an R package called cmdstanR. Stan and cmdstanR are installed in a different way to standard R packages, so require a specific series of installation steps. 

On Windows, before proceeding you may need to install the version of Rtools appropriate for your version of R. Rtools can be found here: https://cran.r-project.org/bin/windows/Rtools/.

The full instructions to install cmdstanR can be found at https://mc-stan.org/cmdstanr/. We've found that the following steps often work, but depending on your operating system and version of R, the process may be more involved.

```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
install_cmdstan() # On networked PCs, specify a local directory here with the argument dir=path_to_local_directory
```

### Installing bisonR

bisonR isn't currently on CRAN, but it can be installed from GitHub. To do this, make sure you have the `remotes` package installed. Then run the following command:

```r
remotes::install_github("JHart96/bisonR")
```

**Check out his repository for more information**

**Also check out his *Getting Started* page here: https://jhart96.github.io/bisonR/articles/getting_started.html.**












