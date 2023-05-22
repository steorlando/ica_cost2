#Packages needed for analysis ####
# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(pacman,
               rio, 
               tidyverse, 
               knitr, 
               flextable, 
               officer, 
               magrittr, 
               broom,
               sjlabelled, 
               sjmisc,
               scales,
               gtsummary,
               DataExplorer,
               janitor, 
               here,
               Matching,
               cobalt,
               dplyr,
               tidyverse,
               mice,
               skimr,
               quantreg,
               lubridate
               )

rm(list = ls())


source(here::here("code","01.import.R")) 
source(here::here("code","03.clean_data.R"))
source(here::here("code","04.labelling.R"))
source(here::here("code","05.propensity.R"))
source(here::here("code","06.PS_batteri_siti.R"))


# Save image ####
save.image (file = "code/my_work_space.RData")

