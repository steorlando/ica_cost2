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
               here
)

rm(list = ls())

source(here::here("code","01.import.R")) 
source(here::here("code","03.clean_data.R"))
source(here::here("code","04.labelling.R"))

