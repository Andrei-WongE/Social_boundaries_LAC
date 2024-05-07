## ----------------------------------------------------
##
## Script name: General.R
##
## Project: PhD thesis - Methods chapter
##
## Purpose of script: Review SB methods
##
## Author: Andrei Wong Espejo
##
## Date Created: Fri Mar 22 11:31:31 2024
##
## Date Updated: 
##
## Email: awonge01@student.bbk.ac.uk
## -----------------------------------------------------
##
## Notes: 
##   
##
## ---------------------------

## Load required packages ----
p_boot()

pacman::p_load(here)

install.packages("groundhog") #For some reason does not work with pacman
library(groundhog)
install.packages("remotes")
remotes::install_github("jlegewie/BoundaryDetection")


set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2024-01-20"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("ggplot","sf")

groundhog.library(pkgs, groundhog.day)


## Program Set-up ------------------------------------------------------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
renv::snapshot()
# renv::update()
#set.seed() # See https://www.gigacalculator.com/calculators/random-number-generator.php
theme_set(theme_minimal())

## Runs the following --------------------------------------------------------

# 1. Installs packages
# 2. Upload ${4:data}
# 3. ${5:other}
## ---------------------------


install.packages("remotes")
remotes::install_github("jlegewie/BoundaryDetection")
