## Set-up correct library ----

versionR <- strsplit(version[['version.string']], ' ')[[1]][3]

if (versionR!= "4.4.0") {
  stop("This script was written in R version 4.4.0. Please install this 
       version to ensure reproducibility.")
}

.libPaths( c("C:/Program Files/R/R-4.4.0/library", .libPaths() ) )

## Load required packages ----

if (!require("pacman")|!require("groundhog")| !require("here")) {
  
  install.packages(c("pacman","groundhog", "here"))
}

library("pacman")
library("here")
library("groundhog")

set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2024-04-25"

# To ensure reproducibility of your script, given timezone differences and
# delays in updating different CRAN mirrors, don't use a date more
# recent than two days ago

#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("here", "yaml", "dplyr", "tidyverse", "janitor", "sf"
         , "tmap", "devtools", "renv", "Hmisc", "ggplot2"
         , "xfun", "remotes", "sp", "spdep", "maptools"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork"
         , "geobr"
         )

groundhog.library(pkgs
                  , groundhog.day
                  , #force.install=TRUE
)

## Program Set-up ------------------------------------------------------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
# renv::snapshot()
# renv::update()
set.seed(4185) # See https://www.gigacalculator.com/calculators/random-number-generator.php
theme_set(theme_minimal())

## Runs the following --------------------------------------------------------
# 1. Upload block level data from Lima, Census 2017.
# 2. 


## Import and clean data -------------------------------------------------------

# Available data sets
datasets <- list_geobr()



head(datasets)