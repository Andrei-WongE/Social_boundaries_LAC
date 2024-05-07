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
         , "doSNOW", "purrr", "patchwork")

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
data <- st_read("data/Shape_Lima/ZONAS_CENSALES.shp") %>% 
  st_transform(crs = 24892) #PSAD56 / Peru central zone

# Simple feature collection with 1721 features and 13 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: 260700 ymin: 8618000 xmax: 318200 ymax: 8703000
# Projected CRS: WGS 84 / UTM zone 18S

# Checks if CRS is geographic or not
st_crs(data)$IsGeographic 
# [1] FALSE

# Finds out the CRS units 
st_crs(data)$units_gdal
# [1] "metre"

st_layers("data/LimaMet/EstratoLimaMetropolitanashp.shp")

# data <- data[data$POB > 0,] #Remove rows with no population
# data <- data[data$POB > 100,] #Remove rows with less than 100 population

plot_census_zones <- function(data, title = "Census Zones") {
  ggplot(data) +
    geom_sf(aes(fill = POB)) +
    labs(title = title) +
    theme_minimal()
}







