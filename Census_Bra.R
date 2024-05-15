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
         , "geobr", "crsuggest", "censobr", "arrow"
         , "usethis"
         )

groundhog.library(pkgs
                  , groundhog.day
                  , #force.install=TRUE
)

# renv::snapshot(exclude = c("INLA", "socialFrontiers")
               
## Program Set-up ------------------------------------------------------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation
# renv::snapshot()
# renv::update()
set.seed(4185) # See https://www.gigacalculator.com/calculators/random-number-generator.php
theme_set(theme_minimal())

## Runs the following --------------------------------------------------------
# 1. Upload block level data from Sao Paulo, data from 2020.
# 2. 


## Import and clean data -------------------------------------------------------

# Available data sets
datasets <- list_geobr()
head(datasets)

# Download all geographic areas within a state at a given year for Sao Paulo
cntr_sp <- read_census_tract(  code_tract = 3550308
                             , year = 2020
                             , showProgress = TRUE
                             , simplified = TRUE  #return the original data set with high resolution at detailed geographic scale 
                             )

# Saving as a RData object to save time
saveRDS(cntr_sp, file = here("Data", "ctr_sp_2020.RDS"))

  # cntr_sp <- readRDS(here("Data", "ctr_sp_2020.RDS"))

# Checks 
sf_use_s2()
  # [1] TRUE
sf::sf_use_s2(FALSE)
  # Spherical geometry (s2) switched off

class(cntr_sp)
  # [1] "sf"         "data.frame"

str(cntr_sp)
  # Classes ‘sf’ and 'data.frame':	101942 obs. of  12 variables

# Checks if CRS is geographic or not
st_crs(cntr_sp)$IsGeographic 
  # [1] TRUE

# Finds out the CRS units 
st_crs(cntr_sp)$units_gdal
  # [1] "degree"

#Extracts its ‘SRID’ identifier
st_crs(cntr_sp)$srid
  # [1] "EPSG:4674"

# Extracts the proj-string
st_crs(cntr_sp)$proj4string
  # [1] "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Convert to projected 
# suggest_crs(cntr_sp)

cntr_sp <- st_transform(cntr_sp, crs = 5880) #EPSG:5880
# Finds out the CRS units 
st_crs(cntr_sp)$units_gdal
  # [1] "metre"

plot(st_geometry(cntr_sp))

## Merge with external data ------------------------------------------------

# Using geobr together with censobr
# only includes microdata from the 2000 and 2010 censuses!!!
data_dictionary( year = 2010
                , dataset = "households"
                , showProgress = TRUE
                , cache = TRUE
               )

hs <- read_households(  year = 2010
                      , columns = NULL #If NULL, it will return all columns
                      , showProgress = TRUE
                      , as_data_frame = TRUE #If TRUE returns an Arrow Dataset
                      , cache = TRUE #If TRUE, it will save the data in a cache folder
                      )

# merge data
cntr_sp$code_muni <- as.character(cntr_sp$code_muni)
hs$code_muni <- as.character(hs$code_muni)

cntr_pop_sp <- left_join(cntr_sp, hs, by = 'code_muni')

# plot map
ggplot() +
  geom_sf(data = esg_sf, aes(fill = cobertura), color=NA) +
  labs(title = "Population") +
  scale_fill_distiller(palette = "Greens", direction = 1, 
                       name='Share of\nhouseholds', 
                       labels = scales::percent) +
  theme_void()

