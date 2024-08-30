# require(utils)
# data(london)

if (!require("pacman")|!require("groundhog")| !require("here")) {
  
  install.packages(c("pacman","groundhog", "here"))
}


library("pacman")
library("here")
library("groundhog")

# install.packages("groundhog")
library("groundhog")
set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2024-04-25" #"2020-05-12"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("dplyr", "tidyverse", "janitor", "sf"
         , "tmap", "devtools", "renv", "Hmisc", "ggplot2"
         , "xfun", "remotes", "sp", "spdep", "maptools"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork", "rmapshaper"
         , "dplyr", "openxlsx", "MASS", "kaggler"
         )

groundhog.library(pkgs, groundhog.day)

# require(devtools)
# devtools::install_github("mkearney/kaggler")
require(kaggler)

# groundhog.library("stringi", groundhog.day, force.source.main = TRUE)
##ISSUES
# With Matrix and stringi, copy folder from the library to the groundhog_library

packageVersion("openxlsx")
# [1] ‘4.2.5.2’
# renv::snapshot(exclude = c("INLA", "socialFrontiers"))

# Get LSOA boundaries:----
# from: https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
# dir.create(here("Data", "London"), showWarnings = FALSE)
download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip"
              , "london_boundaries.zip")

unzip("london_boundaries.zip", exdir = here("Data", "London")
      , overwrite = TRUE
      , junkpaths = TRUE # remove the defauklt path from the zip file
      )
  
london <- read_sf(here("Data", "London","LSOA_2011_London_gen_MHW.shp"))

plot(st_geometry(london))
london

london <- st_transform(london, crs = 4326)

# Simplify shapes
london2 <- ms_simplify(london)

##  Filter to the borough of Barnet
# barnet <-
#   london %>%
#   filter(substr(LSOAname, 1, 6) %in% 'Barnet')


neighb <- spdep::poly2nb( london2
                         , queen=TRUE
                       # , snap=eps
)

coords <- st_coordinates(st_centroid(st_geometry(london2)))

plot(neighb, coords, col="grey", add = FALSE)
# plot(london2)

# Get additional data compatible with LSOA boundaries:----
# from: https://www.data.gov.uk/dataset/ffd9f142-6b70-412e-ab30-6442ff37f685/lsoa-atlas
download.file("https://data.london.gov.uk/download/lsoa-atlas/0193f884-2ccd-49c2-968e-28aa3b1c480d/lsoa-data.csv"
              , destfile = here("Data"
                                , "London"
                                , "LSOA_Atlas.csv")
              )

# Read the file
lsoa_data <- read.csv(here("Data"
                             , "London"
                             , "LSOA_Atlas.csv")
                      , header = TRUE
                      , check.names = FALSE # Col.names with numerals
                      ) %>% 
            dplyr::rename_at(1, ~'LSOA11CD')

lsoa_data[rowSums(is.na(lsoa_data)) != ncol(lsoa_data), ]

#Visual inspection of the data shows that the last two rows are not useful
lsoa_data <- lsoa_data[1:(nrow(lsoa_data) - 2), ]
data.table::setDT(lsoa_data)
nrow(lsoa_data)
# [1] 4835

# Cleaning names
lsoa_data <- clean_names(lsoa_data) %>% 
  dplyr::rename_at(1, ~'LSOA11CD')

colnames(lsoa_data)

# Merge the data with the LSOA boundaries: ----
lsoa_data_sf <- merge(london2, lsoa_data, by.x = "LSOA11CD", by.y = "LSOA11CD")

table(duplicated(lsoa_data_sf$LSOA11CD))
  # FALSE 
  # 4829

lsoa_nbs <- spdep::poly2nb(lsoa_data_sf)
  # Neighbour list object:
  # Number of regions: 4829 
  # Number of nonzero links: 28652 
  # Percentage nonzero weights: 0.1228685 
  # Average number of links: 5.93332 

no_neighbors <- vapply(lsoa_nbs, function(x) identical(x, 0L), logical(1))
table(no_neighbors)
  # FALSE 
  # 4829 

# plot(lsoa_nbs, coords, col = "grey")
# plot(lsoa_nbs[logical_vector == TRUE,], coords, col = "red", add = TRUE)

# restrict to connected map
# lsoa_data_sf <- lsoa_data_sf[!no_neighbors,]

plot(st_geometry(lsoa_data_sf))

# Leaving relevant variables
lsoa_data_sf <- lsoa_data_sf %>% 
  dplyr::select(  "LSOA11CD"
                , "POPDEN"
                , "names"
                , "mid_year_population_estimates_all_ages_2011"
                , "mid_year_population_estimates_aged_16_29_2011"
                , "mid_year_population_estimates_aged_30_44_2011"
                , "mid_year_population_estimates_aged_45_64_2011"
                , "mid_year_population_estimates_aged_65_2011"
                , "mid_year_population_estimates_working_age_2011"
                , "x2011_census_population_age_structure_all_ages"
                , "household_composition_couple_household_without_dependent_children_2011"
                , "household_composition_lone_parent_household_2011"
                , "country_of_birth_percent_not_united_kingdom_2011"
                , "ethnic_group_white_percent_2011"  
                , "ethnic_group_mixed_multiple_ethnic_groups_percent_2011"                                                                
                , "ethnic_group_asian_asian_british_percent_2011"                                                                         
                , "ethnic_group_black_african_caribbean_black_british_percent_2011"                                                       
                , "ethnic_group_other_ethnic_group_percent_2011"                                                                          
                , "ethnic_group_bame_percent_2011" 
                , "household_language_percent_of_households_where_no_people_aged_16_or_over_have_english_as_a_main_language_2011"
                , "tenure_private_rented_percent_2011"
                , "tenure_social_rented_percent_2011"
                , "tenure_owned_with_a_mortgage_or_loan_percent_2011"
                , "house_prices_median_price_2010"
                , "house_prices_median_price_2011"
                , "house_prices_median_price_2012"
                , "adults_in_employment_percent_of_households_with_no_adults_in_employment_with_dependent_children_2011"
                , "economic_activity_economically_active_total_2011"                                                                      
                , "economic_activity_economically_inactive_total_2011"                                                                    
                , "economic_activity_economically_active_employee_2011"                                                                   
                , "economic_activity_economically_active_self_employed_2011"                                                              
                , "economic_activity_economically_active_unemployed_2011"                                                                 
                , "economic_activity_economically_active_full_time_student_2011"
                , "economic_activity_unemployment_rate_2011"
                , "qualifications_percent_no_qualifications_2011"
                , "health_bad_or_very_bad_health_percent_2011"
                , "car_or_van_availability_no_cars_or_vans_in_household_percent_2011"
              )

# Adding other sources of data, check NOMIS
dir(here("Data", "London"), pattern = ".csv")
# [1] "employment_pop.csv"             
# [2] "local_units_indus-employ.csv"  MSOA!!! 
# [3] "LSOA_Atlas.csv"                 
# [4] "Qualification_by_age_LSOA11.csv"

# 
# url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_141_1.data.xlsx?geography=1245708449...1245708476,1245708289,1245708620...1245708645,1245715064,1245715067,1245708646...1245708705,1245714941,1245708822...1245708865,1245708886...1245708919,1245714947,1245708920...1245708952,1245714930,1245714931,1245714944,1245708978...1245709014,1245709066...1245709097,1245714948,1245709121...1245709150,1245714999,1245715000,1245709179...1245709239,1245708290...1245708310,1245714945,1245708311...1245708378,1245714932,1245708379...1245708448,1245714929,1245714934,1245714936,1245708477...1245708519,1245714935,1245708520...1245708557,1245714938,1245708558...1245708592,1245714940,1245708593...1245708619,1245714933,1245715072...1245715076,1245708706...1245708733,1245714942,1245715028,1245708734...1245708794,1245714943,1245708795...1245708821,1245714939,1245708866...1245708885,1245708953...1245708977,1245709015...1245709042,1245714946,1245715069,1245715070,1245709043...1245709065,1245709098...1245709120,1245714982,1245709151...1245709178&date=latestMINUS12&industry=163577857...163577874&employment_sizeband=0,10,20,30,40&legal_status=0,10&measures=20100&signature=NPK-e156dc8ba10b7412aa8bed:0x6ee9910ec10c11565ad435bf27a42320737b893c"
# ##Content
# # 2
# # 2011, Total, Total, value
# # 3
# # 2011, Total, Private sector total, value
# # 4
# # 2011, Micro (0 to 9), Total, value
# # 5
# # 2011, Micro (0 to 9), Private sector total, value
# # 6
# # 2011, Small (10 to 49), Total, value
# # 7
# # 2011, Small (10 to 49), Private sector total, value
# # 8
# # 2011, Medium-sized (50 to 249), Total, value
# # 9
# # 2011, Medium-sized (50 to 249), Private sector total, value
# # 10
# # 2011, Large (250+), Total, value
# # 11
# # 2011, Large (250+), Private sector total, value
# 
# local_units <- read.xlsx(  url
#                          , sheet = 3
#                          , startRow = 2
#                          , skipEmptyRows = TRUE
#                          , skipEmptyCols = TRUE
#                          # , rows = c(2:10138)
#                          # , cols = c(1:20)
#                  )
# 
# View(local_units)
# 
# # # Function to detect blank rows
# # is_blank_row <- function(row) {
# #   all(row == "")
# # }
# # 
# # # Identify the start and end rows of each table
# # table_starts <- which(apply(local_units, 1, is_blank_row) == FALSE)
# # table_ends <- c(table_starts[-1] - 1, nrow(local_units))
# # 
# # # Extract each table
# # tables <- list()
# # for (i in seq_along(table_starts)) {
# #   tables[[i]] <- data[table_starts[i]:table_ends[i], ]
# # }
# # 
# # # Print the extracted tables
# # for (i in seq_along(tables)) {
# #   print(tables[[i]])
# # }
# 
# 
# lsoa_data_sf <- merge(lsoa_data, local_units, by.x = "LSOA11CD", by.y = "LSOA11CD")
# 
# dim(lsoa_data_sf)
# # [1] 4829   38
# 
# saveRDS(lsoa_data_sf, here("Data","London", "lsoa_data_sf.rds"))

# Adding other sources of data, check Kaagle
# https://www.kaggle.com/datasets/dalreada/all-uk-active-companies-by-sic-and-geolocated
# Set-up account: https://koderkow.github.io/kaggler/articles/kaggler.html
# Copy JSON file
# kgl_auth_file_setup("C:Users/Andre/Downloads/kaggle.json")

# Autenticate
kgl_auth(creds_file = "C:/Users/Andrei_WongE/.kaggle/kaggle.json")

# Download dataset
kgl_datasets_download_file(owner_dataset = "dalreada/all-uk-active-companies-by-sic-and-geolocated"
                           , file_name = "AllCompanies2.csv"
                           , path = here("Data","London"))

saveRDS(lsoa_data_sf, here("Data","London", "lsoa_data_sf.rds"))
lsoa_data_sf <- readRDS(here("Data","London", "lsoa_data_sf.rds"))

#Census data https://www.nomisweb.co.uk/census/2011/bulk
# Job density https://www.nomisweb.co.uk/datasets/jd
# Query https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?theme=75&subgrp=Local+Characteristics
# https://webarchive.nationalarchives.gov.uk/ukgwa/20160105230903/http://www.ons.gov.uk/ons/guide-method/classifications/current-standard-classifications/standard-industrial-classification/index.html
# https://npalomin.github.io/pnum/IDBR.html


# Boundaries detection (Following Legewie, 2018) -----

# library("matrixStats")
require("scales")
require("spdep")

groundhog.library(c("tidycensus", "stargazer", "lintr",
                    "styler", "matrixStats"
)
, groundhog.day
# , ignore.deps = "Matrix"
, force.install = TRUE
)

# library("BoundaryDetection")

# Loading functions
source(here("BoundaryDetection.R"))

# Difference variables
colnames(lsoa_data_sf)

# [14] "ethnic_group_white_percent_2011"                                                                              
# [15] "ethnic_group_mixed_multiple_ethnic_groups_percent_2011"                                                       
# [16] "ethnic_group_asian_asian_british_percent_2011"                                                                
# [17] "ethnic_group_black_african_caribbean_black_british_percent_2011"                                              
# [18] "ethnic_group_other_ethnic_group_percent_2011"                                                                 
# [19] "ethnic_group_bame_percent_2011" 

new_names <- c("p_race_white"
               , "p_race_mixed"
               , "p_race_asian_brit"
               , "p_race_african_carib_black"
               , "p_race_bame"
              )

old_names <- c("ethnic_group_white_percent_2011"
                 , "ethnic_group_mixed_multiple_ethnic_groups_percent_2011"
                 , "ethnic_group_asian_asian_british_percent_2011"
                 , "ethnic_group_black_african_caribbean_black_british_percent_2011"
                 , "ethnic_group_bame_percent_2011"
              )   

lsoa_data_sf <- lsoa_data_sf %>% 
                dplyr::rename_with(~ new_names, all_of(old_names)) %>% 
                dplyr::mutate(across(all_of(new_names), ~ . / 100))

vars <- c("p_race_white"
          , "p_race_mixed"
          , "p_race_asian_brit"
          , "p_race_african_carib_black"
          , "p_race_bame"
        )

table(new_names)

# Verifying if proportion data is correct
filtered_data <- lsoa_data_sf %>%
  filter(if_any(all_of(vars), all_vars(. >= 1)))

View(filtered_data[vars])
rm(filtered_data)

# #sp do not support empty geometries
sum(st_is_empty(lsoa_data_sf))
# [1] 0
# chi_ct <- chi_ct[!st_is_empty(chi_ct), ]


#FUNCTION ERRORS!!!!!

#[1]: error in evaluating the argument 'x' in selecting a method for function 
#'addAttrToGeom': empty geometries are not supported by sp classes: conversion failed
#'ORIGIN ERROR 1: sp do not support empty geometries
#'SOLUTION ERROR 1: remove empty geometries
#'LOCATION ERROR 1: function areal_wombling:25; function areal_wombling_bayesian:84;
#'                function areal_wombling_bayesian:130
#'
#'[2]: `mutate_()` was deprecated in dplyr 0.7.0. Please use `mutate()` instead.
#'ORIGIN ERROR 2: function deprecated
#'SOLUTION ERROR 2: use mutate instead of mutate_
#'LOCATION ERROR 2: function areal_wombling:49&51
#'DOWNSTREAM ERROR DUE TO SOLUTION 2:  mutate() cannot handle formulas directly. 
#'use rlang::eval_tidy() to evaluate the formulas within the data frame’s context.
#'modify your code to evaluate the formulas in dots_blv and dots_bmv and return a vector
#'
# debug(areal_wombling)

  bdr <- areal_wombling(  lsoa_data_sf
                        , vars
                        , threshold = NA #{NA}= fuzzy wombling, {0,1}= crips wombling
) 


# bdr <- areal_wombling_bayesian(chi_crime$crime_violent ~ 1 #estimates a model without covariates.
#                         ,"poisson"
#                         , chi_ct, 
#                         , vars
#                         , phi = "leroux"
#                         , threshold = NA #{NA}= fuzzy wombling, {0,1}= crips wombling
#                         ) 

# Mapping ################################################
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))

# Plot 1: Areal units with proportion African-American
scale_color <- col_numeric(c("#F1EEF6", "#034E7B"), domain = c(0, 1))
plot(as(lsoa_data_sf, "Spatial"),
     col = scale_color(lsoa_data_sf$p_race_african_carib_black),
     lwd = 0.01, bor = "white"
)
vals <- quantile(lsoa_data_sf$p_race_african_carib_black, probs = c(0, 0.25, .5, 0.75, 1), na.rm = TRUE)
legend(-87.87, 41.73,
       legend = c("0%", "25%", "50%", "75%", "100%"),
       fill = scale_color(vals), cex = 0.8,
       box.lty = 0, border = "#00000000", title = "Percent Black", title.adj = 3.5
)

# Plot 2: Boundaries
plot(bdr, lwd = rescale(bdr$p_race_african_carib_black_blv, to = c(0.1, 1.25)))
# The spatial lines object does not include Chicago's city boundaries. Let's add them
chi <- st_union(lsoa_data_sf)
plot(as(chi, "Spatial"), lwd = 0.8, add = TRUE)
# Legend showing the scale of boundary values
legend(-87.87, 41.73,
       legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
       lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1.25)),
       cex = 0.8, box.lty = 0, border = "#00000000",
       title = "Boundary Value", title.adj = 3.5
)

png(here("Figures","p_race_african_carib_black_blv.png"), width = 800, height = 600)

dev.off()

# aggregate from line segments to block group level
lsoa_data_sf_blv <- bind_rows(
  group_by(bdr@data, i) %>%
    summarise_at(vars(ends_with("blv")), max, na.rm = TRUE),
  group_by(bdr@data, j) %>%
    summarise_at(vars(ends_with("blv")), max, na.rm = TRUE) %>% dplyr::rename(i = j)
) %>%
  group_by(i) %>%
  summarise_at(vars(ends_with("blv")), max, na.rm = TRUE) %>% 
  janitor::clean_names()

lsoa_data_sf_boundaries <- bind_cols(lsoa_data_sf
                                     , dplyr::select(lsoa_data_sf_blv
                                     , -i)
                                    )

# compare the boundary values for spatial lines (border line segments) with the
# boundary values for areal units.
# for spatial lines (border line segments) with the boundary values for areal units.
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))

# Plot 1: Boundary values for border line segments
(summary_data <- bdr@data %>%
  summarise(mean_value = mean(p_race_african_carib_black_blv)
            , min_value = min(p_race_african_carib_black_blv)
            , max_value = max(p_race_african_carib_black_blv)
          )
)

# plot(bdr, lwd = rescale(bdr$p_race_african_carib_black_blv, to = c(0.1, 1.25)))
plot(bdr, lwd = rescale(bdr$p_race_african_carib_black_blv, to = c(0.1, 0.35)))
# plot(bdr, lwd = bdr$p_race_african_carib_black_blv)

chi <- st_union(lsoa_data_sf_boundaries)

plot(as(chi, "Spatial"), lwd = 0.5, add = TRUE)
legend(-87.87, 41.73,
       legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
       lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 0.35)),
       # lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1.25)),
       cex = 0.8, box.lty = 0, border = "#00000000",
       title = "Boundary Value", title.adj = 3.5
)

png(here("Figures","Boundary_values_segment.png"), width = 800, height = 600)

# Plot 2: Boundary values for border line segments for areal units
sel <- is.finite(lsoa_data_sf_boundaries$p_race_african_carib_black_blv)
scale_color <- col_numeric(c("#F1EEF6", "#034E7B"), domain = c(0, 1))
cols <- scale_color(lsoa_data_sf_boundaries$p_race_african_carib_black_blv[sel])

plot(as(lsoa_data_sf_boundaries[sel, ], "Spatial"), lwd = 0.1, col = cols, bor = "white")
legend(-87.87, 41.73,
       legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
       fill = scale_color(c(0, 0.25, 0.5, 0.75, 1)), cex = 0.8, box.lty = 0,
       border = "#00000000", title = "Boundary Value", title.adj = 3.5
)

png(here("Figures","Boundary_values_areal.png"), width = 800, height = 600)
dev.off()

# Spatial regression ################################################
f1 <- "crime_violent ~ edge_wombling_race + log(pop) + p_race_black + p_race_hisp +
p_race_asian + con_disadv + res_instab + immi_con + hhi + age_15_35_male"
m1 <- glm.nb(f1, data = lsoa_data_sf)
stargazer(m1, type = "text", no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
