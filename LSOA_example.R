if (!require("pacman")|!require("groundhog")| !require("here")) {
  
  install.packages(c("pacman","groundhog", "here"))
}


library("pacman")
library("here")
library("groundhog")

set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2024-04-25" #"2020-05-12"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("dplyr", "tidyverse", "janitor", "sf"
         , "tmap", "devtools", "renv", "Hmisc", "ggplot2"
         , "xfun", "remotes", "sp", "spdep", "maptools"
         , "foreach", "doParallel", "parallel", "progress"
         , "doSNOW", "purrr", "patchwork", "rmapshaper"
         , "dplyr", "openxlsx", "MASS", "reticulate"
         , "future", "furrr", "data.table","leaflet"
         , "spgwr", "jtools"
         )

groundhog.library(pkgs, groundhog.day)
library(parallel) # Core package, so no install

# requir(devtools)
# devtools::install_github("mkearney/kaggler")
# require(kaggler) # This does not work
# IN Terminal: pip install kaglle

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
lsoa_data <- janitor::clean_names(lsoa_data) %>% 
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
# Place the kaggle.json file in the ~/.kaggle/ directory 
# THEN py_run_string("import kaggle")

# Python kaggle API
require(reticulate)
kaggle <- import("kaggle")

# Download all files in dataset [see fifference between '''_download_file and '''_download_files]
kaggle$api$dataset_download_files("dalreada/all-uk-active-companies-by-sic-and-geolocated"
                                 , path = here("Data", "London")
                                 , unzip = TRUE
                                 )
#Downloads 2 files: AllCompanies2.csv and SIC.csv

# Read the files, clean and generate variables
companies_data <- read.csv(here("Data"
                           , "London"
                           , "AllCompanies2.csv")
                      , header = TRUE
                      , check.names = FALSE
                      ) %>% 
                  janitor::clean_names(.) %>% 
                  mutate(sic = ifelse(sic == "None", NA, sic)) %>% 
                  mutate(sic = as.integer(sic)) %>% 
                  filter(sic != 99999) %>%
                  mutate(sic = sprintf("%05d", sic)) %>% 
                  mutate(incorporation_date = as.Date(incorporation_date, format = "%d/%m/%Y")) %>% 
                  mutate(firm_age = interval(incorporation_date, Sys.Date()) / years(1)) %>% 
                  mutate(firm_age = round(firm_age, 0))
  
# companies_data %>% 
#   filter(sic == "None") %>% 
#   nrow() #[1] 2.5% with empty SIC

#A dormant company is a company that has already been through the company registration 
# and incorporation process but it does not carry out business activities in a given 
# period of time. SIC code 99999.

sic_df <- read.csv(here("Data"
                          , "London"
                          , "SIC.csv")
                          , header = TRUE
                          , check.names = FALSE
                          ) %>% 
            janitor::clean_names(.) %>% 
            mutate(sic_code = as.character(sic_code)) %>%
            mutate(sic_code = sprintf("%05s", sic_code))

# Merge the SIC data with the companies data
companies_data <- companies_data %>% 
  left_join(sic_df, by = c("sic"="sic_code"))

dim(companies_data)
# [1] 3546225       8

# Check if all companies have Y:X pairs
colnames(companies_data)
dim(companies_data)


companies_data %>% 
  filter(is.na(latitude) | is.na(longitude)) %>% 
  nrow()

# Check if all companies have SIC
companies_data %>% 
  filter(is.na(sic)) %>% 
  nrow()

# Merge the data with the LSOA boundaries:
# Convert csv to sf
points_sf <- st_as_sf(companies_data
                      , coords = c("longitude", "latitude")
                      , crs = st_crs(lsoa_data_sf))

points_sf$points_id <- seq_len(nrow(points_sf))
lsoa_data_sf$polygon_id <- seq_len(nrow(lsoa_data_sf))

# Sample a subset of points
# sample_points <- points_sf %>% sample_n(1000)
# 
# plot(st_geometry(sample_points), col = "blue", pch = 16, main = "Sample")

# Spatial join
# lsoa_data_sf<- st_join(lsoa_data_sf, points_sf)
# rm(points_sf)

# # Parallelizing spatial join
# plan(multisession, workers = parallel::detectCores() - 1)
# 
# # Increase the maximum size of globals
# options(future.globals.maxSize = 500 * 1024^2)  # 500 MB
# 
# # Split into chunks of 1000 rows, 3801732/1000 = 3802 chunks/11 cores = 346 chunks per core
# chunks <- split(points_sf, 1:nrow(points_sf) %/% 1000)  
# 
# results <- future_map(chunks, ~ st_join(lsoa_data_sf, .x))
# 
# merged_data <- do.call(rbind, results)

# Perform the intersection using parallel processing

  # # Set up parallel cluster
  # cl <- makeCluster(detectCores() - 1)
  # 
  # # Export objects to cluster
  # clusterExport(cl, c("lsoa_data_sf", "points_sf"))
  # 
  # intersections <- parLapply(cl, 1:nrow(lsoa_data_sf), function(i) {
  #   sf::st_intersects(lsoa_data_sf[i, ], points_sf)
  # })
  # 
  # # Stop the cluster
  # stopCluster(cl)
  # 
  # # saveRDS(intersections, here("Data","London", "intersections.rds"))
  # # intersections <- readRDS(here("Data","London", "intersections.rds"))
  # 
  # # Create a data frame from the intersections
  # #unlist() might be flattening the list in an unexpected way, so use sapply() instead
  # 
  # polygon_ids <- integer(0)
  # point_ids <- integer(0)
  # 
  # for (i in seq_along(intersections)) {
  #   if (length(intersections[[i]]) > 0) {
  #     polygon_ids <- c(polygon_ids, rep(i, length(intersections[[i]])))
  #     point_ids <- c(point_ids, intersections[[i]])
  #   }
  # }
  # 
  # # Create the data frame
  # intersect_df <- data.frame(
  #   polygon_id = polygon_ids,
  #   point_id = point_ids
  # )
  # 
  # 
  # # Ensure the CRS are the same
  # st_crs(lsoa_data_sf)==st_crs(points_sf)
  # 
  # # Merge the data based on the intersections
  # lsoa_data_sf <- lsoa_data_sf %>%
  #   left_join(intersect_df, by = c("id" = "polygon_id")) %>%
  #   st_join(points_sf, join = st_intersects)
  # 
  # dim(lsoa_data_sf)

# Set up parallel processing
num_cores <- parallel::detectCores() - 1  # Leave one core free for system processes
cl <- makeCluster(num_cores)

# Export necessary packages and functions to the cluster
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
})

# Function to process chunks
process_chunk <- function(chunk, polygons) {
  # Use st_intersects for efficient spatial join
  intersects <- sf::st_intersects(chunk, polygons)
  
  # Assign polygon IDs to points
  chunk$matched_polygon_id <- sapply(intersects, function(x) {
    if (length(x) > 0) polygons$polygon_id[x[1]] else NA
  })
  
  # Join polygon attributes
  chunk <- left_join(chunk, 
                     as.data.frame(polygons) %>% select(-geometry),
                     by = c("matched_polygon_id" = "polygon_id"))
  
  return(chunk)
}

# Main processing function
spatial_join <- function(points_sf, lsoa_data_sf, chunk_size = 100000) {
  
  # Ensure validity of polygons
  lsoa_data_sf <- st_make_valid(lsoa_data_sf)
  
  # Split points into chunks
  point_list <- split(points_sf, ceiling(seq_len(nrow(points_sf))/chunk_size))
  
  # Export large objects to the cluster workers
  clusterExport(cl, c("lsoa_data_sf", "process_chunk"))
  
  # Process chunks in parallel
  results <- parLapply(cl, point_list, process_chunk, polygons = lsoa_data_sf)
  
  # Combine results
  points_with_polygon <- do.call(rbind, results)
  
  return(points_with_polygon)
}

# Check points_id and polygon_id are present
if (!"points_id" %in% names(points_sf)) stop("points_sf must have a 'points_id' column")
if (!"polygon_id" %in% names(lsoa_data_sf)) stop("lsoa_data_sf must have a 'polygon_id' column")

# Perform the efficient spatial join
points_with_polygon <- spatial_join(points_sf, lsoa_data_sf)

# Clean up and save
stopCluster(cl)
gc()

# Save the resulting dbs
saveRDS(points_with_polygon, here("Data","London", "points_with_polygon.rds"))
saveRDS(lsoa_data_sf, here("Data","London", "lsoa_data_sf.rds"))

# Create a color palette for polygons
pal <- colorFactor(palette = "viridis", domain = lsoa_data_sf$polygon_id)

# Mapping the map
map <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(data = lsoa_data_sf, 
              fillColor = ~pal(polygon_id),
              fillOpacity = 0.7, 
              color = "white", 
              weight = 1,
              label = ~as.character(polygon_id),
              labelOptions = labelOptions(noHide = FALSE, direction = "auto")) %>%
  addCircleMarkers(data = points_with_polygon, 
                   radius = 2, 
                   color = "red", 
                   fillOpacity = 0.7,
                   label = ~paste("Point ID:"
                                         , as.character(points_id)
                                         , "| Type:", as.character(description)),
                   labelOptions = labelOptions(noHide = FALSE, direction = "auto"))

# Add legend
map <- map %>% addLegend(pal = pal, values = lsoa_data_sf$polygon_id, 
                         title = "Polygon ID", position = "bottomright")

# map

# Save map
dir.create("Maps")
# install.packages("webshot")
# webshot::install_phantomjs()

# mapview::mapshot(map, file = here("Maps", "lsoa_points_map.png"))

###ERROR!!! NO EFFING IDEA WHY
#  Windows exception, code 0xc0000005.
# PhantomJS has crashed. Please read the bug reporting guide at
# <http://phantomjs.org/bug-reporting.html> and file a bug report.
# Error in (function (url = NULL, file = "webshot.png", vwidth = 992, vheight = 744,  : 
#                       webshot.js returned failure value: -1073741819
###SOLUTION: export as html file

# Save as an HTML file
require(htmlwidgets)
saveWidget(map
           , file = here("Maps", "lsoa_points_map.html")
           , selfcontained = TRUE
           , title = "London companies and LSOA Polygons"
           )

# Summary of the join results
cat("Points assigned to polygons:", sum(!is.na(points_with_polygon$matched_polygon_id)), "\n")
cat("Points not assigned to any polygon:", sum(is.na(points_with_polygon$matched_polygon_id)), "\n")
##CHECK!!!!!! Lots of points not assigned to any polygon!!!!

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

View(filtered_data[vars]) # Should be an empty table
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
                        , threshold = NA 
                        #{NA}= fuzzy wombling, {0,1}= crips wombling
) 


# bdr <- areal_wombling_bayesian(chi_crime$crime_violent ~ 1 #estimates a model without covariates.
#                         ,"poisson"
#                         , chi_ct, 
#                         , vars
#                         , phi = "leroux"
#                         , threshold = NA #{NA}= fuzzy wombling, {0,1}= crips wombling
#                         ) 

saveRDS(lsoa_data_sf, here("Data","London", "lsoa_data_sf.rds"))
saveRDS(bdr, here("Data","London", "bdr_sp.rds"))

# Starting point with saved data ----
points_with_polygon <- readRDS(here("Data","London", "points_with_polygon.rds"))
lsoa_data_sf <- readRDS(here("Data","London", "lsoa_data_sf.rds"))
bdr <- readRDS(here("Data","London", "bdr.rds"))

bdr_sf <- st_as_sf(bdr)
bdr_sf <- bdr_sf[!st_is_empty(bdr_sf), ] # Remove any features with empty geometries

# Mapping ################################################
require(grDevices)
require(viridis)

par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))

# bbox <- st_bbox(bdr_sf)
# legend_x <- bbox["xmin"] + (bbox["xmax"] - bbox["xmin"]) * 0.1  # 10% from the left
# legend_y <- bbox["ymin"] + (bbox["ymax"] - bbox["ymin"]) * 0.9  # 90% from the bottom


# Plot 1: Areal units with proportion African-American
scale_color <- col_numeric(c("#F1EEF6", "#034E7B"), domain = c(0, 1))
plot(lsoa_data_sf[,17],
     col = scale_color(lsoa_data_sf$p_race_african_carib_black),
     lwd = 0.01, border = "white"
)
vals <- quantile(lsoa_data_sf$p_race_african_carib_black, probs = c(0, 0.25, .5, 0.75, 1), na.rm = TRUE)
legend(-0.4180936,51.64666,
       legend = c("0%", "25%", "50%", "75%", "100%"),
       fill = scale_color(vals), cex = 0.8,
       box.lty = 0, border = "#00000000", title = "Percent Black", title.adj = 3.5
)
# png(here("Figures","p_race_african_carib_black_blv_areal.png")
#     , width = 800, height = 600)

# Plot 2: Boundaries
plot(bdr_sf[,6]
     , lwd = rescale(bdr_sf$p_race_african_carib_black_blv, to = c(0.1, 1.5)))
# The spatial lines object does not include city boundaries. Let's add them
chi <- st_union(lsoa_data_sf)
plot(chi, lwd = 0.8, add = TRUE)
# Legend showing the scale of boundary values
legend(-0.4180936,51.64666,
       legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
       lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.2, 1.5)),
       cex = 0.8, box.lty = 0, border = "#00000000",
       title = "Boundary Value", title.adj = 3.5
)

# png(here("Figures","p_race_african_carib_black_blv_boundaries.png")
#     , width = 800, height = 600)

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

lsoa_data_sf <- bind_cols(lsoa_data_sf
                                     , dplyr::select(lsoa_data_sf_blv
                                     , -i)
                                    )

# compare the boundary values for spatial lines (border line segments) with the
# boundary values for areal units.
# for spatial lines (border line segments) with the boundary values for areal units.
  # par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
  # 
  # # Plot 1: Boundary values for border line segments
  # 
  # print(summary(bdr_sf$p_race_african_carib_black_blv))
  # 
  # plot(bdr_sf[,6]
  #      , lwd = rescale(bdr_sf$p_race_african_carib_black_blv
  #      , to = c(0.1, 1.25))
  #      )
  # # plot(bdr, lwd = rescale(bdr$p_race_african_carib_black_blv, to = c(0.1, 0.47)))
  # # plot(bdr, lwd = bdr$p_race_african_carib_black_blv)
  # 
  # chi <- st_union(lsoa_data_sf)
  # 
  # plot(chi, lwd = 0.5, add = TRUE)
  # legend("bottomright",
  #        legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
  #        # lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1)),
  #        lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1.25)),
  #        cex = 0.8, box.lty = 0, border = "#00000000",
  #        title = "Boundary Value", title.adj = 3.5
  # )
  # 
  # # png(here("Figures","Boundary_values_segment.png"), width = 800, height = 600)
  # 
  # # Plot 2: Boundary values for border line segments for areal units
  # sel <- is.finite(lsoa_data_sf$p_race_african_carib_black_blv)
  # scale_color <- col_numeric(c("#F1EEF6", "#034E7B"), domain = c(0, 1))
  # cols <- scale_color(lsoa_data_sf$p_race_african_carib_black_blv[sel])
  # 
  # plot(as(lsoa_data_sf[sel, ], "Spatial"), lwd = 0.1, col = cols, bor = "white")
  # legend("bottomright",
  #        legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
  #        fill = scale_color(c(0, 0.25, 0.5, 0.75, 1)), cex = 0.8, box.lty = 0,
  #        border = "#00000000", title = "Boundary Value", title.adj = 3.5
  # )
  # 
  # png(here("Figures","Boundary_values_lineseg.png"), width = 800, height = 600)
  # dev.off()
  # 
  # 

# Set up the plotting area
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))

# Plot 1: Boundary values for border line segments
print(summary(bdr_sf$p_race_african_carib_black_blv))

plot(st_geometry(bdr_sf),
     lwd = rescale(bdr_sf$p_race_african_carib_black_blv, to = c(0.1, 1.25)),
     main = "Boundary Values for Border Line Segments")

chi <- st_union(lsoa_data_sf)

plot(st_geometry(chi), lwd = 0.5, add = TRUE)
legend("bottomright",
       legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
       lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1.25)),
       cex = 0.8, box.lty = 0, border = "#00000000",
       title = "Boundary Value", title.adj = 3.5)

# Plot 2: Boundary values for border line segments for areal units
sel <- is.finite(lsoa_data_sf$p_race_african_carib_black_blv)
scale_color <- colorNumeric(viridis(100), domain = c(0, 1))
cols <- scale_color(lsoa_data_sf$p_race_african_carib_black_blv[sel])

plot(st_geometry(lsoa_data_sf[sel, ]),
     col = cols, border = "white",
     main = "Boundary Values for Areal Units")

legend("bottomright",
       legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
       fill = scale_color(c(0, 0.25, 0.5, 0.75, 1)),
       cex = 0.8, box.lty = 0, border = "#00000000",
       title = "Boundary Value", title.adj = 3.5)

# Save the plots
png(here("Figures", "Boundary_values_combined.png"), width = 1600, height = 600)
dev.off()

# Spatial EDA ################################################
#"r pkg("sfdep")" "Python pkg("PySal")"

# Spatial autocorrelation


# Spatial heterogeneity



# Spatial regression ################################################
new_names2 <- c("working_age_pop"
               , "all_ages_pop" #CHECK!!!
               , "lone_parent_hh"
               , "not_uk_born"
               , "no_english_main_lang"
               , "house_price_2010"
               , "no_adults_in_employment"
               , "full_time_student"
               , "unemployment_rate"
               , "no_qualifications"
               , "bad_health"
              )

old_names2 <- c("mid_year_population_estimates_working_age_2011"
               , "x2011_census_population_age_structure_all_ages"
               , "household_composition_lone_parent_household_2011"
               , "country_of_birth_percent_not_united_kingdom_2011"
               , "household_language_percent_of_households_where_no_people_aged_16_or_over_have_english_as_a_main_language_2011"
               , "house_prices_median_price_2010"
               , "adults_in_employment_percent_of_households_with_no_adults_in_employment_with_dependent_children_2011"
               , "economic_activity_economically_active_full_time_student_2011"
               , "economic_activity_unemployment_rate_2011"
               , "qualifications_percent_no_qualifications_2011"
               , "health_bad_or_very_bad_health_percent_2011"
              )

lsoa_data_sf <- lsoa_data_sf %>% 
  dplyr::rename_with(~ new_names2, all_of(old_names2))

# Create variable with count of companies, using matched id
# Assuming that lsoa_data_sf$polygon_id matches points_with_polygon$matched_polygon_id
# The coalesce() function at the end ensures that polygons with no matching points get a count of 0 instead of NA

library(sf)
library(dplyr)

# Convert points_with_polygon to a regular data frame for counting
points_df <- points_with_polygon %>%
  st_drop_geometry() %>%
  group_by(matched_polygon_id) %>%
  summarise(company_count = n())

# Join count data to the sf object
lsoa_data_sf <- lsoa_data_sf %>%
  left_join(points_df, by = c("polygon_id" = "matched_polygon_id")) %>%
  mutate(company_count = coalesce(company_count, 0))  # Replace NA with 0 for polygons with no points


# Legewie (2018) derives two measures from the boundary values that are appropriate for multi-group settings. 
# Change this as this is not correct

lsoa_data_sf <- lsoa_data_sf %>%
  mutate(
    # composite measure
    edge_wombling_race = matrixStats::rowMaxs(
      cbind(p_race_white_blv
            ,p_race_mixed_blv
            ,p_race_asian_brit_blv
            ,p_race_african_carib_black_blv
            ,p_race_bame_blv
            )
      , na.rm = FALSE
                                          ),
    # pairwise boundary measures
     edge_race_wm    = p_race_white_blv * p_race_mixed_blv
    ,edge_race_wab   = p_race_white_blv * p_race_asian_brit_blv
    ,edge_race_wafc  = p_race_white_blv * p_race_african_carib_black_blv
    ,edge_race_wb    = p_race_white_blv * p_race_bame_blv
    ,edge_race_mab   = p_race_mixed_blv * p_race_asian_brit_blv
    ,edge_race_mafc  = p_race_mixed_blv * p_race_african_carib_black_blv
    ,edge_race_wb    = p_race_mixed_blv * p_race_bame_blv
    ,edge_race_abafc = p_race_asian_brit_blv * p_race_african_carib_black_blv
    ,edge_race_abb   = p_race_asian_brit_blv * p_race_bame_blv
    ,edge_race_afcb  = p_race_african_carib_black_blv * p_race_bame_blv
  )

# Run regressions ----
# See https://github.com/cran-task-views/Spatial/blob/main/Spatial.md
## Basic regression
f1 <- "company_count ~ edge_wombling_race + log(all_ages_pop) + p_race_white +
               p_race_mixed + p_race_asian_brit  + p_race_african_carib_black +
               p_race_bame + lone_parent_hh + not_uk_born + house_price_2010 + 
               bad_health + working_age_pop"
m1 <- glm.nb(f1, data = lsoa_data_sf)

stargazer(m1
          , type = "text"
          , no.space = TRUE
          , star.cutoffs = c(0.05, 0.01, 0.001)
          , out = here("Output", "regression_tablef1.txt")
          )

# Compare the composite measure to the pairwise boundary measures
f2 <- "company_count ~ edge_wombling_race +log(all_ages_pop) + p_race_white  + 
               p_race_mixed + p_race_asian_brit  + p_race_african_carib_black + 
               p_race_bame + lone_parent_hh + not_uk_born + house_price_2010 +  
               bad_health + working_age_pop"

f3 <- "company_count ~ edge_race_wm + edge_race_wab + edge_race_wafc + edge_race_wb +     
                          edge_race_mab + edge_race_mafc + edge_race_wb + edge_race_abafc + 
                          edge_race_abb + edge_race_afcb + log(all_ages_pop) +  
                          p_race_white + p_race_mixed + p_race_asian_brit +  
                          p_race_african_carib_black + p_race_bame +  
                          lone_parent_hh + not_uk_born + house_price_2010 +  
                          bad_health + working_age_pop"

m2 <- glm.nb(f2, data = lsoa_data_sf)
m3 <- glm.nb(f3, data = lsoa_data_sf)

stargazer(m2, m3
          , type = "text"
          , no.space = TRUE
          , star.cutoffs = c(0.05, 0.01, 0.001)
          , column.labels = c("Composite measure", "Pairwise boundaries")
          , out = here("Output", "regression_tablef2f3.txt")
          )

## GWR or spatially varying coefficient (SVC) models?
# CAVEAT:collinearity tends to be problematic in GWR models, See Comber et al. 2022
# https://onlinelibrary.wiley.com/doi/10.1111/gean.12316
# https://gdsl-ul.github.io/san/09-gwr.html

#r pkg("varycoef") and r pkg("spBayes")
#r pkg("gwrr") pkg("GWmodel") pkg("spgwr")
# Also fastgwr https://github.com/Ziqi-Li/FastGWR in Python
require(spgwr)
require(jtools)

# specify a model equation
eq1 <- company_count ~ edge_wombling_race +log(all_ages_pop) + p_race_white  + 
               p_race_mixed + p_race_asian_brit  + p_race_african_carib_black + 
               p_race_bame + lone_parent_hh + not_uk_born + house_price_2010 +  
               bad_health + working_age_pop

eq2 <- company_count ~ edge_race_wm + edge_race_wab + edge_race_wafc + edge_race_wb +     
                          edge_race_mab + edge_race_mafc + edge_race_wb + edge_race_abafc + 
                          edge_race_abb + edge_race_afcb + log(all_ages_pop) +  
                          p_race_white + p_race_mixed + p_race_asian_brit +  
                          p_race_african_carib_black + p_race_bame +  
                          lone_parent_hh + not_uk_born + house_price_2010 +  
                          bad_health + working_age_pop

# Fixed Bandwidth
# find optimal kernel bandwidth using cross validation
lsoa_data_sp <- as(lsoa_data_sf, "Spatial")
coords_db <- coordinates(lsoa_data_sp)

fbw <- gwr.sel(eq1, 
               data = lsoa_data_sp, 
               coords=coords_db,
               longlat = FALSE, # if SpatialPoints object, the value taken from the object itself
               adapt=FALSE, 
               gweight = gwr.Gauss, 
               verbose = FALSE)

# view selected bandwidth
fbw

# fit a gwr based on fixed bandwidth
fb_gwr <- gwr(eq1, 
              data = lsoa_data_sp,
              coords=coords_db,
              longlat = FALSE,
              bandwidth = fbw, 
              gweight = gwr.Gauss,
              hatmatrix=TRUE, 
              se.fit=TRUE)

fb_gwr

# write gwr output into a data frame
fb_gwr_out <- as.data.frame(fb_gwr$SDF)

utla_shp$fmb_localR2 <- fb_gwr_out$localR2

# mapping results
# Local R2
map_fbgwr1 = tm_shape(lsoa_data_sp) +
  tm_fill(col = "fmb_localR2", title = legend_title, palette = magma(256), style = "cont") + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("right", "top") , size = 5) + # add compass
  tm_scale_bar(breaks = c(0,1,2), text.size = 0.7, position =  c("center", "bottom")) + # add scale bar
  tm_layout(bg.color = "white") # change background colour
map_fbgwr1 + tm_shape(reg_shp) + # add region boundaries
  tm_borders(col = "white", lwd = .5) # add borders
legend_title = expression("Fixed: Local R2")

# Adaptive bandwidth
# find optimal kernel bandwidth using cross validation
abw <- gwr.sel(eq1, 
               data = lsoa_data_sp, 
               coords=coords_db,
               longlat = TRUE,
               adapt = TRUE, 
               gweight = gwr.Gauss, 
               verbose = FALSE)

# view selected bandwidth
abw

# Assessing model fit
# write gwr output into a data frame
ab_gwr_out <- as.data.frame(ab_gwr$SDF)

utla_shp$amb_ethnic <- ab_gwr_out$ethnic
utla_shp$amb_lt_illness <- ab_gwr_out$lt_illness
utla_shp$amb_localR2 <- ab_gwr_out$localR2

# Mapping results
# Local R2
map_abgwr1 = tm_shape(lsoa_data_sp) +
  tm_fill(col = "amb_localR2", title = legend_title, palette = magma(256), style = "cont") + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("right", "top") , size = 5) + # add compass
  tm_scale_bar(breaks = c(0,1,2), text.size = 0.7, position =  c("center", "bottom")) + # add scale bar
  tm_layout(bg.color = "white") # change background colour
map_abgwr1 + tm_shape(reg_shp) + # add region boundaries
  tm_borders(col = "white", lwd = .5) # add borders
legend_title = expression("Adaptive: Local R2")

## SGWR
# See: https://github.com/Lessani252/SGWR
# system("conda activate your-env-name && python -m sgwr run -np 8 -datasystem("conda activate your-env-name && python -m sgwr run -np 4 -data C:/path/to/your/data")")

