# require(utils)
# data(london)

require(rmapshaper)
require(sf)
require(dplyr)
require(spdep)
require(ggplot2)
require(janitor)

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
                , "x2011_census_population_age_structure_aged_16_29"
                , "x2011_census_population_age_structure_aged_30_44"
                , "x2011_census_population_age_structure_aged_45_64"
                , "x2011_census_population_age_structure_aged_65"
                , "household_composition_couple_household_without_dependent_children_2011"
                , "household_composition_lone_parent_household_2011"
                , "country_of_birth_percent_not_united_kingdom_2011"
                , "ethnic_group_bame_percent_2011"
                , "ethnic_group_white_percent_2011"
                , "ethnic_group_asian_asian_british_percent_2011"
                , "ethnic_group_black_black_british_percent_2011"
                , "ethnic_group_black_african_caribbean_black_british_percent_2011"
                , "household_language_percent_of_households_where_no_people_aged_16_or_over_have_english_as_a_main_language_2011"
                , "tenure_private_rented_percent_2011"
                , "tenure_social_rented_percent_2011"
                , "tenure_owned_with_a_mortgage_or_loan_percent_2011"
                , "house_prices_median_price_2010"
                , "house_prices_median_price_2011"
                , "house_prices_median_price_2012"
                , "adults_in_employment_percent_of_households_with_no_adults_in_employment_with_dependent_children_2011"
                , "economic_activity_economically_active_unemployed_2011"
                , "economic_activity_economically_active_inactive_20"
                , "economic_activity_economically_active_full_time_student_2011"
                , "economic_activity_economically_active_retired_2011"
                , "qualifications_percent_no_qualifications_2011"
                , "health_bad_or_very_bad_health_percent_2011"
                , "car_or_van_availability_no_cars_or_vans_in_household_percent_2011"
              )
                
                
                
mid_year_population_estimates_aged_45_59_2011
                
#Census data https://www.nomisweb.co.uk/census/2011/bulk
# Job density https://www.nomisweb.co.uk/datasets/jd
# Query https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?theme=75&subgrp=Local+Characteristics
# https://webarchive.nationalarchives.gov.uk/ukgwa/20160105230903/http://www.ons.gov.uk/ons/guide-method/classifications/current-standard-classifications/standard-industrial-classification/index.html
