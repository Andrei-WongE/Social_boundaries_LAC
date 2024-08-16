# require(utils)
# data(london)

require(rmapshaper)
require(sf)
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
plot(london2)



