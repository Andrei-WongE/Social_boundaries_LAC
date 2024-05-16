library("MASS")
library("sp")
library("sf")
# library("matrixStats")
library("tidyverse")
library("scales")

groundhog.library(c("tidycensus", "stargazer", "lintr",
                    "styler", "matrixStats"
                    )
                , groundhog.day
              # , ignore.deps = "Matrix"
                , force.install = TRUE
                )

# library("BoundaryDetection")

--------------------------------------------------------------
  # Code mainly from Legewie, 2018. See supplementary material.#
--------------------------------------------------------------

# Load the data ###############################################################
    CENSUS_API_KEY <- Sys.getenv('CENSUS_API_KEY')

    tidycensus::census_api_key(
      "CENSUS_API_KEY"
      , install = TRUE
      , overwrite=TRUE
    ) # See Sys.getenv("CENSUS_API_KEY")

# 2010 decennial census data on the census tract level (chi_sf1)
# 2007-2011 ACS 5-years estimates (chi_acs) for Cook County, Illinois.

# Download Decennial Census data 2010
vars_census <- c(
  "P001001", "P012006", "P012007", "P012008", "P012009",
  "P012010", "P012011", "P012012", "P005003", "P005004",
  "P005006", "P005010", "P015001", "P005005", "P005007",
  "P005008", "P005009"
)

chi_sf1 <- tidycensus::get_decennial( geography = "tract"
                                    , state = "IL"
                                    , county = "Cook County"
                                    , variables = vars_census
                                    , year = 2010
                                    , output = "wide"
                                    , geometry = TRUE
                                  )

# Download ACS data 2011-2016
vars_acs <- c(
  "C24010_004E", "C24010_040E", "C24010_001E", "B23025_005E", "B23025_003E",
  "B17021_001E", "B17021_002E", "B15002_011E", "B15002_028E", "B15002_001E",
  "B11003_016E", "B11001_001E", "B25001_001E", "B25003_003E", "B25002_002E",
  "B25002_003E", "B16004_001E", "B16004_006E", "B16004_007E", "B16004_008E",
  "B16004_011E", "B16004_012E", "B16004_013E", "B16004_016E", "B16004_017E",
  "B16004_018E", "B16004_021E", "B16004_022E", "B16004_023E", "B16004_028E",
  "B16004_029E", "B16004_030E", "B16004_033E", "B16004_034E", "B16004_035E",
  "B16004_038E", "B16004_039E", "B16004_040E", "B16004_043E", "B16004_044E",
  "B16004_045E", "B16004_050E", "B16004_051E", "B16004_052E", "B16004_055E",
  "B16004_056E", "B16004_057E", "B16004_060E", "B16004_061E", "B16004_062E",
  "B16004_065E", "B16004_066E", "B16004_067E", "B16004_008E", "B16004_013E",
  "B16004_018E", "B16004_023E", "B16004_030E", "B16004_035E", "B16004_040E",
  "B16004_045E", "B16004_052E", "B16004_057E", "B16004_062E", "B16004_067E",
  "B16004_001E", "B25038_004E", "B25038_011E", "B25038_004E", "B25038_011E"
)

chi_acs <- tidycensus::get_acs(  geography = "tract"
                               , state = "IL"
                               , county = "Cook County"
                               , variables = vars_acs
                               , year = 2011
                               , output = "wide"
                              )

# Wrangle the data ############################################################

# herfindahl or fractionalization index
f_index <- function(x) 1 - rowSums(x**2)

# factor analysis
fac <- function(x, factors, rotation = "varimax", scores = "regression") {
  ok <- complete.cases(x)
  fa <- factanal(x[ok, ], factors, rotation = rotation, scores = scores)
  score <- rep(NA, fa$n.obs + sum(!ok))
  score[ok] <- fa$scores
  return(score)
}

# recode Decennial Census
chi_sf1 <- chi_sf1 %>%
  transmute(
    CT_CODE = GEOID,
    pop = P001001,
    hh = P015001,
    race_white = P005003,
    race_black = P005004,
    race_asian = P005006,
    race_hisp = P005010,
    race_other = P005005 + P005007 + P005008 + P005009,
    p_race_white = race_white / pop,
    p_race_black = race_black / pop,
    p_race_asian = race_asian / pop,
    p_race_hisp = race_hisp / pop,
    p_race_other = race_other / pop,
    age_15_35_male = P012006 + P012007 + P012008 + P012009 + P012010 +
      P012011 + P012012,
    hhi = f_index(cbind(
      p_race_white, p_race_black, p_race_asian,
      p_race_hisp, p_race_other
    )),
    geometry
  )

# recode ACS
chi_acs <- chi_acs %>%
  transmute(
    CT_CODE = GEOID,
    occ_management = C24010_004E + C24010_040E,
    occ_universe = C24010_001E,
    emp_unemployed = B23025_005E,
    emp_civ_lab_force = B23025_003E,
    poverty_universe = B17021_001E,
    poverty = B17021_002E,
    edu_hs = B15002_011E + B15002_028E,
    edu_universe = B15002_001E,
    hh_single_mother = B11003_016E,
    hh = B11001_001E,
    hu = B25001_001E,
    hu_occupied_renter = B25003_003E,
    hu_occupied = B25002_002E,
    hu_vacant = B25002_003E,
    lang_universe = B16004_001E,
    lang_eng_limited = # Speak Eng less than "very well"
        B16004_006E + B16004_007E + B16004_008E +
        B16004_011E + B16004_012E + B16004_013E +
        B16004_016E + B16004_017E + B16004_018E +
        B16004_021E + B16004_022E + B16004_023E +
        B16004_028E + B16004_029E + B16004_030E +
        B16004_033E + B16004_034E + B16004_035E +
        B16004_038E + B16004_039E + B16004_040E +
        B16004_043E + B16004_044E + B16004_045E +
        B16004_050E + B16004_051E + B16004_052E +
        B16004_055E + B16004_056E + B16004_057E +
        B16004_060E + B16004_061E + B16004_062E +
        B16004_065E + B16004_066E + B16004_067E,
    lang_eng_no = B16004_008E + B16004_013E + B16004_018E + B16004_023E +
        B16004_030E + B16004_035E + B16004_040E + B16004_045E +
        B16004_052E + B16004_057E + B16004_062E + B16004_067E,
    lang_universe = B16004_001E,
    housing_moved_2000_2009 = B25038_004E + B25038_011E,
    housing_moved_2000_2009 = B25038_004E + B25038_011E,
    p_occ_management = occ_management / occ_universe,
    p_emp_unemployed = emp_unemployed / emp_civ_lab_force,
    p_poverty = poverty / poverty_universe,
    p_edu_hs = edu_hs / edu_universe,
    p_single_mother = hh_single_mother / hh,
    p_hu_occupied_renter = hu_occupied_renter / hu_occupied,
    p_hu_vacant = hu_vacant / hu,
    p_lang_eng_limited = lang_eng_limited / lang_universe,
    p_lang_eng_no = lang_eng_no / lang_universe,
    p_housing_moved_2000_2009 = housing_moved_2000_2009 / hu_occupied,
    
    # concentrated disadv, residential instability and immigrant concentration
    
    con_disadv = fac(cbind(
                            p_occ_management, p_emp_unemployed, p_poverty,
                            p_edu_hs, p_single_mother
                          ), factors = 1),
    res_instab = fac(cbind(
                            p_hu_occupied_renter, p_housing_moved_2000_2009,
                            p_hu_vacant
                          ), factors = 1),
    immi_con = rowMeans(cbind(p_lang_eng_limited, p_lang_eng_no))
  ) 

chi_acs <- chi_acs %>%
  select(CT_CODE, con_disadv, res_instab, immi_con)

# join decennial census with ACS data
chi_ct <- chi_sf1 %>% left_join(chi_acs, by = "CT_CODE")

# Merge with crime data  ################################################
chi_crime <- read_csv("http://jlegewie.com/files/chi-violent-crime-bg-2011.csv"
                      , col_types = list(col_character(), col_integer())
                    )

chi_crime <- chi_crime %>%
  mutate(CT_CODE = str_sub(BG_CODE, 1, 11)) %>%
  group_by(CT_CODE) %>%
  summarize(crime_violent = sum(crime_violent))

chi_ct <- chi_ct %>% left_join(chi_crime, by = "CT_CODE")

# restrict our data to Chicago.
chi_ct <- chi_ct %>% filter(complete.cases(crime_violent))

# Boundary detection ################################################
vars <- c("p_race_white", "p_race_black", "p_race_hisp", "p_race_asian")

#sp do not support empty geometries
sum(st_is_empty(chi_ct))
# [1] 1
chi_ct <- chi_ct[!st_is_empty(chi_ct), ]


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

bdr <- areal_wombling(  chi_ct
                      , vars
                      , threshold = NA #{NA}= fuzzy wombling, {0,1}= crips wombling
                      ) 


# Mapping ################################################
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
# Plot 1: Areal units with proportion African-American
scale_color <- col_numeric(c("#F1EEF6", "#034E7B"), domain = c(0, 1))
plot(as(chi_ct, "Spatial"),
  col = scale_color(chi_ct$p_race_black),
  lwd = 0.01, bor = "white"
)
vals <- quantile(chi_ct$p_race_black, probs = c(0, 0.25, .5, 0.75, 1), na.rm = TRUE)
legend(-87.87, 41.73,
  legend = c("0%", "25%", "50%", "75%", "100%"),
  fill = scale_color(vals), cex = 0.8,
  box.lty = 0, border = "#00000000", title = "Percent Black", title.adj = 3.5
)
# Plot 2: Boundaries
plot(bdr, lwd = rescale(bdr$p_race_black_blv, to = c(0.1, 1.25)))
# The spatial lines object does not include Chicago's city boundaries. Let's add them
chi <- st_union(chi_ct)
plot(as(chi, "Spatial"), lwd = 0.5, add = TRUE)
# Legend showing the scale of boundary values
legend(-87.87, 41.73,
  legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
  lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1.25)),
  cex = 0.8, box.lty = 0, border = "#00000000",
  title = "Boundary Value", title.adj = 3.5
)

# aggregate from line segments to block group level
chi_ct_blv <- bind_rows(
  group_by(bdr@data, i) %>%
    summarise_at(vars(ends_with("blv")), max, na.rm = TRUE),
  group_by(bdr@data, j) %>%
    summarise_at(vars(ends_with("blv")), max, na.rm = TRUE) %>% dplyr::rename(i = j)
) %>%
  group_by(i) %>%
  summarise_at(vars(ends_with("blv")), max, na.rm = TRUE)
chi_ct <- bind_cols(chi_ct, select(chi_ct_blv, -i))

# compare the boundary values for spatial lines (border line segments) with the
# boundary values for areal units.
# for spatial lines (border line segments) with the boundary values for areal units.
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))

# Plot 1: Boundary values for border line segments
plot(bdr, lwd = rescale(bdr$p_race_black_blv, to = c(0.1, 1.25)))
chi <- st_union(chi_ct)
plot(as(chi, "Spatial"), lwd = 0.5, add = TRUE)
legend(-87.87, 41.73,
  legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
  lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1.25)),
  cex = 0.8, box.lty = 0, border = "#00000000",
  title = "Boundary Value", title.adj = 3.5
)
# Plot 2: Boundary values for border line segments for areal units
sel <- is.finite(chi_ct$p_race_black_blv)
scale_color <- col_numeric(c("#F1EEF6", "#034E7B"), domain = c(0, 1))
cols <- scale_color(chi_ct$p_race_black_blv[sel])
plot(as(chi_ct[sel, ], "Spatial"), lwd = 0.1, col = cols, bor = "white")
legend(-87.87, 41.73,
  legend = c("0.0", "0.25", "0.5", "0.75", "1.0"),
  fill = scale_color(c(0, 0.25, 0.5, 0.75, 1)), cex = 0.8, box.lty = 0,
  border = "#00000000", title = "Boundary Value", title.adj = 3.5
)



# Spatial regression ################################################
f1 <- "crime_violent ~ edge_wombling_race + log(pop) + p_race_black + p_race_hisp +
p_race_asian + con_disadv + res_instab + immi_con + hhi + age_15_35_male"
m1 <- glm.nb(f1, data = chi_ct)
stargazer(m1, type = "text", no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
