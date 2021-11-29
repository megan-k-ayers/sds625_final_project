# The purpose of this script is to collect demographic information at the county
# level from the most recent Census Bureau ACS. 

rm(list = ls())
library(tidycensus)

readRenviron("~/.Renviron")

# --------------------------- DEMOGRAPHICS ------------------------------------

# List the variables to be pulled from ACS, see link for full list of
# codes: https://www.census.gov/data/developers/data-sets/acs-1year.html

my_variables <- c(# Total, Male, Female
  "B01001_001", "B01001_002", "B01001_026", 
  
  # Median age, Male age, Female age
  "B01002_001", "B01002_002", "B01002_003", 
  
  # Race: White, Black, Native, Asian, Pacific, Other, 2+
  # (note that these all represent single-race selections except for 2+)
  "B02001_002", "B02001_003", "B02001_004", 
  "B02001_005", "B02001_006", "B02001_007", "B02001_008", 
  
  # Ethnicity: White non-Hispanic, Hispanic, white Hispanic, black Hispanic
  "B03002_003", "B03002_012", "B03002_013", 'B03002_014',
  
  # Median Household Income (in the past year)
  "B19013_001"
  
)

x = as.data.frame(get_acs(geography = "county",
                  variables = my_variables,
                  year = 2019))

# Perform a reshape so that each variable is a column
x <- x[, c("GEOID", "variable", "estimate")]
names(x) <- c("fips", "var", "value")
x <- reshape(x, idvar = c("fips"), timevar = "var", direction = "wide")
names(x) <- gsub("value.", "", names(x))

# Split out state and county FIPS codes
x$fips <- sprintf("%05s", x$fips)
x$c_fips <- as.integer(gsub("^[0-9]{2}", "", x$fips))
x$s_fips <- as.integer(gsub("[0-9]{3}$", "", x$fips))


# The tidycensus package comes with its own FIPS code mapper, nice
fips_map <- fips_codes
fips_map$state_code <- as.integer(fips_map$state_code)
fips_map$county_code <- as.integer(fips_map$county_code)

# Filter out "states" that are actually territories not in the continental US
fips_map <- fips_map[! fips_map$state_code >= 60, ]

# Check that counties with demographic data can all be matched by the FIPS map
test <- merge(x, fips_map, by.x = c("c_fips", "s_fips"),
              by.y = c("county_code", "state_code"))

# Only excluding data from Puerto Rico, which is on purpose since this analysis
# focuses on continental US + Hawaii 
unique(x[!x$fips %in% test$fips, "s_fips"])

# Check which counties in the FIPS map aren't represented in the demographic
# data
test <- merge(fips_map, x, by.x = c("county_code", "state_code"),
              by.y = c("c_fips", "s_fips"))

# This Alaskan county recently had a name change which might explain the issue.
# I won't worry about it though because I am planning to drop Alaskan counties
# altogether due to the inability to merge their voting districts to counties.
# The Virginian county used to be an independent city but is now part of 
# Bedford county which is its own entry
unique(fips_map[!fips_map$county %in% test$county, ])


# Clean up column names of Census demographic data and save to CSV
# Also do want to permanently merge on fips_map to get state/county names for
# convenience
x <- merge(x, fips_map, by.x = c("c_fips", "s_fips"),
           by.y = c("county_code", "state_code"))
x <- x[, -(which(names(x) %in% c("fips", "state")))]

# I don't like hard-coding the column indices like long lists of variables
# less, and want to reorder the columns for consistency with other data sets
x <- x[, c(21, 2, 22, 1, 3:20)]
names(x) <- c("state", "s_fips", "county", "c_fips", "ttl_pop", "male_pop",
              "female_pop", "med_age", "male_med_age", "female_med_age",
              "white_pop", "black_pop", "native_pop", "asian_pop",
              "pacific_pop", "other_race_pop", "mult_race_pop",
              "white_nonhisp_pop", "hisp_ttl_pop", "white_hisp_pop",
              "black_hisp_pop", "med_income")


write.csv(x, "./processed_data/census_data_cleaned.csv", row.names = FALSE)

