# The purpose of this script is to get a "ground truth" reference for names of
# all of the counties in the US and their correct FIPS codes, to use as I
# attempt to stitch multiple county-level data sources together.

rm(list = ls())

# ------------------------------ CENSUS FIPS ----------------------------------

# Read in Census 2019 FIPS code mapping to have "ground truth"
f_map <- read.csv("./src_data/census_fips_codes.csv", skip = 4)
names(f_map) <- c("summary_level", "s_fips", "c_fips",
                  "county_subdiv_fips", "place_code_fips",
                  "consolid_city_fips", "area_name")
f_map <- f_map[, c("summary_level", "s_fips", "c_fips",
                   "area_name")]

# Separate out the states to have state name attached to each county
states <- f_map[f_map$summary_level == 40,
                c("s_fips", "area_name")]
states <- states[!states$s_fips == c(72), ] # Not Puerto Rico
names(states) <- c("s_fips", "state")
f_map <- f_map[f_map$summary_level == 50, ]

# Expect to see the number below after merging (losing Puerto Rico counties)
# for the rows in f_map
nrow(f_map) - sum(f_map$s_fips == 72)  # 72 is the "state" FIPS code for PR
f_map <- merge(f_map, states, by = "s_fips")
f_map <- f_map[, c("s_fips", "c_fips", "area_name",
                   "state")]
names(f_map) <- c("s_fips", "c_fips", "county", "state")

# Make county, state names all caps
f_map$county <- toupper(f_map$county)
f_map$state <- toupper(f_map$state)

# Save this cleaned version as a csv
write.csv(f_map, "./processed_data/census_fips_cleaned.csv", row.names = FALSE)

