# The purpose of this script is to standardize the county and state columns
# in the YCOM data for future merging with other data sets, to verify that
# all US counties identified by the Census are accounted for, and to run some
# basic QA before writing a cleaned file.

rm(list = ls())

# ------------------------------- YCOM ----------------------------------------

# Read in the YCOM data, reference county FIPS codes from the Census
fips_map <- read.csv("./processed_data/census_fips_cleaned.csv")
y <- read.csv("src_data/YCOM_2020_Data.csv")
Encoding(y$GeoName) <- 'latin1'  # Takes care of an accent in NM county name

# For dev just looking at high level "climate change is happening vs not
# happening" opinion estimates
y <- y[, c("GeoType", "GeoName", "GEOID", "happening",
           "happeningOppose")]

# Only look at counties for now, separate GEOID into county and state FIPS
y <- y[y$GeoType == "County", ] # Can drop GeoType now
y <- y[, c("GeoName", "GEOID", "happening", "happeningOppose")]
y$GEOID <- sprintf('%05d', y$GEOID)  # Pad so that we can properly separate FIPS
y$FIPS_C <- as.numeric(regmatches(y$GEOID,
                                  gregexpr("([0-9][0-9][0-9])$",y$GEOID)))
y$FIPS_S <- as.numeric(regmatches(y$GEOID, gregexpr("^([0-9][0-9])",y$GEOID)))

names(y) <- c("c_name", "geoid", "happening", "happening_oppose", "c_fips",
              "s_fips")

# Double check that all counties are represented in the YCOM data (double
# check that row counts aren't the same due to some duplication)
nrow(merge(y, fips_map, by = c("c_fips", "s_fips"))) == nrow(y) # Yay 
sum(duplicated(y[, c("c_fips", "s_fips")])) == 0 # Yay 
sum(duplicated(fips_map[, c("c_fips", "s_fips")])) == 0 # Yay 

# Merge in standardized Census county and state names, check that these agree
y <- merge(y, fips_map, by = c("c_fips", "s_fips"))
state_check <- toupper(gsub(".+, ", "", y$c_name))
y$c_name <- toupper(gsub(",.*$", "", y$c_name))
sum(state_check != y$state) / nrow(y) == 0  # Do all state names agree?
sum(y$c_name == y$county) / nrow(y) == 1  # Do all county names agree?

# Perform some quick checks on the YCOM data
sapply(y, function(x){sum(is.na(x))})  # Are there any NA values in any cols?
hist(y$happening, breaks = 30)  # What is the distribution of `happening`? 
qqnorm(y$happening)
sum(table(y$geoid) > 1) == 0  # Are GEOIDs unique?

# Drop extraneous county and GEOID columns, reorder, and save cleaned version to CSV
y <- y[, c("state", "s_fips", "c_name", "c_fips", "happening",
           "happening_oppose")]
names(y) <- c("state", "s_fips", "county", "c_fips", "happening",
              "happening_oppose")

write.csv(y, "./processed_data/ycom_cleaned.csv", row.names = FALSE)
