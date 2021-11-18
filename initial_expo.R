rm(list = ls())

# Checking out NOAA extreme weather data for 2019
weather19 <- read.csv("./src_data/NOAA_storm_events_2019.csv")
names(weather19)

keep_cols <- c("EPISODE_ID", "STATE", "STATE_FIPS", "EVENT_TYPE", "CZ_TYPE",
               "CZ_FIPS", "CZ_NAME", "BEGIN_DATE_TIME", "END_DATE_TIME",
               "INJURIES_DIRECT", "INJURIES_INDIRECT", "DEATHS_DIRECT",
               "DEATHS_INDIRECT", "DAMAGE_PROPERTY", "DAMAGE_CROPS", "SOURCE",
               "FLOOD_CAUSE", "CATEGORY", "MAGNITUDE", "MAGNITUDE_TYPE")
weather19 <- weather19[, names(weather19) %in% keep_cols]
names(weather19) <- tolower(names(weather19))

# QA Checks
sum(table(weather19$episode_id) > 1) # So this has some higher group meaning
length(unique(weather19$cz_name))
table(weather19$cz_type)

length(unique(weather19$cz_name))
length(unique(weather19$cz_fips)) # Many less FIPS than there are names

length(unique(weather19[weather19$cz_type == "C", "cz_fips"]))

# From spot checks I do not trust the cz_fips column - try joining by county
# name and hopefully it is not too terrible

# Mapping below from https://www.weather.gov/gis/ZoneCounty for getting zones
# FIPS codes
cz_map <- read.csv("./src_data/zones_to_counties_weather_gov.csv")
names(cz_map) <- c("state", "zone", "cwa", "name", "state_zone", "county", "fips",
                   "time_zone", "fe_area", "lat", "long")
state_map <- read.csv("./src_data/state_names_codes.csv")

cz_map <- merge(cz_map, state_map, by.x = "state", by.y = "Code")
cz_map <- cz_map[, c("zone", "cwa", "name", "state_zone", "county", "fips",
                     "State")]
names(cz_map) <- c("zone", "cwa", "name", "state_zone", "county", "fips",
                   "state")
cz_map$state <- toupper(cz_map$state)
cz_map$name <- toupper(cz_map$name)

# Should I take out the words "COUNTY" and "(C)"? In brief testing it looks like
# removing them actually hurts the joined percentage...

unique(cz_map[grep("COUNTY|\\(C\\)", cz_map$name), "name"])
unique(weather19[grep("COUNTY|\\(C\\)", weather19$cz_name), "cz_name"])


# Remove those words from both 
cz_map$name <- gsub("COUNTY|\\(C\\)", "", cz_map$name)
cz_map$name <- trimws(cz_map$name)

weather19$cz_name <- gsub("COUNTY|\\(C\\)", "", weather19$cz_name)
weather19$cz_name <- trimws(weather19$cz_name)


unique(cz_map[grep("COUNTIES", cz_map$name), "name"])
unique(weather19[grep("COUNTIES", weather19$name), "cz_name"])


# I *THINK* that cz_map includes the state fips number in the fips column,
# take only the last three digits so that it matches the weather19 data
cz_map$fips_c <- regmatches(cz_map$fips,
                            gregexpr("([0-9][0-9][0-9])$",cz_map$fips))
weather19$fips_c <- sprintf('%03d', weather19$cz_fips)
  

# TEST joining
test <- merge(weather19, cz_map, by.x = c("state", "cz_name", "fips_c"),
              by.y = c("state", "name", "fips_c"))

# Not terrible for a first pass, over 88% of events are preserved, only
# about 71% of counties present in the original weather data set though
nrow(test) / nrow(weather19)
length(unique(test$cz_name)) / length(unique(weather19$cz_name))


# Who didn't match?
no_match <- weather19[!weather19$cz_name %in% test$cz_name, ]
unique(no_match$cz_name)
length(unique(weather19$cz_name)) - length(unique(test$cz_name))

no_match_c <- no_match[, c("state", "cz_type", "cz_fips", "cz_name")]
no_match_c <- no_match_c[!duplicated(no_match_c), ]
no_match_c[table(no_match_c$cz_name) > 1, ]


# NEXT STEP: Check those that did not make it. Then bring in Census, election
# data. Combine everything and start EDA.





