# The purpose of this script is to standardize the county and state name and
# FIPS columns in the NOAA weather data for future merging with other data sets,
# to check if all US counties identified by the Census are accounted for, and
# to run some basic QA before writing a cleaned file.

rm(list = ls())

# TODO: (Maybe) Run over 2018, 2020 data as well (in which case I should
#               build some of this work out as functions)

# --------------------------- NOAA WEATHER ------------------------------------

# Read in the NOAA extreme weather events 2019 data, discard unnecessary cols,
# reference county FIPS codes from the Census
weather19 <- read.csv("./src_data/NOAA_storm_events_2019.csv")
fips_map <- read.csv("./processed_data/census_fips_cleaned.csv")

names(weather19)

keep_cols <- c("EVENT_ID", "EPISODE_ID", "STATE", "STATE_FIPS", "EVENT_TYPE",
               "CZ_TYPE", "CZ_FIPS", "CZ_NAME", "BEGIN_DATE_TIME",
               "END_DATE_TIME", "INJURIES_DIRECT", "INJURIES_INDIRECT",
               "DEATHS_DIRECT", "DEATHS_INDIRECT", "DAMAGE_PROPERTY",
               "DAMAGE_CROPS", "SOURCE", "FLOOD_CAUSE", "CATEGORY", "MAGNITUDE",
               "MAGNITUDE_TYPE")
weather19 <- weather19[, names(weather19) %in% keep_cols]
names(weather19) <- tolower(names(weather19))  # Makes typing col names easier


### QA CHECKS / CLEANUP
sum(table(weather19$event_id) > 1)  # Data has unique identifier for each event
sum(table(weather19$episode_id) > 1)  # Episode ID - not unique
length(unique(weather19$cz_name))
# table(weather19$event_type)

# Note that there are two different geographic levels for weather event entry:
# zones and counties
table(weather19$cz_type)  # More entries at county level than zone level
sapply(weather19, function(x){sum(is.na(x))})  # Are there any NA values?


# Standardize name and check state FIPS column
weather19 <- weather19[weather19$state %in% unique(fips_map$state), ]
all(sort(unique(weather19$state_fips)) ==  # All states represented in weather19
      sort(unique(fips_map$s_fips)))


# Read in an additional file for mapping between counties and zones (obtained
# from weather.gov, full link: https://www.weather.gov/gis/ZoneCounty).
# Ultimately I want to be able to have a row for each county affected by a
# weather event (whether that affected a zone which is a subset or super-set of
# the county)
cz_map <- read.csv("./src_data/zones_to_counties_weather_gov.csv", header = F)
names(cz_map) <- c("state", "zone", "cwa", "name", "state_zone", "county",
                   "fips", "time_zone", "fe_area", "lat", "long")
cz_map <- cz_map[, c("state", "zone", "name", "county", "fips")]

# Check for duplicates, remove them from the mapping (I think some of these
# contain finer-granularity details than county-zone level, which I don't need)
cz_map$county <- toupper(cz_map$county)
cz_map$name <- toupper(cz_map$name)
sum(duplicated(cz_map))
cz_map <- cz_map[!duplicated(cz_map), ]

# Separate state FIPS from county FIPS in cz_map for consistency with other
# data sets
cz_map$fips <- sprintf("%05s", cz_map$fips)  # Pad FIPS with 0 for separation
# State FIPS is the first two numbers in the full 5-digit FIPS...
cz_map$s_fips <- regmatches(cz_map$fips, gregexpr("^([0-9]{2})", cz_map$fips))
# ... and county FIPS is the last three numbers
cz_map$c_fips <- regmatches(cz_map$fips, gregexpr("([0-9]{3})$", cz_map$fips))
cz_map$s_fips <- gsub("^0", "", cz_map$s_fips)  # Remove extra 0's 
cz_map$c_fips <- gsub("^0{1,2}", "", cz_map$c_fips)

# Only keep continental US states + HI, AK. Double check that the cz_map
# counties exist in the Census data
cz_map <- cz_map[cz_map$s_fips %in% unique(fips_map$s_fips), ]
all(sort(unique(as.numeric(cz_map$s_fips))) ==  # All states represented
    sort(unique(fips_map$s_fips)))


# Verified below that there are sometimes multiple zones recorded for the same
# county, as well as cases with multiple counties recorded for the same zone.
# Although a weather event occurring in a zone does not mean that the weather
# event necessarily affected all individuals across the county, many types of
# weather events are only recorded at the zone level (ex. wildfires,
# hurricanes). Since we are working at the county level and not the individual
# level with the climate opinion data anyways, I would argue that severe weather
# in a zone has an impact on the county that it is a part of.
test_zct <- aggregate(zone ~ s_fips + c_fips,
                      data = cz_map,
                      function(x) length(unique(x)))
table(test_zct$zone)
test_cct <- aggregate(c_fips ~ s_fips + zone,
                      data = cz_map,
                      function(x) length(unique(x)))
table(test_cct$c_fips)

# Make a flag for when the county name == the zone name to have for
# reference
cz_map$same_cz <- cz_map$county == cz_map$name
table(cz_map$same_cz)


# Is it ever the case that the same zone number has different names (within the
# same state)? Making sure that joining on state and zone codes will be
# enough to distinguish every case.
zone_comp <- cz_map[!duplicated(cz_map[, c("state", "zone", "name")]), ]
zone_comp <- aggregate(name ~ zone + state, data = zone_comp,
                       function(x){length(unique(x))})
zone_comp[zone_comp$name > 1, ] 

# These look like typos to me, so I am assuming zone # is unique within a state.
# Also note that each row is for a different county that falls within the zone
unique(cz_map[cz_map$state == "CO" & cz_map$zone %in% c(65, 72),
              c("name", "county")])

# Ran into one instance where a zone was mistakenly flagged as a county - fix
# this in the weather19 data
weather19[weather19$event_id == "857412", "cz_type"] <- "Z"


### MATCH ZONES TO COUNTIES
# Separate out rows for zones and rows for counties from weather19 into two
# different df's 
w19_c <- weather19[weather19$cz_type == "C", ]
w19_z <- weather19[weather19$cz_type == "Z", ]

# Joining on state FIPS, zone code to attach county FIPS to the zone entries
# of the weather data
w19_z2 <- merge(w19_z, cz_map[, c("s_fips", "zone", "c_fips", "same_cz",
                                  "county")],
                by.x = c("state_fips", "cz_fips"),
                by.y = c("s_fips", "zone"))


# I expect the merge to result in more rows than we started with because
# sometimes a zone refers to an area larger than a county, which matches to
# multiple counties. In this case we DO want "duplicated" rows so that we have a
# record of this weather event for each county affected, but check that this is
# the only duplication case.
duped_ids <- names(table(w19_z2$event_id)[table(w19_z2$event_id) > 1])
duped_ids <- w19_z2[w19_z2$event_id %in% duped_ids, ]
t1 <- aggregate(c_fips ~ event_id, data = duped_ids,
                function(x){length(unique(x))}) # Count unique c_fips per event
t2 <- as.data.frame(table(duped_ids$event_id)) # Count # each event_id duped
names(t2) <- c("event_id", "c_fips")
t2$event_id <- as.integer(as.character(t2$event_id))
all.equal(t1, t2)  # Confirming duplicates match # unique c_fips per event

# Are there event IDs that did NOT make it into the merge and are obscured by
# the number of duplicated events? 
(n_lost <- sum(!unique(w19_z$event_id) %in% unique(w19_z2$event_id)))  # Yes.
n_extra_dup <- nrow(duped_ids) - length(unique(duped_ids$event_id))
nrow(w19_z) - n_lost + n_extra_dup == nrow(w19_z2)

# Although not a large number are lost, certain states seem to have more
# problems joining - worried about CA, NM, UT, HI.
lost <- w19_z[!w19_z$event_id %in% w19_z2$event_id, ]
# CA, NM, UT, WY are missing from the join at higher proportions than the
# proportions that the make up in the original zone data - not great.
round(table(lost$state)/nrow(lost)*100, 2)
round(table(w19_z[w19_z$state %in% lost$state, "state"])/nrow(w19_z)*100, 2)


# Some of these have the word "County" and for whatever reason their zone codes
# are not matching to the mapping file properly. Not sure which end this
# problem stems from (mapping file or NOAA file).
# View(cz_map[cz_map$state == "CA", ])
# View(lost[lost$state == "CALIFORNIA", ])
# View(fips_map[fips_map$state == "CALIFORNIA", ])

# Merging again by just state and zone name matches a good number (about
# 1/3). More than I had hoped still remain unmatched, but it looks like they
# will require individual string adjustments to infer a match, or just don't
# exist in the county-zone map. If I have more time towards the end of the
# project I will come back and try to tinker to get as many matches as possible.
lost$cz_name2 <- gsub(" AREA", "", lost$cz_name)

temp <- merge(lost,
              cz_map[, c("s_fips", "c_fips", "same_cz", "county", "name")],
              by.x = c("state_fips", "cz_name2"),
              by.y = c("s_fips", "name"), all.x = F, all.y = F)
length(unique(temp$event_id)) / length(unique(lost$event_id))

temp <- temp[, c("state", "state_fips", "county", "c_fips", "cz_name",
                 "cz_fips", "event_id", "episode_id", "begin_date_time",
                 "end_date_time", "injuries_direct", "injuries_indirect",
                 "deaths_direct", "deaths_indirect", "damage_property",
                 "damage_crops", "source", "magnitude", "magnitude_type",
                 "flood_cause", "category", "same_cz", "event_type")]
names(temp) <- c("state", "s_fips", "county", "c_fips", "zone", "z_fips",
                 "event_id", "episode_id", "begin_date_time", "end_date_time",
                 "injuries_direct", "injuries_indirect", "deaths_direct", 
                 "deaths_indirect", "damage_property", "damage_crops",
                 "source", "magnitude", "magnitude_type", "flood_cause",
                 "category", "same_cz", "event_type") 


# Choose relevant columns for merging the zone-level rows (now with county fips)
# back with county-level rows
w19_z2 <- w19_z2[, c("state", "state_fips", "county", "c_fips", "cz_name",
                     "cz_fips", "event_id", "episode_id", "begin_date_time",
                     "end_date_time", "injuries_direct", "injuries_indirect",
                     "deaths_direct", "deaths_indirect", "damage_property",
                     "damage_crops", "source", "magnitude", "magnitude_type",
                     "flood_cause", "category", "same_cz", "event_type")]
names(w19_z2) <- c("state", "s_fips", "county", "c_fips", "zone", "z_fips",
                   "event_id", "episode_id", "begin_date_time", "end_date_time",
                   "injuries_direct", "injuries_indirect", "deaths_direct", 
                   "deaths_indirect", "damage_property", "damage_crops",
                   "source", "magnitude", "magnitude_type", "flood_cause",
                   "category", "same_cz", "event_type")

sum(unique(w19_z2$event_id) %in% unique(temp$event_id))
sum(unique(temp$event_id) %in% unique(w19_z2$event_id))
w19_z2 <- rbind(w19_z2, temp)

# Match rate to county FIPS - successfully matched a county FIPS code to
# 95% of zone-level weather events
length(unique(w19_z2$event_id)) / length(unique(w19_z$event_id))

# Check final proportions of those states that I was worried about losing too
# large a proportion of records for in this matched df compared to the original.
# Doesn't look like there is an overall problem of losing too many records for
# some states.
round(table(w19_z2[w19_z2$state %in% lost$state, "state"])/nrow(w19_z2)*100, 2)
round(table(w19_z[w19_z$state %in% lost$state, "state"])/nrow(w19_z)*100, 2)



### QA COUNTY-LEVEL ROWS
# Check that all state/county FIPS are valid according to the Census
cs_combos <- w19_c[, c("cz_fips", "state_fips")]
cs_combos <- cs_combos[!duplicated(cs_combos), ]
sum(sapply(1:nrow(cs_combos), 
           function(x){
             cs_combos[x, "state_fips"] %in% unique(fips_map$s_fips) &
               cs_combos[x, "cz_fips"] %in% unique(fips_map$c_fips)})) ==
  nrow(cs_combos) # Long-winded way of checking validity of all FIPS pairs

# Add columns for zone info (will be NA for these rows, which are counties)
# just to keep a record of which events were originally recorded for which
# geographic area
w19_c$zone <- NA
w19_c$z_fips <- NA
w19_c$same_cz <- NA  # Recall this is the flag for county name == zone name
w19_c <- w19_c[, c("state", "state_fips", "cz_name", "cz_fips", "zone",
                   "z_fips", "event_id", "episode_id", "begin_date_time",
                   "end_date_time", "injuries_direct", "injuries_indirect",
                   "deaths_direct", "deaths_indirect", "damage_property",
                   "damage_crops", "source", "magnitude", "magnitude_type",
                   "flood_cause", "category", "same_cz", "event_type")]
names(w19_c) <- c("state", "s_fips", "county", "c_fips", "zone", "z_fips",
                  "event_id", "episode_id", "begin_date_time", "end_date_time",
                  "injuries_direct", "injuries_indirect", "deaths_direct",
                  "deaths_indirect", "damage_property", "damage_crops",
                  "source", "magnitude", "magnitude_type", "flood_cause",
                  "category", "same_cz", "event_type")


### COMBINE COUNTY AND ZONE LEVEL CLEANED ROWS, SAVE CSV
w19 <- rbind(w19_c, w19_z2)

# Final proportion of events that I was able to map to a county FIPS code:
# over 98%! 
length(unique(w19$event_id)) / length(unique(weather19$event_id))

# Save this csv
write.csv(w19, "./processed_data/noaa_weather19_cleaned.csv", row.names = F)



