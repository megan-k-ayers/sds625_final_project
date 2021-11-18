# The purpose of this script is to standardize the county and state name and
# FIPS columns in the NOAA weather data for future merging with other data sets,
# to check if all US counties identified by the Census are accounted for, and
# to run some basic QA before writing a cleaned file.

rm(list = ls())

# QUESTIONS THAT NEED TO BE RESOLVED:
# - When are zones and counties the same? According to weather.gov, zones are
#   usually the same as counties but often subsets:
#   https://www.weather.gov/gis/PublicZones. 


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
names(weather19) <- tolower(names(weather19))


# Initial QA Checks
sum(table(weather19$event_id) > 1)  # Unique identifier for each event
sum(table(weather19$episode_id) > 1)  # Has higher group meaning - not unique
length(unique(weather19$cz_name))
table(weather19$event_type)

# Note that there are two different geographic levels for weather event entry:
# zones and counties
table(weather19$cz_type)  # More entries at county level than zone level
sapply(weather19, function(x){sum(is.na(x))})  # Are there any NA values?


# Standardize name and check state FIPS column
weather19 <- weather19[weather19$state %in% unique(fips_map$state), ]
all(sort(unique(weather19$state_fips)) ==  # All states represented
      sort(unique(fips_map$s_fips)))


# Read in an additional file for mapping between county and "zones" (obtained
# from weather.gov, full link: https://www.weather.gov/gis/ZoneCounty)
cz_map <- read.csv("./src_data/zones_to_counties_weather_gov.csv", header = F)
names(cz_map) <- c("state", "zone", "cwa", "name", "state_zone", "county", "fips",
                   "time_zone", "fe_area", "lat", "long")
cz_map <- cz_map[, c("state", "zone", "name", "county", "fips")]

# Check for duplicates, remove them from the mapping
cz_map$county <- toupper(cz_map$county)
cz_map$name <- toupper(cz_map$name)
sum(duplicated(cz_map))
cz_map <- cz_map[!duplicated(cz_map), ]

# Separate state fips from county fips in cz_map
cz_map$fips <- sprintf("%05s", cz_map$fips)  # Pad FIPS with 0 for separation
cz_map$s_fips <- regmatches(cz_map$fips, gregexpr("^([0-9]{2})",cz_map$fips))
cz_map$c_fips <- regmatches(cz_map$fips, gregexpr("([0-9]{3})$",cz_map$fips))
cz_map$s_fips <- gsub("^0", "", cz_map$s_fips)
cz_map$c_fips <- gsub("^0{1,2}", "", cz_map$c_fips)

# Only keep continental US states + HI, AK. Double check that the cz_map
# counties exist in the Census data
cz_map <- cz_map[cz_map$s_fips %in% unique(fips_map$s_fips), ]
all(sort(unique(as.numeric(cz_map$s_fips))) ==  # All states represented
    sort(unique(fips_map$s_fips)))


# Verified below that there are sometimes multiple zones recorded for the same
# county, as well as cases with multiple counties recorded for the same zone.
# TODO: I will need to decide if all of these cases are appropriate to consider
# at the county level when it comes to modeling.
test_zct <- aggregate(zone ~ s_fips + c_fips,
                      data = cz_map,
                      function(x) length(unique(x)))
table(test_zct$zone)
test_cct <- aggregate(c_fips ~ s_fips + zone,
                      data = cz_map,
                      function(x) length(unique(x)))
table(test_cct$c_fips)


# Is it ever the case that the same zone number has different names (within the
# same state)? Making sure that joining on state and zone codes will be
# enough to distinguish every case.
zone_comp <- cz_map[!duplicated(cz_map[, c("state", "zone", "name")]), ]
zone_comp <- aggregate(name ~ zone + state, data = zone_comp,
                       function(x){length(unique(x))})
zone_comp[zone_comp$name > 1, ] 

# These look like typos to me, so I am assuming zone # is unique within a state
# and note that each row is for a different county that falls within the zone
unique(cz_map[cz_map$state == "CO" & cz_map$zone %in% c(65, 72),
              c("name", "county")])


# How frequently are zones and counties the same? Over half the time.
sum(cz_map$name == cz_map$county) / nrow(cz_map)
cz_map$c_z_flag <- cz_map$name == cz_map$county # Flag for when county == zone

# No instances of multiple counties per zone when county name == zone name
sum(aggregate(c_fips ~ zone + s_fips,
              data = cz_map[cz_map$c_z_flag, ],
              function(x){length(unique(x))})$c_fips > 1)

# No instances of multiple zones per county when county name == zone name
sum(aggregate(zone ~ c_fips + s_fips,
              data = cz_map[cz_map$c_z_flag, ],
              function(x){length(unique(x))})$zone > 1)

# Flag when a zone encompasses multiple counties - I think it should be OK
# to map weather events in these zones to counties, though to be ideally
# rigorous I would check that these counties are fully surrounded by the zones.
temp <- aggregate(c_fips ~ zone + s_fips, data = cz_map[!cz_map$c_z_flag, ],
                  function(x){length(unique(x))})
temp$mult_cnty_z <- temp$c_fips > 1
cz_map <- merge(cz_map, temp[, c("s_fips", "zone", "mult_cnty_z")],
                by = c("s_fips", "zone"), all.x = T)
cz_map$mult_cnty_z <- ifelse(is.na(cz_map$mult_cnty_z), F, cz_map$mult_cnty_z)
 
# UGHGGH I don't think I can use zones at all. Even if zones contain multiple
# counties they don't necessarily contain the entire counties. Ex. upper
# treasure valley in Idaho

# Also flag when a county includes multiple zones. Assume that if a zone is a
# "superset" (ie contains multiple counties)
# In these cases I do NOT
# want to interpolate weather events in these zones to the county level because
# only a subset of the county potentially witnessed the event
temp <- aggregate(zone ~ c_fips + s_fips, data = cz_map[!cz_map$c_z_flag, ],
                  function(x){length(unique(x))})
temp$mult_zone_c <- temp$zone > 1
cz_map <- merge(cz_map, temp[, c("c_fips", "s_fips", "mult_zone_c")],
              by = c("c_fips", "s_fips"), all.x = T)
cz_map$mult_zone_c <- ifelse(is.na(cz_map$mult_zone_c), F, cz_map$mult_zone_c)

table(cz_map$mult_cnty_z, cz_map$mult_zone_c)
sum(cz_map$mult_cnty_z & !cz_map$mult_zone_c)



# Ran into one instance where a zone was mistakenly flagged as a county - fix
# this in the weather19 data
weather19[weather19$event_id == "857412", "cz_type"] <- "Z"


# *!*!*! Dev

# Separate into zones and counties from weather19
w19_c <- weather19[weather19$cz_type == "C", ]
w19_z <- weather19[weather19$cz_type == "Z", ]

# Joining on state FIPS, zone code to attach county fips to the zone entries
# of the weather data
w19_z2 <- merge(w19_z, cz_map, by.x = c("state_fips", "cz_fips"),
                by.y = c("s_fips", "zone"))


# I expect the merge to result in more rows than we started with because
# occasionally a zone refers to an area larger than a county, which matches to
# multiple counties. In this case we do want "duplicated" rows so that we have a
# record of this weather event for each county affected, but check that this is
# the only duplication case.
duped_ids <- names(table(w19_z2$event_id)[table(w19_z2$event_id) > 1])
duped_ids <- w19_z2[w19_z2$event_id %in% duped_ids, ]
t1 <- aggregate(c_fips ~ event_id, data = duped_ids,
                function(x){length(unique(x))}) # Count unique c_fips per event
t2 <- as.data.frame(table(duped_ids$event_id)) # Count # each event_id duped
names(t2) <- c("event_id", "c_fips")
t2$event_id <- as.integer(as.character(t2$event_id))
all.equal(t1, t2)  # Confirming duplicates match -> # unique c_fips per event

# Are there event IDs that did NOT make it into the merge and are obscured by
# the number of duplicated events? 
n_extra_dup <- nrow(duped_ids) - length(unique(duped_ids$event_id))
n_lost <- sum(!unique(w19_z$event_id) %in% unique(w19_z2$event_id))  # Yes. 
nrow(w19_z) - n_lost + n_extra_dup == nrow(w19_z2)

# Although not a large number are lost, certain states seem to have more
# problems joining - worried about CA, NM, UT, WY.
lost <- w19_z[!w19_z$event_id %in% w19_z2$event_id, ]
round(table(lost$state)/nrow(lost)*100, 2)
round(table(w19_z2[w19_z2$state %in% lost$state, "state"])/nrow(w19_z2)*100, 2)


# Some of these have the word "County" and appear to be mistakenly recorded
# as zones and not counties...
# View(cz_map[cz_map$state == "CA", ])
# View(lost[lost$state == "CALIFORNIA", ])
# View(fips_map[fips_map$state == "CALIFORNIA", ])

# Wow, merging again by just state and zone name matches a good number (about
# 1/3). But this gives me concerns about the quality of the c_fips variable!
# How do I know that that isn't just as inaccurate as the zone code for these?
test <- merge(lost, cz_map, by.x = c("state_fips", "cz_name"),
              by.y = c("s_fips", "name"))
length(unique(test$event_id)) / length(unique(lost$event_id))

lost2 <- test[!lost$event_id %in% test$event_id, ]

# I am not convinced everything joined properly, Bakersfield should be ok?!
View(fips_map[fips_map$state == "CALIFORNIA", ])
View(cz_map[cz_map$state == "CA", ])
View(lost2[lost2$state.x == "CALIFORNIA", ])

# WHY ARE THERE NA???
unique(lost2$state.x)
unique(lost2$state.y)
unique(lost2$state_fips)


# Choose relevant columns for merging the zone-level rows (now with county fips)
# back with county-level rows
w19_z2 <- w19_z2[, c("state.x", "state_fips", "county", "c_fips", "cz_name", "cz_fips",
                     "event_id", "episode_id", "begin_date_time", "end_date_time",
                     "injuries_direct", "injuries_indirect", "deaths_direct", 
                     "deaths_indirect", "damage_property", "damage_crops", "source",
                     "magnitude", "magnitude_type", "flood_cause", "category")]
names(w19_z2) <- c("state", "state_fips", "county", "c_fips", "zone", "z_fips",
                   "event_id", "episode_id", "begin_date_time", "end_date_time",
                   "injuries_direct", "injuries_indirect", "deaths_direct", 
                   "deaths_indirect", "damage_property", "damage_crops", "source",
                   "magnitude", "magnitude_type", "flood_cause", "category")
# Match rate to county FIPS:
length(unique(w19_z2$event_id)) / length(unique(w19_z$event_id))


# # QA the county-level rows
# test <- merge(w19_c, fips_map, by.x = c("state_fips", "cz_fips"),
#            by.y = c("s_fips", 'c_fips'))
# 
# library(dplyr)
# View(anti_join(w19_c, fips_map,
#                by = c("state_fips" = "s_fips", "cz_fips" = "c_fips")))
# 
# 
# w19_c$zone <- NA
# w19_c$z_fips <- NA
# w19_c <- w19_c[, c("state", "state_fips", "cz_name", "cz_fips", "zone",
#                    "z_fips", "event_id", "episode_id", "begin_date_time",
#                    "end_date_time", "injuries_direct", "injuries_indirect",
#                    "deaths_direct", "deaths_indirect", "damage_property",
#                    "damage_crops", "source", "magnitude", "magnitude_type",
#                    "flood_cause", "category")]
# names(w19_c) <- c("state", "state_fips", "county", "c_fips", "zone", "z_fips",
#                   "event_id", "episode_id", "begin_date_time", "end_date_time",
#                   "injuries_direct", "injuries_indirect", "deaths_direct", 
#                   "deaths_indirect", "damage_property", "damage_crops", "source",
#                   "magnitude", "magnitude_type", "flood_cause", "category")
# w19 <- rbind(w19_c, w19_z2)
# 
# # Check 
# w19[!w19$c_fips %in% unique(fips_map$c_fips), ]


