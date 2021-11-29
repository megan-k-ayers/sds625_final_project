# The purpose of this script is to merge the 2019 ACS demographic data, the
# 2020 election data, the 2019 NOAA weather events data, and the YCOM
# climate change survey data into one data frame to save as a CSV for analysis.

rm(list = ls())

c <- read.csv("./processed_data/census_data_cleaned.csv", header = T)
e <- read.csv("./processed_data/election2020_cleaned.csv", header = T)
w <- read.csv("./processed_data/noaa_weather19_cleaned.csv", header = T)
y <- read.csv("./processed_data/ycom_cleaned.csv", header = T)


# Attempt to merge all of this (may reduce size later by filtering to most
# "extreme" weather events but why not start with all info available)
x <- merge(c, y[, -which(names(y) %in% c("state", "county"))],
           by = c("c_fips", "s_fips"))
x <- merge(x, e[, -which(names(y) %in% c("state", "county"))],
           by = c("c_fips", "s_fips"))
x <- merge(x, w[, -which(names(y) %in% c("state", "county"))],
           by = c("c_fips", "s_fips"))

# Double check that the only weather events not included in final join are
# those in Alaska (where I couldn't match electiond data)
unique(w[!w$event_id %in% x$event_id, "state"]) == "ALASKA"

# Save to CSV
write.csv(x, "./processed_data/merged_data.csv", row.names = F)

