# Data from https://www.fema.gov/about/openfema/data-sets#disaster

# x: https://www.fema.gov/openfema-data-page/fema-web-declaration-areas-v1
# y: https://www.fema.gov/openfema-data-page/fema-web-disaster-declarations-v1

rm(list = ls())
x <- read.csv("./src_data/FemaWebDeclarationAreas.csv")
y <- read.csv("./src_data/FemaWebDisasterDeclarations.csv")


# ----------------- Check out FEMA disaster declaration file -----------------
head(y)  # Has disaster number, incident type (ex hurricane, flood) by state
table(y$incidentType)

# Only keep weather related incidents
incidents <- c("Coastal Storm", "Drought", "Fire", "Flood", "Freezing",
               "Hurricane", "Severe Ice Storm", "Severe Storm(s)", "Snow",
               "Tornado", "Typhoon")
y <- y[y$incidentType %in% incidents, ]
y <- y[, c("disasterNumber", "declarationDate", "disasterName", "stateCode",
          "stateName", "incidentType")]
length(unique(y$disasterNumber)) == nrow(y)  # DisasterNumber is unique here

head(y$declarationDate)
y$year <- gsub("-.*", "", y$declarationDate)

# Can't safely use 2020, survey responses were gathered just through Spring
# 2020
y <- y[y$year %in% c(2018, 2019), ]
table(y$incidentType)


# ------------------- Check out FEMA declaration area file -------------------
head(x)  # Has disaster number, county name
length(unique(x$id)) # id is unique
# Disaster number NOT unique for this data set, the same disaster can apply to
# multiple counties and has multiple types of disaster assistance available...
hist(table(aggregate(id ~ disasterNumber, data = x, length)$id))

# ...but I only care about one row for each county, so just keep that
# information.
x <- x[, c("disasterNumber", "stateCode", "stateName", "placeCode",
              "placeName")]
x <- x[!duplicated(x), ]


# --------- Merge to get county-level information about incident type ---------
z <- merge(x, y, by = c("disasterNumber", "stateCode", "stateName"),
           all.y = T)

z <- z[, c("stateCode", "stateName", "placeCode", "placeName", "incidentType",
           "year")]
z$placeName <- toupper(z$placeName)
z$placeName <- gsub("[\\(\\)]", "", z$placeName)
z$stateName <- toupper(z$stateName)

# Get FIPS codes for the counties instead of the FEMA designated placeCode
fips_map <- read.csv("./processed_data/census_fips_cleaned.csv")
z <- z[z$stateName %in% fips_map$state, ]  # Limit to states + DC

z$temp_id <- 1:nrow(z)

w <- merge(z, fips_map, by.x = c("placeName", "stateName"),
           by.y = c("county", "state"))

# Many of the rows that didn't match are reservations, which aren't counties.
# Aside from those, it is mostly the Virginia city vs. county issue that is
# troublesome.
z[!z$temp_id %in% w$temp_id, c("placeName", "stateName")]

# Making an educated guess that the FEMA data adds "(County)" to the end of 
# Virginia county names, but not "(City)" when it is an unincorporated city
# (sometimes these have the same name so it is important to distinguish)
z[z$stateCode == "VA",
  "placeName"] <- ifelse(grepl("COUNTY", z[z$stateCode == "VA", "placeName"]),
                         z[z$stateCode == "VA", "placeName"],
                         paste(z[z$stateCode == "VA", "placeName"], "CITY"))

# It also looks like FEMA gives fancy names to counties that are designated
# for some public assistance program:
# ex: "New Haven (County)(in (P)MSA 1160,5480,8880)" 
# Verified that there are not duplicated  names without the numerical code.
# (fema.gov/disaster/747/designated-areas)

z$placeName <- gsub("COUNTYIN PMSA.*", "COUNTY", z$placeName)

w <- merge(z, fips_map, by.x = c("placeName", "stateName"),
           by.y = c("county", "state"))

# At this point almost 98% of events were matched to counties! I'm happy with
# that.
nrow(w) / nrow(z)

# How many counties are included here (have experienced a weather related
# disaster?)
nrow(w[!duplicated(w[, c("s_fips", "c_fips")]), ])





