# The purpose of this script is to standardize the county and state name and
# FIPS columns in the election data for future merging with other data sets,
# to check if all US counties identified by the Census are accounted for, and
# to run some basic QA before writing a cleaned file.

# Data is from the Harvard data-verse:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

# TODO: Add variable indicating long-run political affiliation (since 2000) if
# exists?

rm(list = ls())

# --------------------------- ELECTIONS ------------------------------------
# Read in the elections 2000-2020 dataset and the processed Census FIPS mapping
# file for reference
x <- read.csv("./src_data/dataverse_files/countypres_2000-2020.csv")
fips_map <- read.csv("./processed_data/census_fips_cleaned.csv")

# Filter to the 2020 elections (start with these for an idea of the political
# leanings in each county at the time of the climate opinion survey - could
# create another variable to indicate long term political leaning maybe)
x <- x[x$year == 2020,
       c("year", "state", "county_name", "county_fips", "office", "candidate",
         "party", "candidatevotes", "totalvotes", "mode")]


# Are there any NA values?
sapply(x, function(a){sum(is.na(a))})

# Handling NA county_fips
x[is.na(x$county_fips), ]
# The Census codes DC with FIPS 11001, follow that convention
x$county_fips <- ifelse(x$state == "DISTRICT OF COLUMBIA", 11001, x$county_fips)
# Not sure what is going on with this "FEDERAL PRECINCT" in RI. Not finding
# much by googling it, but this is a small number of votes not tied to a county
# so I'll toss it.
x <- x[!(x$state == "RHODE ISLAND" & x$county_name == "FEDERAL PRECINCT"), ]

# Not sure why San Joaquin County CA is having issues with NA votes. I verified
# these existing candidatevotes values on their county website, so I'll
# manually fix this case
# (https://www.sjgov.org/department/rov/results/election-results)
x[is.na(x$candidatevotes), ]
x[is.na(x$totalvotes), ]
# x[x$state == "CALIFORNIA" & x$county_fips == 6077, ]

x[x$state == "CALIFORNIA" & x$county_fips == 6077, "totalvotes"] <- 289781
x[x$state == "CALIFORNIA" & x$county_fips == 6077 & x$party == "OTHER",
  "candidatevotes"] <- 964 + 1251 + 34 + 7 + 8 + 1289
sum(x[x$state == "CALIFORNIA" & x$county_fips == 6077, "candidatevotes"])

# Check NA values again - should be all gone
sapply(x, function(a){sum(is.na(a))})


# Differences in `office` are not noted in the code book, I am assuming these
# can all be combined as the candidates are all known to have been running for
# the same office
table(x$office, x$candidate)

# Check that each party only has one candidate listed in the data set
table(x$candidate, x$party)

# The modes of voting recorded differ by state. Just make sure that states
# that have mode == "TOTAL" do not have any other modes (don't want to
# count votes more than once)
# table(x$mode, x$state)
ttl_states <- unique(x[x$mode == "TOTAL", "state"])
ttl_states <- aggregate(mode ~ state, data = x[x$state %in% ttl_states, ],
                        function(x){length(unique(x))})
ttl_states[ttl_states$mode > 1, ] # Utah is in over-counting danger zone...
# But it's okay because there are no votes actually recorded for those rows
sum(ttl_states[x$state == "UTAH" & x$mode != "TOTAL", "totalvotes"])
sum(ttl_states[x$state == "UTAH" & x$mode != "TOTAL", "candidatevotes"])

# Check that for each state-county pair there is only one unique value of
# totalvotes
sum(aggregate(totalvotes ~ state + county_fips, data = x, 
          function(a){length(unique(a))})$totalvotes > 1) == 0



# Aggregate candidatevotes across voting modes for each state, county,
# candidate
x_c <- aggregate(candidatevotes ~ state + county_name + county_fips + party,
                 data = x, FUN = sum)
x_t <- x[, c("state", "county_name", "county_fips", "totalvotes")]
x_t <- x_t[!duplicated(x_t), ]
x <- merge(x_t, x_c, by = c("state", "county_name", "county_fips"))

# Check that the sum of candidate votes is the same as the total number of votes
x_c <- aggregate(candidatevotes ~ state + county_name + county_fips,
                 data = x_c, FUN = sum)
names(x_c) <- c("state", "county_name", "county_fips", "recalc_totalvotes")
x <- merge(x, x_c, by = c("state", "county_name", "county_fips"))

all(x$totalvotes == x$recalc_totalvotes) # Great!

x <- x[, c("state", "county_name", "county_fips", "party",
           "candidatevotes", "totalvotes")]
names(x) <- c("state", "county", "fips", "party", "cand_votes", "ttl_votes")


# Would like to pivot the votes information so that each party's share of the
# vote is a column:
x <- reshape(x, idvar = c("state", "county", "fips", "ttl_votes"),
                timevar = "party", direction = "wide")
attr(x, "reshapeWide") <- NULL  # Distracting to see these in environment

x[, grepl("^cand_votes", names(x))] <- x[, grepl("^cand_votes", names(x))] /
  x$ttl_votes
names(x) <- tolower(gsub("cand_votes\\.", "prop_", names(x)))
x$prop_other <- ifelse(is.na(x$prop_other), 0, x$prop_other)
x$prop_green <- ifelse(is.na(x$prop_green), 0, x$prop_green)
x$prop_libertarian <- ifelse(is.na(x$prop_lib), 0, x$prop_lib)

# Double check that all proportions add to 1 (within machine precision errors)
all(round(x$prop_rep + x$prop_dem + x$prop_green + x$prop_other +
      x$prop_lib, 14) == 1)



# Break out county/state FIPS individually, check with Census mapping
x$fips <- sprintf("%05d", x$fips)
x$s_fips <- as.integer(gsub("[0-9]{3}$", "", x$fips))
x$c_fips <- as.integer(gsub("^[0-9]{2}", "", x$fips))

nrow(duplicated(x[, c("c_fips", "s_fips")])) # No duplicates here, good

# These almost entirely belong to Alaska, which has a different county system
# to Louisiana from my understanding. An initial Google did not help me see how
# to immediately rectify this issue, and it seems that there are fewer counties
# listed in the Census FIPS mapping than there are "districts", so rolling up
# would be a problem... For now I will continue the analysis without these
# "counties." The other bad_fips rows are for Kansas City, which is not a county
# (is in Jackson County MO), and Oglala Lakota County, whose FIPS seems to be
# mistaken in the elections results data (this county changed name and FIPS in
# 2014 according to )
bad_fips <- x[!x$fips %in% merge(x, fips_map,by = c("s_fips", "c_fips"))$fips, ]

# View(x[x$state == "ALASKA", ])
# View(fips_map[fips_map$state == "ALASKA", ])

x[x$county == "OGLALA LAKOTA", "c_fips"] <- 102
x[x$county == "OGLALA LAKOTA", "fips"] <- 46102

# Okay, so every county in the 2020 elections data aside from these Alaska
# districts and Kansas City (which is not a county) matches to something valid
# in the Census data. Keep only those that match.
bad_fips <- x[!x$fips %in% merge(x, fips_map,by = c("s_fips", "c_fips"))$fips, ]
x <- merge(x, fips_map[, c("s_fips", "c_fips")], by = c("s_fips", "c_fips"))

# Gut check that the most democratic/republican/etc leaning counties make sense
head(x[order(-x$prop_democrat), ])
head(x[order(-x$prop_republican), ])

# Clean up column ordering and names and save to CSV
x <- x[, c("state", "s_fips", "county", "c_fips", "prop_democrat",
           "prop_republican", "prop_libertarian", "prop_green", "prop_other",
           "ttl_votes")]
names(x) <- c("state", "s_fips", "county", "c_fips", "prop_dem", "prop_rep",
             "prop_lib", "prop_green", "prop_other", "ttl_votes")

write.csv(x, "./processed_data/election2020_cleaned.csv", row.names = FALSE)


