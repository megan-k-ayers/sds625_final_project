# The purpose of this script is to standardize the county and state name and
# FIPS columns in the election data for future merging with other data sets,
# to check if all US counties identified by the Census are accounted for, and
# to run some basic QA before writing a cleaned file.

# Data is from the Harvard data-verse:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

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

# Do some quick QA

# Differences in `office` are not noted in the code book, I am assuming these
# can all be combined as the candidates are all known to have been running for
# the same office
table(x$office, x$candidate)

# Check that "Democrat" == "Joe Biden", "Republican" == "Trump" etc.
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

# Aggregate across voting modes for each state, county, candidate
# test <- aggregate(c("totalvotes", "candidatevotes"),
#                     by = list("state", "county_name", "county_fips",
#                            "candidate", "party"), data = x, FUN = sum)


##### 

# x <- x[, c("state", "county_name", "county_fips", "party",
#            "candidatevotes", "totalvotes")]
# 
# # Aggregate votes to party level (only relevant for "Other" party)
# x <- aggregate(candidatevotes ~ state + county_name + county_fips + party +
#                  totalvotes, data = x, sum)
# 
# # Uh oh, lots of counties are not represented...
# election_counties <- x[!duplicated(x[, c("state", "county_fips"), ]),
#                        c("state", "county_fips")]
# 
# table(election_counties$state)



