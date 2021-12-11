# The purpose of this script is to merge the processed data sets together and
# identify the covariates among those I collected that seem predictive of
# climate change belief.

rm(list = ls())

# ---- Read in processed data and merge them together, one row per county ----
x <- read.csv("./processed_data/ycom_cleaned.csv", header = T)
x <- merge(read.csv("./processed_data/election2020_cleaned.csv", header = T),
           x[, !names(x) %in% c("county", "state")], by = c("c_fips", "s_fips"),
           all.y = T)
x <- merge(read.csv("./processed_data/fema_weather_cleaned.csv", header = T),
           x[, !names(x) %in% c("county", "state")], by = c("c_fips", "s_fips"),
           all.y = T)
x <- merge(read.csv("./processed_data/census_data_cleaned.csv", header = T),
           x[, !names(x) %in% c("county", "state")], by = c("c_fips", "s_fips"),
           all.y = T)

# ----------------- CENSUS DATA VARIABLE SELECTION, CLEANING -----------------

# Turn count variables into proportions (except for the total population count
# of each county)
x$male_pop <- x$male_pop / x$ttl_pop
x <- x[, !names(x) == "female_pop"]  # Can be deduced from male_pop

# Dropping gender specific median ages - I think median age alone will be
# enough. Interesting that gender has a tiny influence but not seeing anything
# that convincing when I look at proportion of male population alone.
plot(x$med_age, x$happening)
abline(lm(happening ~ male_med_age, data = x), col = "blue")
abline(lm(happening ~ female_med_age, data = x), col = "red")

plot(x$male_pop, x$happening)

x <- x[, !names(x) %in% c("male_med_age", "female_med_age", "male_pop")]

# Calculate county proportions by race rather than raw counts
x$white_pop <- x$white_pop / x$ttl_pop
x$black_pop <- x$black_pop / x$ttl_pop
x$native_pop <- x$native_pop / x$ttl_pop
x$asian_pop <- x$asian_pop / x$ttl_pop
x$hisp_ttl_pop <- x$hisp_ttl_pop / x$ttl_pop
x$pacific_pop <- x$pacific_pop / x$ttl_pop
x$other_race_pop <- x$other_race_pop / x$ttl_pop
x$mult_race_pop <- x$mult_race_pop / x$ttl_pop

# I don't want to include too many of these...the most predictive ones look
# like sqrt of non-white population, Asian population and Hispanic population
# (Using sqrt instead of log because I won't want log(0) happening for
# any modeling later)
plot(x$white_pop, x$happening)
plot(sqrt(1 - x$white_pop), x$happening) # sqrt of proportion of not-White pop
plot(sqrt(x$black_pop), x$happening)
plot(sqrt(x$native), x$happening)
plot(sqrt(x$asian_pop), x$happening)
plot(sqrt(x$hisp_ttl_pop), x$happening)
plot(sqrt(x$pacific_pop), x$happening)
plot(sqrt(x$other_race_pop), x$happening)
plot(sqrt(x$mult_race_pop), x$happening)

x$non_white_pop <- 1 - x$white_pop

drop_cols <- c("black_pop", "native_pop",
               "pacific_pop", "other_race_pop", "mult_race_pop",
               "white_nonhisp_pop", "black_hisp_pop",
               "white_hisp_pop", "white_pop")

x <- x[, !names(x) %in% drop_cols]

# I expect this to be predictive and also tied to living nearer to the coasts
plot(log(x$med_income), x$happening)

# Don't want total population counts for matching, just proportions
x <- x[, !names(x) %in% c("ttl_pop", "ttl_votes")]



# ----------------- WEATHER DATA VARIABLE SELECTION, CLEANING -----------------

# Replace weather flag NA values resulting from merge with FALSE
weath_cols <- grep("any_weather|hurricane", names(x))
x[, weath_cols] <- as.data.frame(sapply(x[, weath_cols], function(a){
  ifelse(is.na(a), FALSE, a) }))

# Interested in counties where extreme weather occured as closely as possible
# to the time of the survey, but also where it wouldn't be a fluke (more than
# two other extreme events happened in the last 5 years)
x$weather_flag <- x$any_weather_2019 & (x$any_weather_2018 +
                                          x$any_weather_2017 +
                                          x$any_weather_2016 +
                                          x$any_weather_2015 >= 2)

sum(x$weather_flag) / nrow(x)

# Same logic but specifically wit hurricanes - might not end up using this but
# could be interesting
x$hurricane_flag <- x$hurricane_2019 & (x$hurricane_2018 +
                                        x$hurricane_2017 +
                                        x$hurricane_2016 +
                                        x$hurricane_2015 >= 2)
sum(x$hurricane_flag) / nrow(x)

# Drop other weather variables
x <- x[, -weath_cols]


# ---------------- ELECTION DATA VARIABLE SELECTION, CLEANING ----------------

# Dropping proportion of votes to "other" party - I don't really expect to see
# much effect from proportions other than major 2-parties (though maybe Green?
# but their share is so tiny in all counties). Even if "other" appeared to
# have an effect, it wouldn't be interpretable. 
x <- x[, names(x) != "prop_other"]
plot(x$prop_dem, x$happening) # This makes sense based on what we know about
# the world...
plot(x$prop_rep, x$happening)

# Looks to maybe be a positive relationship as I'd expect, but the proportions
# are tiny. Nowhere near as predictive as democrat v. republican.
plot(log(x$prop_green), x$happening)
plot(log(x$prop_lib), x$happening) # Not convincing

x <- x[, !names(x) %in% c("prop_green", "prop_lib")]

# Create red state/blue state indicator - just out of curiosity want to see
# how much political leaning alone can predict climate change belief
x$poli_lean <- ifelse(x$prop_dem > x$prop_rep, "blue", "red")
hist(x$prop_dem)

# Fairly normal, don't think this will require any transforms
hist(x$happening, breaks = 50)
table(x$poli_lean) 

# Start with super basic ANOVA test
tapply(x$happening, x$poli_lean, mean)  # Checks out! 
basic_lm <- lm(happening ~ poli_lean, data = x)
summary(basic_lm)

# -------------------------------- SAVE FILE --------------------------------

# Before saving, checking for NA values...
# Recall that I was unable to get election data for Alaska at the proper
# county level to match with the Census, plus one Hawaii county. Drop these.
sapply(x, function(a){sum(is.na(a))})
x <- x[!is.na(x$prop_dem), ]

write.csv(x, "./processed_data/merged_data.csv", row.names = F)

