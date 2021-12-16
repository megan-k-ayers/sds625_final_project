# The purpose of this script is to merge the processed data sets together,
# clean resulting data frame and do preparations that will apply to all analysis
# approaches (ie making weather flags, calculation population proportions, etc.)

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

# Recall that I was unable to get election data for Alaska at the proper
# county level to match with the Census, plus one Hawaii county. Drop these.
sapply(x, function(a){sum(is.na(a))})
x <- x[!is.na(x$prop_dem), ]

# ----------------- CENSUS DATA VARIABLE ADJUSTMENTS -----------------

# Turn count variables into proportions (except for the total population count
# of each county)
x$male_pop <- x$male_pop / x$ttl_pop
x <- x[, !names(x) == "female_pop"]  # Can be deduced from male_pop

x$white_pop <- x$white_pop / x$ttl_pop
x$black_pop <- x$black_pop / x$ttl_pop
x$native_pop <- x$native_pop / x$ttl_pop
x$asian_pop <- x$asian_pop / x$ttl_pop
x$hisp_ttl_pop <- x$hisp_ttl_pop / x$ttl_pop
x$pacific_pop <- x$pacific_pop / x$ttl_pop
x$other_race_pop <- x$other_race_pop / x$ttl_pop
x$mult_race_pop <- x$mult_race_pop / x$ttl_pop
x$white_hisp_pop <- x$white_hisp_pop / x$ttl_pop
x$white_nonhisp_pop <- x$white_nonhisp_pop / x$ttl_pop
x$black_hisp_pop <- x$black_hisp_pop / x$ttl_pop

# --------------- WEATHER DATA VARIABLE CLEANING, FLAG CREATION ---------------

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


# ---------------------- CREATE DEMOCRAT/REPUBLICAN FLAG ----------------------
# Create red state/blue state indicator
x$poli_lean <- ifelse(x$prop_dem > x$prop_rep, "blue", "red")

# Make total votes a proportion of the county population
x$ttl_votes <- x$ttl_votes / x$ttl_pop


# ------------- CHECK RESPONSE IS RELATIVELY NORMALLY DISTRIBUTED -------------

# Save qqplot for report appendix
png(filename = "./writeups/images/response_qqplot.png",
    width = 800, height = 500, pointsize = 20)
qqnorm(x$happening, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
     main = "Normal Q-Q Plot of Response Variable `happening`")
dev.off()

# ------------------------------ SAVE DATA FRAME ------------------------------
# Double check we have no more NA values
sum(sapply(x, function(a){sum(is.na(a))}) > 0) == 0

write.csv(x, "./processed_data/merged_data.csv", row.names = F)

