# The purpose of this script is to do some EDA, feel out which plots I may
# want to include in the final draft, do some modeling tests

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(maps)
library(usmap)

# ---------------- 2020 ELECTION RESULTS vs YCOM DATA -------------------------

# First want to validate assumption that political leaning is a strong
# predictor of likelihood of believing that climate change is happening -
# merge survey data with election data alone

e <- read.csv("./processed_data/election2020_cleaned.csv", header = T)
y <- read.csv("./processed_data/ycom_cleaned.csv", header = T)

y <- merge(y, e[, !names(e) %in% c("state", "county")],
           by = c("c_fips", "s_fips"))

# Create red state/blue state indicator
y$poli_lean <- ifelse(y$prop_dem > y$prop_rep, "blue", "red")

# Start with super basic ANOVA test
tapply(y$happening, y$poli_lean, mean)
basic_lm <- lm(happening ~ poli_lean, data = y)
summary(basic_lm)



# ---------------- EXTREME WEATHER VISUALIZATIONS -------------------------

rm(list = ls())
w <- read.csv("./processed_data/noaa_weather19_cleaned.csv", header = T)
y <- read.csv("./processed_data/ycom_cleaned.csv", header = T)

# Very concerning that there are only 9 records of hurricanes.... I should
# have caught this earlier but there are only 10 records in the original data.
# TODO: Need to either find a better data source about hurricanes or verify
# that they effected a small number of counties in 2019.
sort(table(w$event_type))

# Filter to only events that caused injury or death (direct or indirect)
w <- w[w$deaths_indirect > 0 | w$deaths_direct > 0 |
         w$injuries_indirect > 0 | w$injuries_direct > 0, ]

# Now there are no hurricanes in here... 
sort(table(w$event_type))

# Get count of weather episodes for each county (for now don't worry about what
# kind, just curious to see the overall distribution)
# (Trying episodes instead of events bc this seems more appropriate to
# aggregate - not sure how consistently individual events within episodes are
# recorded)

test <- aggregate(episode_id ~ state + s_fips + county + c_fips,
                  data = w, function(a){length(unique(a))})
test$log_episode_id <- log(test$episode_id)

# hist(test$episode_id)
# hist(test$log_episode_id)  # More Normal

# plot_usmap requires 5 digit FIPS code column for plotting counties
test$fips <- paste(sprintf("%02s", test$s_fips),
                   sprintf("%03s", test$c_fips), sep = "")

# This needs a lot of improving on visually but just a start
plot_usmap(data = test,
           values = "episode_id",
           color = NA) + 
  scale_fill_gradient(low = "blue",
                      high = "red") +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.size = grid::unit(1.5, "lines"),
        legend.title=element_text(size=8),
        plot.title = element_text(face="bold")) +
  ggtitle("test")


# I am not confident in the weather data at this point, so I will probably
# look for other sources.... but just want to check what I have so far.
# Is there an indication in a simple ANOVA test if experiencing a weather event
# is linked to increased belief in climate change (acknowledging that this
# may be confounded with other variables)
test$weather_flag <- TRUE
test <- test[, c("s_fips", "c_fips", "weather_flag")]

test <- merge(y, test, by = c("c_fips", "s_fips"), all.x = T)
test$weather_flag <- ifelse(is.na(test$weather_flag), F, T)

table(test$weather_flag)

tapply(test$happening, test$weather_flag, mean)
basic_lm <- lm(happening ~ weather_flag, data = test)
summary(basic_lm)

# Based on quality issues in NOAA data that I am now discovering, I might try
# to do a last minute pivot to using Census data -
# https://www.census.gov/topics/preparedness/events.html

# Downside is that it is not immediately clear if I will be able to get all of
# this data from anything more aggregated than sheets for individual states
# affected by each event... Which would be a huge hassle. I don't think I have
# the time or mental fortitude to scrape it (plus I'm not sure if the Census
# allows that...)






