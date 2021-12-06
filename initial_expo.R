# The purpose of this script is to do some EDA, feel out which plots I may
# want to include in the final draft, do some modeling tests

rm(list = ls())

# Packages for plotting only :)
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



# ---------------- EXTREME WEATHER EXPLORATION -------------------------

rm(list = ls())
w <- read.csv("./processed_data/fema_weather_cleaned.csv", header = T)
y <- read.csv("./processed_data/ycom_cleaned.csv", header = T)

y <- merge(y, w, by = c("c_fips", "county", "state", "s_fips"), all.x = T)

# Columns flagging weather event occurrences end with _YYYY
weather_cols <- grep("_2018|_2019", names(y))

# Replace weather flag NA values resulting from merge with FALSE
y[, weather_cols] <- as.data.frame(sapply(y[, weather_cols], function(a){
  ifelse(is.na(a), FALSE, a) }))

y$any_weather_18 <- sapply(1:nrow(y), function(a){
  any(y[a, grep("_2018", names(y))]) })
y$any_weather_19 <- sapply(1:nrow(y), function(a){
  any(y[a, grep("_2019", names(y))]) })
y$any_weather <- y$any_weather_18 | y$any_weather_19

# How many counties with climate change belief estimates experienced FEMA
# declared weather events in 2018/2019?
table(y$any_weather_18)/nrow(y)
table(y$any_weather_19)/nrow(y)

table(y$any_weather_18, y$any_weather_19)/nrow(y)

# Start with super basic ANOVA test - average climate belief estimates are
# practically the same between those who did/did not experience a FEMA
# declared weather event. But this is not that surprising to me since this is
# way oversimplifying things - this is why I wanted to bring in other data.
tapply(y$happening, y$any_weather, mean)
basic_lm <- lm(happening ~ any_weather, data = y)
summary(basic_lm)

