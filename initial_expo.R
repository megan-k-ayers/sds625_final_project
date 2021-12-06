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



# ---------------- EXTREME WEATHER VISUALIZATIONS -------------------------

rm(list = ls())
w <- read.csv("./processed_data/fema_weather_cleaned.csv", header = T)
y <- read.csv("./processed_data/ycom_cleaned.csv", header = T)











