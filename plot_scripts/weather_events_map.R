# The purpose of this script is to generate a simple reference map showing
# counties affected by weather events that were FEMA declared disasters in
# the past two years

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(maps)
library(usmap)

# Get cleaned election data
y <- read.csv("./processed_data/fema_weather_cleaned_LONG.csv", header = T)

# The function plot_usmap requires 5 digit FIPS code column for county plots
y$fips <- paste(sprintf("%02s", y$s_fips), sprintf("%03s", y$c_fips), sep = "")


# Handle the fact that in each year (or in aggregate) counties could experience
# multiple types of weather disasters
# I would do this less manually if there were more years to analyze, but for
# only two this is fine
y_2018 <- y[y$year == "2018", ]
temp <- sapply(unique(y_2018$fips), function(f){
  w <- y_2018[y_2018$fips == f, "weather_type"]
  if (length(unique(w)) > 1) { return("Multiple") } else { return(unique(w)) }
})
y_2018 <- data.frame(fips = unique(y_2018$fips), weather_type_f = temp)
# Turn weather_type into a factor for more control plotting with colors
y_2018$weather_type_f <- factor(y_2018$weather_type_f,
                                levels = c("Hurricane", "Severe Storm(s)",
                                           "Flood", "Coastal Storm", "Tornado",
                                           "Fire", "Multiple"))

y_2019 <- y[y$year == "2019", ]
temp <- sapply(unique(y_2019$fips), function(f){
  w <- y_2019[y_2019$fips == f, "weather_type"]
  if (length(unique(w)) > 1) { return("Multiple") } else { return(unique(w)) }
})
y_2019 <- data.frame(fips = unique(y_2019$fips), weather_type_f = temp)
y_2019$weather_type_f <- factor(y_2019$weather_type_f,
                                levels = c("Hurricane", "Severe Storm(s)",
                                           "Flood", "Coastal Storm", "Tornado",
                                           "Fire", "Multiple"))

temp <- sapply(unique(y$fips), function(f){
  w <- y[y$fips == f, "weather_type"]
  if (length(unique(w)) > 1) { return("Multiple") } else { return(unique(w)) }
})
y_agg <- data.frame(fips = unique(y$fips), weather_type_f = temp)
y_agg$weather_type_f <- factor(y_agg$weather_type_f,
                               levels = c("Hurricane", "Severe Storm(s)",
                                           "Flood", "Coastal Storm", "Tornado",
                                           "Fire", "Multiple"))

# Generate a simple map which contains the state outlines
states <- plot_usmap("states",
                     color = "black",
                     fill = alpha(0.01))


#' Function to plot counties by the FEMA-declared weather disasters that they
#' experienced
#' @param data Data frame with values to plot
#' @param title Text wanted for the title of the plot
#' @return ggplot object (map of the US counties)
get_fema_map <- function(data, title) {
  
  # Create map of counties overlaid with state lines for reference
  p <- plot_usmap(data = data,
                  values = "weather_type_f",
                  # Adding below line gives warning - plot seems fine though...
                  lwd = 0.2,  # Thinner county line widths
                  color = "black") +  # Make county lines black
    scale_fill_manual(name = "Weather event",
                      # Custom colors for each weather event
                      values = c("cornflowerblue", "darkslateblue",
                                 "lightsteelblue3", "turquoise3", "goldenrod1",
                                 "sienna2", "darkseagreen2"),
                      na.translate = F) +
    theme(legend.position = "right",
          legend.justification = "center",
          legend.box = "vertical",
          legend.key.size = grid::unit(1.5, "lines"),
          legend.title=element_text(size=8),
          plot.title = element_text(face="bold", hjust = 0.5, vjust = -3,
                                    size = 12)) +
    geom_polygon(data = states[[1]],  # Adding the state lines on top
                 aes(x = x, y = y, group = group),
                 color = "black",
                 fill = alpha(0.01),
                 lwd = 0.4) +  # Slightly thicker lines
    ggtitle(title)
  
  return(p)
  
}

p_2018 <- get_fema_map(y_2018,
                       "Counties with FEMA declared weather disasters in 2018")
p_2019 <- get_fema_map(y_2019,
                   "Counties with FEMA declared weather disasters in 2019")
# Get a map aggregated across 2018 and 2019
p_agg <- get_fema_map(y_agg,
                      "Counties with FEMA declared weather disasters in 2018 or 2019")

# p_2018; p_2019; p_agg


# Save plots for potential use in report
ggsave("./plots/fema_events_2018_map.png", plot = p_2018,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./plots/fema_events_2019_map.png", plot = p_2019,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./plots/fema_events_agg_map.png", plot = p_agg,
       device = "png", width = 8, height = 5, units = "in", bg = "white")


