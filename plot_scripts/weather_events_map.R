# The purpose of this script is to generate a simple reference map showing
# counties affected by weather events that were FEMA declared disasters in
# the past two years

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(maps)
library(usmap)
library(gridExtra)


# Get cleaned weather data in long format
x <- read.csv("./processed_data/fema_weather_cleaned_LONG.csv", header = T)

# Merged data has flags for counties included in the analysis - use this to
# filter to weather for these counties in a separate dataset
y <- read.csv("./processed_data/merged_data.csv", header = T)
y <- y[, c("c_fips", "s_fips", "weather_flag")]
y <- merge(y, x, by = c("c_fips", "s_fips"))
y <- y[y$weather_flag, ]

# The function plot_usmap requires 5 digit FIPS code column for county plots
y$fips <- paste(sprintf("%02s", y$s_fips), sprintf("%03s", y$c_fips), sep = "")
x$fips <- paste(sprintf("%02s", x$s_fips), sprintf("%03s", x$c_fips), sep = "")

# Because I am really only visualizing the different types of weather as 
# evidence that this data is what we expect, I'm going to combine some
# categories for showing only counties that we flag for the analysis
y$weather_type <- ifelse(y$weather_type %in% c("Severe Ice Storm",
                                               "Coastal Storm"),
                         "Severe Storm(s)", y$weather_type)

# Given weather data with potentially multiple rows per county, aggregate to
# return which type of weather that county experienced each time or "Multiple"
get_county_weather <- function(data) {
  
  temp <- character()
  for (f in unique(data$fips)) {
    
    w <- data[data$fips == f, "weather_type"]
    if (length(unique(w)) > 1) {
      temp <- c(temp, "Multiple")
    } else { temp <- c(temp, unique(w))}
    
  }
  
  df <- data.frame(fips = unique(data$fips), weather_type_f = temp)
  return(df)
}


#' Function to plot counties by the FEMA-declared weather disasters that they
#' experienced
#' @param data Data frame with values to plot
#' @param title Text wanted for the title of the plot
#' @return ggplot object (map of the US counties)
get_fema_map <- function(data, title, colors) {
  
  # Generate a simple map which contains the state outlines
  states <- plot_usmap("states",
                       color = "black",
                       fill = alpha(0.01))
  
  # Create map of counties overlaid with state lines for reference
  p <- plot_usmap(data = data,
                  values = "weather_type_f",
                  # Adding below line gives warning - plot seems fine though...
                  lwd = 0.1,  # Thinner county line widths
                  color = "black") +  # Make county lines black
    scale_fill_manual(name = "Weather event",
                      # Custom colors for each weather event
                      values = colors,
                      na.translate = F) +
    theme(legend.position = "right",
          legend.justification = "center",
          legend.box = "vertical",
          legend.key.size = grid::unit(1.5, "lines"),
          legend.title=element_text(size=8),
          plot.title = element_text(face="bold", hjust = 0.5, vjust = -3,
                                    size = 8)) +
    geom_polygon(data = states[[1]],  # Adding the state lines on top
                 aes(x = x, y = y, group = group),
                 color = "black",
                 fill = alpha(0.01),
                 lwd = 0.25) +  # Slightly thicker lines
    ggtitle(title)
  
  return(p)
  
}

# Want individual plots for each year, then aggregated plot showing the counties
# that make it into the analysis flagged
y_agg <- get_county_weather(y)
x_2019 <- get_county_weather(x[x$year == "2019", ])
x_2018 <- get_county_weather(x[x$year == "2018", ])
x_2017 <- get_county_weather(x[x$year == "2017", ])
x_2016 <- get_county_weather(x[x$year == "2016", ])
x_2015 <- get_county_weather(x[x$year == "2015", ])

(types <- unique(rbind(y_agg, x_2018, x_2018, x_2017, x_2016,
                      x_2015)$weather_type_f))
color_map <- list("Hurricane" = "cornflowerblue",
                  "Severe Storm(s)" = "darkslateblue",
                  "Coastal Storm" = "goldenrod1",
                  "Severe Ice Storm" = "mediumorchid3",
                  "Snow" = "gray50",
                  "Flood" = "turquoise3",
                  "Fire" = "firebrick2",
                  "Tornado" = "sienna2",
                  "Multiple" = "forestgreen")

# Enforce ordering for legend appearance - manual process, don't think it's
# worth the time to try to automate right now
y_agg$weather_type_f <- factor(y_agg$weather_type_f,
                               levels = c("Hurricane", "Severe Storm(s)",
                                          "Flood", "Fire", "Multiple"))
x_2019$weather_type_f <- factor(x_2019$weather_type_f,
                                levels = c("Hurricane", "Severe Storm(s)",
                                           "Coastal Storm",
                                           "Flood", "Fire", "Tornado",
                                           "Multiple"))
x_2018$weather_type_f <- factor(x_2018$weather_type_f,
                                levels = c("Hurricane", "Severe Storm(s)",
                                           "Coastal Storm", "Snow",
                                           "Flood", "Fire", "Tornado",
                                           "Multiple"))
x_2017$weather_type_f <- factor(x_2017$weather_type_f,
                                levels = c("Hurricane", "Severe Storm(s)",
                                           "Snow", "Severe Ice Storm",
                                           "Flood", "Fire", "Tornado",
                                           "Multiple"))
x_2016$weather_type_f <- factor(x_2016$weather_type_f,
                                levels = c("Hurricane", "Severe Storm(s)",
                                           "Snow",
                                           "Flood", "Fire", "Tornado",
                                           "Multiple"))
x_2015$weather_type_f <- factor(x_2015$weather_type_f,
                                levels = c("Severe Storm(s)",
                                           "Snow", "Severe Ice Storm", 
                                           "Coastal Storm",
                                           "Flood", "Fire", "Tornado",
                                           "Multiple"))


# Generate all the plots
p_agg <- get_fema_map(y_agg,
                      title = "Counties with 3 FEMA declared weather disasters 2015-2019,
including one in 2019",
                      colors = as.character(color_map[names(color_map) %in%
                                                        levels(y_agg$weather_type_f)]))
p_2019 <- get_fema_map(x_2019,
                      title = "Counties with FEMA declared weather disasters in 2019",
                      colors = as.character(color_map[names(color_map) %in%
                                                        levels(x_2019$weather_type_f)]))
p_2018 <- get_fema_map(x_2018,
                       title = "Counties with FEMA declared weather disasters in 2018",
                       colors = as.character(color_map[names(color_map) %in%
                                                         levels(x_2018$weather_type_f)]))
p_2017 <- get_fema_map(x_2017,
                       title = "Counties with FEMA declared weather disasters in 2017",
                       colors = as.character(color_map[names(color_map) %in%
                                                         levels(x_2017$weather_type_f)]))
p_2016 <- get_fema_map(x_2016,
                       title = "Counties with FEMA declared weather disasters in 2016",
                       colors = as.character(color_map[names(color_map) %in%
                                                         levels(x_2016$weather_type_f)]))
p_2015 <- get_fema_map(x_2015,
                       title = "Counties with FEMA declared weather disasters in 2015",
                       colors = as.character(color_map[names(color_map) %in%
                                                         levels(x_2015$weather_type_f)]))

# Need a plot that has every weather event type in the legend so that the
# ggarrange combination plot makes sense with all colors - create an unused
# plot just with the correct legend
x_legend <- x[!duplicated(x[, "weather_type"]), c("fips", "weather_type")]
x_legend <- rbind(x_legend, data.frame(fips = 10001, weather_type = "Multiple"))
x_legend$weather_type_f <- factor(x_legend$weather_type,
                                levels = c("Hurricane", "Severe Storm(s)",
                                           "Coastal Storm", "Severe Ice Storm",
                                           "Snow", "Flood", "Fire", "Tornado",
                                           "Multiple"))
p_legend <- get_fema_map(x_legend,
                         title = "Just want the legend with all events",
                         colors = as.character(color_map[names(color_map) %in%
                                                           levels(x_legend$weather_type_f)]))

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Make combined plot
p_all <- grid.arrange(p_agg + theme(legend.position = "none"),
                      p_2019 + theme(legend.position = "none"),
                      p_2018 + theme(legend.position = "none"),
                      p_2017 + theme(legend.position = "none"),
                      p_2016 + theme(legend.position = "none"),
                      p_2015 + theme(legend.position = "none"),
                      g_legend(p_legend), widths = c(2, 2, 1),
                      layout_matrix = rbind(c(1, 2, NA),
                                            c(3, 4, 7),
                                            c(5, 6, NA)))


# Save plots for potential use in report
ggsave("./writeups/images/fema_events_2015_map.png", plot = p_2015,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./writeups/images/fema_events_2016_map.png", plot = p_2016,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./writeups/images/fema_events_2017_map.png", plot = p_2017,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./writeups/images/fema_events_2018_map.png", plot = p_2018,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./writeups/images/fema_events_2019_map.png", plot = p_2019,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./writeups/images/fema_events_agg_map.png", plot = p_agg,
       device = "png", width = 8, height = 5, units = "in", bg = "white")
ggsave("./writeups/images/fema_events_maps_combined.png", plot = p_all,
       device = "png", width = 10, height = 12, units = "in", bg = "white")


