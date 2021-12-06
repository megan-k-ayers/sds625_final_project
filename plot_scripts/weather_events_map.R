# The purpose of this script is to generate a simple reference map showing the
# county-level results of the 2020 presidential election (the proxy for
# political leaning in this analysis)

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(maps)
library(usmap)

# Get cleaned election data

y <- read.csv("./processed_data/fema_weather_cleaned.csv", header = T)

# plot_usmap requires 5 digit FIPS code column for plotting counties
y$fips <- paste(sprintf("%02s", y$s_fips), sprintf("%03s", y$c_fips), sep = "")


# Generate a simple map which contains the state outlines
states <- plot_usmap("states",
                     color = "black",
                     fill = alpha(0.01))

# # Create map of counties overlaid with state lines for reference
# p <- plot_usmap(data = y,
#                 values = "happening",  # Color by flag.f
#                 # Adding below line gives warning - plot seems fine though...
#                 lwd = 0.2,  # Thinner county line widths
#                 color = "black") +  # Make county lines black
#   scale_fill_continuous(type = "viridis",
#                         name = "Estimated proportion \nof county believing that \nclimate change is happening") +
#   theme(legend.position = "right",
#         legend.justification = "center",
#         legend.box = "vertical",
#         legend.key.size = grid::unit(1.5, "lines"),
#         legend.title=element_text(size=8),
#         plot.title = element_text(face="bold", hjust = 0.5, vjust = -3,
#                                   size = 12)) +
#   geom_polygon(data = states[[1]],  # Adding the state lines on top
#                aes(x = x, y = y, group = group),
#                color = "black",
#                fill = alpha(0.01),
#                lwd = 0.4) +  # Slightly thicker lines
#   ggtitle("Climate change belief estimates (county level)")
# 
# p
# 
# # Save plot for potential use in report
# # ggsave("./plots/climate_change_opinions_map.png", plot = p,
# #        device = "png", width = 8, height = 5, units = "in", bg = "white")
# 
# 
# 
# 
