# The purpose of this script is to generate a pair of plots that show both
# the distribution of political leanings across US counties based on the 2020
# presidential election and the distribution of above/below average estimated
# belief in climate change

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(maps)
library(usmap)

# First merge the survey data with the election data

e <- read.csv("./processed_data/election2020_cleaned.csv", header = T)
y <- read.csv("./processed_data/ycom_cleaned.csv", header = T)
y <- merge(y, e[, !names(e) %in% c("state", "county")],
           by = c("c_fips", "s_fips"))

# Separate counties by color into the following discrete groups:
# 1. Counties that voted firm(ish) majority Republican (say > 52.5%) - fill red
# 2. Counties that voted firm(ish) majority Democrat (say > 52.5%) - fill blue
# 3. Counties that were close between Democrat and Republican (neither
#    had more than 52.5) - fill purple
# (Note: these thresholds for blue/red/purple seem intuitive to me but are
# ultimately arbitrary. I think it's fine for getting an exploratory plot
# but would need to be more careful in an inferential analysis, staying in line
# with any established Poli-Sci definitions of a swing/purple county.)
# 
# Adjust the shade of each of the 3 colors to represent climate change opinion
# estimate
# 1. Counties that believe in climate change MORE than average - dark
# 2. Counties that believe in climate change LESS than average - light
# 
# Initially - compare to the grand (national) average climate change belief. 
# Repeat this plot with comparisons to in-group climate change belief.

# plot_usmap requires 5 digit FIPS code column for plotting counties
y$fips <- paste(sprintf("%02s", y$s_fips), sprintf("%03s", y$c_fips), sep = "")

mu <- mean(y$happening)
# Create factor for 6 color-shade pairs
y$flag.f <- ifelse(y$prop_rep > 0.525 & y$happening < mu,
                   "rep_low",
                   ifelse(y$prop_rep > 0.525 & y$happening >= mu,
                          "rep_high",
                          ifelse(y$prop_dem > 0.525 & y$happening < mu,
                                 "dem_low",
                                 ifelse(y$prop_dem > 0.525 & y$happening >= mu,
                                        "dem_high",
                                        ifelse(y$happening < mu, "mod_low",
                                               "mod_high")))))
y$flag.f <- factor(y$flag.f, levels = c("rep_low", "rep_high", "dem_low",
                                        "dem_high", "mod_low", "mod_high"))
table(y$flag.f)

# Define specific colors to be used
colors <- c("indianred2",  # Republican, less belief
            "indianred4",  # Republican, more belief
            "lightblue2",  # Democrat, less belief
            "steelblue",  # Democrat, more belief
            "mediumpurple2",  # Purple, less belief
            "darkorchid4")  # Purple, more belief

# Generate a simple map which contains the state outlines
states <- plot_usmap("states",
                     color = "black",
                     fill = alpha(0.01))

# Create map of counties overlaid with state lines for reference
p <- plot_usmap(data = y,
                values = "flag.f",  # Color by flag.f
                # Adding below line gives warning - plot seems fine though...
                lwd = 0.2,  # Thinner county line widths
                color = "black") +  # Make county lines black
  scale_fill_manual(values = colors,
                    name = "",  # No legend label
                    labels = c("Repub. leaning, below average \nclimate change belief",
                               "Repub. leaning, above average \nclimate change belief",
                               "Dem. leaning, below average \nclimate change belief",
                               "Dem. leaning, above average \nclimate change belief",
                               "\"Split\" county, below average \nclimate change belief",
                               "\"Split\" county county, below average \nclimate change belief")) +
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
  ggtitle("Interaction of 2020 election results and \ncounty climate change belief estimates")

p

# Save plot for potential use in report
ggsave("./plots/election_climate_beliefs_map.png", plot = p,
       device = "png", width = 8, height = 5, units = "in", bg = "white")



# ------ Repeat, but comparing to in-group average climate change belief ------

y$pol_flag <- ifelse(y$prop_rep > 0.525, "rep",
                    ifelse(y$prop_dem > 0.525,"dem", "split"))
                
group_mu <- tapply(y$happening, y$pol_flag, mean)

# Create factor for 6 color-shade pairs, now comparing to in-group mean belief
# in climate change
y$flag.f <- ifelse(y$pol_flag == "rep" & y$happening < group_mu["rep"],
                   "rep_low",
                   ifelse(y$pol_flag == "rep" & y$happening >= group_mu["rep"],
                          "rep_high",
                          ifelse(y$pol_flag == "dem" &
                                   y$happening < group_mu["dem"],
                                 "dem_low",
                                 ifelse(y$pol_flag == "dem" &
                                          y$happening >- group_mu["dem"],
                                        "dem_high",
                                        ifelse(y$happening < group_mu["split"],
                                               "mod_low",
                                               "mod_high")))))
y$flag.f <- factor(y$flag.f, levels = c("rep_low", "rep_high", "dem_low",
                                        "dem_high", "mod_low", "mod_high"))
table(y$flag.f)

# Define specific colors to be used
colors <- c("indianred2",  # Republican, less belief
            "indianred4",  # Republican, more belief
            "lightblue2",  # Democrat, less belief
            "steelblue",  # Democrat, more belief
            "mediumpurple2",  # Purple, less belief
            "darkorchid4")  # Purple, more belief

# Generate a simple map which contains the state outlines
states <- plot_usmap("states",
                     color = "black",
                     fill = alpha(0.01))

# Create map of counties overlaid with state lines for reference
p <- plot_usmap(data = y,
                values = "flag.f",  # Color by flag.f
                # Adding below line gives warning - plot seems fine though...
                lwd = 0.2,  # Thinner county line widths
                color = "black") +  # Make county lines black
  scale_fill_manual(values = colors,
                    name = "",  # No legend label
                    labels = c("Repub. leaning, below average \nclimate change belief",
                               "Repub. leaning, above average \nclimate change belief",
                               "Dem. leaning, below average \nclimate change belief",
                               "Dem. leaning, above average \nclimate change belief",
                               "\"Split\" county, below average \nclimate change belief",
                               "\"Split\" county county, below average \nclimate change belief")) +
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
  ggtitle("Interaction of 2020 election results and \ncounty climate change belief estimates")

p

# Save plot for potential use in report
ggsave("./plots/election_climate_beliefs_map_in_group_comp.png", plot = p,
       device = "png", width = 8, height = 5, units = "in", bg = "white")


