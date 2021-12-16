# The purpose of this script is first to explore any potential differences
# between counties that have and have not experienced extreme weather (as I've
# defined it in analysis_prep.R with weather_flag), with respect to all the
# covariates that have been collected.
# Then, I use LASSO to perform dimensionality reduction in the covariates and
# focus on the variable that this process identifies as being most "predictive"
# via the size of their coefficients.
# I will then dive deeper into exploring those top ~10 covariates by using them
# to form clusters of counties and look for any heterogeneity within different
# clusters of counties wrt how climate change beliefs relate to the extreme
# weather flag, Rather than using an unsupervised method, I will use some
# intuitive thresholds to split counties into 2 groups for each variable.
#

rm(list = ls())
library(ggplot2)
library(tidyverse)
library(glmnet)

# - READ MERGED DATA, EXPLORE DIFFERENCES IN COUNTIES W/, W/O EXTREME WEATHER -

# Read in merged and cleaned data
x <- read.csv("./processed_data/merged_data.csv")

# There are a few counties with over 100% total votes! I trust the Census values
# for population totals more than I trust the election data source, which means
# this could bad values for the vote proportions. Since this is only four
# counties, none of which are in the smaller "extreme-weather" group, I will
# just drop them.
nrow(x[x$ttl_votes > 1, ])
x <- x[x$ttl_votes <= 1, ]

# Split into two data frames: one for counties that have the extreme weather
# flag on, and one for those that don't
x_w <- x[x$weather_flag, ]  # Extreme weather (EW) counties
nrow(x_w)
x_c <- x[!x$weather_flag, ]  # "Control" counties
nrow(x_c)

# Compare summary statistics between the two groups of counties, starting with
# variables that I intuitively expect to be influential in climate change belief
# and that may reasonably vary geographically
round(table(x_w$poli_lean) / nrow(x_w), 3)  # EW counties political leaning
round(table(x_c$poli_lean) / nrow(x_c), 3)  # Control counties political leaning

# Median age
ggplot(x, aes(x = med_age, after_stat(density), fill = weather_flag)) +
  geom_histogram(alpha=0.5, position="identity", bins = 50,
                 col = "black", lwd = 0.25) +
  ggtitle("Comparing median age distrbution between counties that experienced
extreme weather and those that didn't")

# Non-Hispanic white population proportion
ggplot(x, aes(x = white_nonhisp_pop, after_stat(density), fill = weather_flag)) +
  geom_histogram(alpha=0.5, position="identity", bins = 50,
                 col = "black", lwd = 0.25) +
  ggtitle("Comparing proportion of non-Hispanic white population between counties that experienced
extreme weather and those that didn't")

# (Log) Median income
ggplot(x, aes(x = log(med_income), after_stat(density), fill = weather_flag)) +
  geom_histogram(alpha=0.5, position="identity", bins = 50,
                 col = "black", lwd = 0.25) +
  ggtitle("Comparing log of median income between counties that experienced
extreme weather and those that didn't")

# Now look at big summary of all variables (except for weather flags, 
# political leaning, which are discrete and also we already know)
weath_cols <- grep("any_weather_[0-9]{4}|hurricane_[0-9]{4}", names(x))
leave_out <- c("c_fips", "s_fips", "state", "county",
               names(x)[weath_cols], "happening_oppose", "poli_lean",
               "hurricane_flag", "weather_flag", "ttl_pop")
summary(x_w[, !names(x_w) %in% leave_out], digits = 4)

# Want to plot stacked boxplots for all continuous data... need a long df for
# this to use ggplot
x_long <- data.frame(var = character(),
                   s_fips = integer(),
                   c_fips = integer(),
                   value = numeric(),
                   weather_flag = logical())

# Probably a more elegant way to do that iterate definition statement...
for (col in names(x[, !names(x) %in% leave_out])) {
  these <- x[, c("s_fips", "c_fips", col, "weather_flag")]
  names(these) <- c("s_fips", "c_fips", "value", "weather_flag")
  these <- cbind(data.frame(var = rep(col, nrow(these))), these)
  x_long <- rbind(x_long, these)
}

# Apply some transformations to better see distributions - using sqrt for
# race/ethnicity and 3rd party election proportion transforms because some
# counties have 0's

# Function to apply given transformation, and rename the variable to reflect the
# change
transform_var <- function(var, transform){
  
  these <- x_long[x_long$var == var, ]
  if (transform == "sqrt") {
    these$value <- sqrt(these$value)
  } else if (transform == "log") {
    these$value <- log(these$value)
  } else { return(NA) }
  these$var <- paste(c(var, "_", transform), collapse = "")
  
  return(these)
  
}

x_long[x_long$var == "asian_pop",] <- transform_var("asian_pop", "sqrt")
x_long[x_long$var == "black_hisp_pop",] <- transform_var("black_hisp_pop",
                                                         "sqrt")
x_long[x_long$var == "black_pop",] <- transform_var("black_pop","sqrt")
x_long[x_long$var == "hisp_ttl_pop",] <- transform_var("hisp_ttl_pop", "sqrt")
x_long[x_long$var == "male_pop",] <- transform_var("male_pop", "log")
x_long[x_long$var == "med_income",] <- transform_var("med_income", "log")
x_long[x_long$var == "mult_race_pop",] <- transform_var("mult_race_pop", "sqrt")
x_long[x_long$var == "native_pop",] <- transform_var("native_pop", "sqrt")
x_long[x_long$var == "other_race_pop",] <- transform_var("other_race_pop",
                                                         "sqrt")
x_long[x_long$var == "pacific_pop",] <- transform_var("pacific_pop", "sqrt")
x_long[x_long$var == "prop_green",] <- transform_var("prop_green", "sqrt")
x_long[x_long$var == "prop_lib",] <- transform_var("prop_lib", "sqrt")
x_long[x_long$var == "prop_other",] <- transform_var("prop_other", "sqrt")
x_long[x_long$var == "white_hisp_pop",] <- transform_var("white_hisp_pop",
                                                         "sqrt")

(p <- ggplot(data = x_long, aes(x = value, col = weather_flag)) +
  facet_wrap(vars(var), scales = "free", nrow = 6, ncol = 4) +
  geom_boxplot(aes(col = weather_flag)) +
  xlab("") +
  scale_color_discrete(name = "Experienced extreme weather") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Comparing covariate distributions for counties that did vs. did not experience
extreme weather"))

# Save this plot for report
ggsave("./writeups/images/covariate_dist_comp.png", plot = p,
       device = "png", width = 8, height = 8, units = "in", bg = "white")


# ---------------------- VARIABLE SELECTION WITH LASSO ----------------------

# Want to have `happening` to plot with every other covariate, so remove it as
# a "var" and join it back on
x_long <- x_long[x_long$var != "happening", ]
x_long <- merge(x_long, x[, c("c_fips", "s_fips", "happening")],
              by = c("c_fips", "s_fips"))

(p <- ggplot(data = x_long, aes(x = value, y = happening, col = weather_flag)) +
  facet_wrap(vars(var), scales = "free", nrow = 7, ncol = 4) +
  geom_point(alpha = 0.5) +
  xlab("") +
  scale_color_discrete(name = "Experienced extreme weather") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Belief in climate change by each covariate"))

# Save this plot for report
ggsave("./writeups/images/covariates_vs_happening.png", plot = p,
       device = "png", width = 8, height = 8, units = "in", bg = "white")


# None of these are egregiously non-linear looking, so proceed with LASSO
# for variable selection


# Create a grid of values of lambda (between 0 and 0.2):
lambdas <- seq(0, 0.2, 0.002)
set.seed(947)

# Splitting up into a train/test pair to check for over-fitting
inds = sample(1:nrow(x), nrow(x)/2)
data = pivot_wider(x_long, names_from = "var", values_from = "value")

# Because prop_rep and prop_dem  is almost perfectly negatively correlated,
# I will remove one from variable selection. Similarly, going to use just
# median age (not median age split out by gender)
data <- data[,!names(data) %in% c("prop_rep", "female_med_age", "male_med_age")]

train_y = as.numeric(unlist(data[inds, "happening"]))
train_x = data[inds, !names(data) %in% c("c_fips", "s_fips", "weather_flag",
                                           "happening")]
train_x <- scale(train_x)  # Scale data before using LASSO for var selection
train_y <- scale(train_y)
cv_res <- cv.glmnet(as.matrix(train_x), train_y, alpha = 1)

plot(cv_res)
(best_l <- cv_res$lambda.min)

# See how "best" lambda calculates coefficients
train_fit <- glmnet(train_x, train_y, alpha = 1, lambda = best_l)
coefs <- coef(train_fit) 
coefs[order(-abs(coefs[, 1])), 1]

# Check on the testing data to see if the same ordering is appearing
test_x <- data[-inds, !names(data) %in% c("c_fips", "s_fips", "weather_flag",
                                         "happening")]
test_x <- scale(test_x)
test_y <- as.numeric(unlist(data[-inds, "happening"]))
test_y <- scale(test_y)
test_fit <- glmnet(as.matrix(test_x), test_y, alpha = 1,
                   lambda = best_l)
test_coefs <- coef(test_fit)
test_coefs[order(-abs(test_coefs[, 1])), 1]

# Some have switched around, but it seems mostly due to the fact that there is
# some collinearity within the race/ethnicity categories. 

coefs[order(-abs(coefs[, 1])), 1][1:10]
test_coefs[order(-abs(test_coefs[, 1])), 1][1:10]

# Consistenly between the testing and training, prop_dem, hisp_ttl_pop_sqrt,
# white_nonhisp_pop, prop_lib_sqrt, ttl_votes, asian_pop_sqrt, prop_lib_sqrt,
# black_pop_sqrt, and mult_race_pop_sqrt
# made it into the top 10 largest (in absolute value) coefficient list. A little
# surprising to me that age is not consistently large, but okay.
# Looking back at prop_lib, because I am worried about the outlier there.


# ---------------------------- CLUSTER EXPLORATION ----------------------------
# With respect to the variables pulled out by LASSO, explore differences in
# county climate change belief when they are grouped by these variables at a
# high level. Are there heterogeneous relationships with climate change belief
# within these clusters?

# First, the LASSO procedure identified prop_dem as the largest coefficient
# by far, both in training and in the test. This is in line with my intuition,
# and I'm curious to see alone just how much political leaning can explain
# about climate change beliefs.

# Know response is fairly normal without any transformations
hist(x$happening, breaks = 50)
table(x$poli_lean)

# Equal variance assumption is met 
sd(x[x$poli_lean == "red", "happening"])
sd(x[x$poli_lean == "blue", "happening"])

# Start with super basic ANOVA test on political leaning
tapply(x$happening, x$poli_lean, mean)  # Checks out! 
basic_lm <- lm(happening ~ poli_lean, data = x)
summary(basic_lm)

x$poli_lean_f <- factor(x$poli_lean, levels = c("blue", "red"))

# Make plot visualizing this test 
p <- ggplot(data = x, aes(x = poli_lean_f, y = happening)) +
  geom_jitter(aes(col = poli_lean_f), alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete(labels = c("Democrat", "Republican")) +
  xlab("") +
  ylab("") +
  ggtitle("County belief in climate change by 2020 election results") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("./writeups/images/election_party_anova.png", plot = p,
       device = "png", width = 8, height = 5, units = "in", bg = "white")


# Now starting the full splitting out of the population into clusters...

# The LASSO procedure gave the proportion of the Hispanic population a
# consistently large coefficient compared to other variables. Split on this
# into two categories for simplicity, with the threshold being the national
# proportion of the Hispanic population (grouping counties above/below this
# threshold).
# The Census reported in their 2019 population estimates that 18.5% of
# the national population is "Hispanic or Latino".
# https://www.census.gov/quickfacts/fact/table/US/PST045219
x$hisp_ttl_pop_g <- ifelse(x$hisp_ttl_pop < 0.185, "low", "high")
table(x$hisp_ttl_pop_g)  # This is not balanced, but I prefer interpretable to
# well balanced with some random threshold for this initial exploration
table(x$hisp_ttl_pop_g, x$poli_lean)

# Repeat this with white_nonhisp_pop, the proportion of the non-Hispanic 
# white population, which was the next largest coefficient in both test and
# training
# The Census reported in their 2019 population estimates that 60.1% of
# the national population is "White alone, not Hispanic or Latino".
# https://www.census.gov/quickfacts/fact/table/US/PST045219
x$white_nonhisp_pop_g <- ifelse(x$white_nonhisp_pop < 0.601, "low", "high")
table(x$white_nonhisp_pop_g)
table(x$hisp_ttl_pop_g, x$poli_lean)


# I am going to stop there with the race/ethnicity categories, because at this
# point we will be getting into groups that have pretty small intersections,
# and some collinearity. Conveniently, the two largest coefficients
# corresponding to a race/ethnicity proportion were the two largest disjoint
# groups. 

# Next largest coefficient was ttl_votes, the proportion of the county
# population that voted in the 2020 general election.
hist(x$ttl_votes, breaks = 40)
abline(v = mean(x$ttl_votes), col = "red")
mean(x$ttl_votes)

# Categorize each county based on if half of the population voted, or not
# (fortunately happens to be pretty intuitive as well as balanced)
x$vote_prop_g <- ifelse(x$ttl_votes <= 0.5, "low", "high")
table(x$vote_prop_g)
table(x$vote_prop_g, x$poli_lean)
table(x$vote_prop_g, x$hisp_ttl_pop_g)
table(x$vote_prop_g, x$white_nonhisp_pop_g)


# The last non race/ethnicity related coefficient with a top 10 largest absolute
# value shared by the test/train LASSO procedure was the (sqrt) proportion of
# Libertarian vote in the 2020 general election.
hist(x$prop_lib, breaks = 40)
abline(v = mean(x$prop_lib), col = "red")
mean(x$prop_lib)  # This is so small! But let's try it. 

x$prop_lib_g <- ifelse(x$prop_lib <= mean(x$prop_lib), "low", "high")
table(x$prop_lib_g)
table(x$vote_prop_g, x$prop_lib_g)
table(x$prop_lib_g, x$hisp_ttl_pop_g)
table(x$prop_lib_g, x$white_nonhisp_pop_g)



# Collect counties corresponding to permutations of these categories. There
# are a lot of combinations, but I expect a large number will be unusable due
# to size
y <- x[, c("state", "county", "weather_flag", "happening", "poli_lean",
           "hisp_ttl_pop_g", "white_nonhisp_pop_g", "vote_prop_g",
           "prop_lib_g")]
grid <- expand.grid(unique(y$weather_flag), unique(y$poli_lean),
                    unique(y$hisp_ttl_pop_g), unique(y$white_nonhisp_pop_g),
                    unique(y$vote_prop_g), unique(x$prop_lib_g))
names(grid) <- c("weather_flag", "poli_lean", "hisp_ttl_pop_g",
                 "white_nonhisp_pop_g", "vote_prop_g", "prop_lib_g")

y$group <- 0
grid$n <- 0
grid$grp_sd <- 0
for (i in 1:nrow(grid)) {
  row <- grid[i, ]
  these <- which(y$poli_lean == row$poli_lean &
                 y$hisp_ttl_pop_g == row$hisp_ttl_pop_g &
                 y$vote_prop_g == row$vote_prop_g &
                 y$white_nonhisp_pop_g == row$white_nonhisp_pop_g &
                 y$prop_lib_g == row$prop_lib_g &
                 y$weather_flag == row$weather_flag)
  y[these, "group"] <- i
  grid[i, "n"] <- length(these)
  grid[i, "grp_sd"] <- sd(y[these, "happening"], na.rm = T)
}

# Some of these groups contain very few counties, and recall that an ANOVA
# assumption is similar variances between comparison groups. Drop pairs (groups
# the same except for any_weather) that have larger variance > 2*(min variance)
grid

drop_grps <- sapply(seq(1, nrow(grid), 2), function(i){
  if (is.na(grid[i, "grp_sd"]) | is.na(grid[i+1, "grp_sd"])) return(FALSE)
  return( max(grid[c(i, i+1), "grp_sd"]) > 2*min(grid[c(i, i+1), "grp_sd"]) )
})

drop_grps <- seq(1, nrow(grid), 2)[drop_grps]
drop_grps <- c(drop_grps, drop_grps + 1)
grid <- grid[-drop_grps, ]

# Some groups with very small sample sizes remain, and based on initial looks
# at qqplots below, I'm going to make a cut-off of 20 counties per group
drop_grps <- sapply(1:nrow(grid), function(i){
  if (grid[i, "n"] < 20 & grid[i, "weather_flag"]){
    return(i - 1) # Skip up to row where any_weather == FALSE for this group
  } else if (grid[i, "n"] < 20) { return(i) } else {return(NA)}
})
drop_grps <- unique(drop_grps[!is.na(drop_grps)])
drop_grps <- c(drop_grps, drop_grps + 1)
grid <- grid[-drop_grps, ]

y <- y[y$group %in% rownames(grid), ]

# Now that sample size / variance conditions are somewhat addressed, check
# out response normality within each group. The smaller groups don't look
# great but pretty hard to tell, the rest look okay.
for (i in rownames(grid)) {
  
  these <- y[y$group == i, "happening"]
  qqnorm(these, main = paste("group:", i))
  
}

# Get a glimpse at the group means before running some ANOVA analysis
grid$avg_happ <- sapply(rownames(grid), function(i){
  mean(y[y$group == i, "happening"])})


# Since I am most interested in how the extreme weather events affect the
# climate change opinions, I am going to run one-way ANOVA between each of the
# groups differing only by the any_weather flag, which is...
nrow(grid)/2
# ... different tests. Although I will need to apply a correction for multiple
# testing this way, if I test the hypothesis that all means are 0, I know
# I am going to get a pretty meaningless positive result (we already have
# established that some covariates have a strong impact on climate change
# belief).

res <- data.frame(group = integer(),
                  coef = numeric(),
                  p = numeric())
for (i in as.numeric(rownames(grid))) {
  
  if (i %% 2 == 0) next  # Skip even groups - these are paired with an odd group
  
  these <- y[y$group %in% c(i, i + 1), c("group", "happening")]
  these$group <- factor(these$group, levels = c(i, i+1))
  fit <- lm(happening ~ group, data = these)
  coef <- as.numeric(fit$coefficients[2])
  p <- as.numeric(summary(fit)$coefficients[2, 4])
  res <- rbind(res, data.frame(group = i, coef = coef, p = p))
  
}

# For this exploratory analysis I will control FDR instead of the family
# wise error rate - the stakes are low if there is one or two false positives,
# so using Benjamini-Hochberg correction (less conservative) seems appropriate.
res$p_bh <- p.adjust(res$p, method = "hochberg")
res[order(res$p_bh), ]

