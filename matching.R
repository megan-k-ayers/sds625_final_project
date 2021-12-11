# The purpose of this script is to match counties that did experience an extreme
# weather event in 2019 to counties that did not, based on the other covariates
# that I selected in analysis_prep.R. I will match counties using the 
# MatchIt R package: 
# https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html

rm(list = ls())
library(MatchIt)
set.seed(947)

# Read in merged and cleaned data, continue only with columns to use for
# matching analysis
x <- read.csv("./processed_data/merged_data.csv")
x <- x[, c("state", "county", "med_age", "asian_pop", "hisp_ttl_pop",
           "non_white_pop", "med_income", "prop_dem", "prop_rep",
           "happening", "weather_flag")]

# Matching all counties that experienced weather event in 2019 (i.e.
# any_weather == T) with counties that did not based on covariates.
# Use method = "optimal" because this is "optimal in the sense that that sum of
# the absolute pairwise distances in the matched sample is as small as
# possible" (package documentation). This is advantageous because "it is less
# likely that extreme within-pair distances will be large". 
m <- matchit(weather_flag ~ .,
             data = x[, !names(x) %in% c("state", "county", "happening")],
             method = "optimal")

# I'm calling these "treat" and "control" just for intuitive naming, although
# I am NOT claiming that this is a causal inference analysis.
# The matchit function outputs the rows of those treated in match.matrix with
# treated unit row numbers as the row names, and their pairs as the matrix
# entry.
matches <- data.frame(treat = rownames(m$match.matrix),
                      control = m$match.matrix)
summary(matches)

# Some checks to convince myself that this did something somewhat reliable...
x_t <- x[matches[, 1], ]
x_c <- x[matches[, 2], ]

mean(x_t$med_age - x_c$med_age) / sd(x$med_age)
mean(x_t$asian_pop - x_c$asian_pop) / sd(x$asian_pop)
mean(x_t$hisp_ttl_pop - x_c$hisp_ttl_pop) / sd(x$hisp_ttl_pop)
mean(x_t$med_income - x_c$med_income) / sd(x$med_income)
mean(x_t$prop_dem - x_c$prop_dem) / sd(x$prop_dem)
mean(x_t$prop_rep - x_c$prop_rep) / sd(x$prop_rep)
mean(x_t$non_white_pop - x_c$non_white_pop) / sd(x$non_white_pop)


# Doing some spot-checking, some matches definitely look as though they are
# better than others. If I have time I could try to implement this myself.
x[matches[1, 1], ]; x[matches[1, 2], ]  # Pretty different...
x[matches[47, 1], ]; x[matches[47, 2], ]
x[matches[242, 1], ]; x[matches[242, 2], ]  # Pretty close...

# Pretty low, positive difference (happening is a percentage, not a proportion).
mean(x_t$happening - x_c$happening)



# --------- Repeating with hurricane occurrence instead of all weather --------- 
x <- read.csv("./processed_data/merged_data.csv")
x <- x[, c("state", "county", "med_age", "asian_pop", "hisp_ttl_pop",
           "non_white_pop", "med_income", "prop_dem", "prop_rep",
           "happening", "hurricane_flag")]

# Matching all counties that experienced weather event in 2019 (i.e.
# any_weather == T) with counties that did not based on covariates.
# Use method = "optimal" because this is "optimal in the sense that that sum of
# the absolute pairwise distances in the matched sample is as small as
# possible" (package documentation). This is advantageous because "it is less
# likely that extreme within-pair distances will be large". 
m <- matchit(hurricane_flag ~ .,
             data = x[, !names(x) %in% c("state", "county", "happening")],
             method = "optimal")

# I'm calling these "treat" and "control" just for intuitive naming, although
# I am NOT claiming that this is a causal inference analysis.
# The matchit function outputs the rows of those treated in match.matrix with
# treated unit row numbers as the row names, and their pairs as the matrix
# entry.
matches <- data.frame(treat = rownames(m$match.matrix),
                      control = m$match.matrix)
summary(matches)

# Some checks to convince myself that this did something somewhat reliable...
x_t <- x[matches[, 1], ]
x_c <- x[matches[, 2], ]

mean(x_t$med_age - x_c$med_age) / sd(x$med_age)
mean(x_t$asian_pop - x_c$asian_pop) / sd(x$asian_pop)
mean(x_t$hisp_ttl_pop - x_c$hisp_ttl_pop) / sd(x$hisp_ttl_pop)
mean(x_t$med_income - x_c$med_income) / sd(x$med_income)
mean(x_t$prop_dem - x_c$prop_dem) / sd(x$prop_dem)
mean(x_t$prop_rep - x_c$prop_rep) / sd(x$prop_rep)
mean(x_t$non_white_pop - x_c$non_white_pop) / sd(x$non_white_pop)


# Doing some spot-checking, some matches definitely look as though they are
# better than others. If I have time I could try to implement this myself.
x[matches[1, 1], ]; x[matches[1, 2], ]
x[matches[47, 1], ]; x[matches[47, 2], ]
x[matches[102, 1], ]; x[matches[102, 2], ]

# Larger positive difference, interesting
mean(x_t$happening - x_c$happening)

