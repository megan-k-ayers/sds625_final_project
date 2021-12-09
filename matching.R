rm(list = ls())
library(MatchIt)
set.seed(947)

x <- read.csv("./processed_data/merged_data.csv")
x <- x[, c("state", "county", "med_age", "asian_pop", "hisp_ttl_pop",
           "non_white_pop", "med_income", "prop_dem", "prop_rep",
           "happening", "any_weather")]

# NEXT:
# Attempt to look for relationship in two ways:

# Matching, take difference in beliefs, average over differences.

# Or cluster continuous variables using some legal/political definitions, 
# then do ANOVA.  


m <- matchit(any_weather ~ .,
             data = x[, !names(x) %in% c("state", "county", "happening")],
             method = "optimal")

# I'm calling these "treat" and "control" just for my own memory, this is NOT
# a causal inference analysis
matches <- data.frame(treat = rownames(m$match.matrix),
                      control = m$match.matrix)
summary(matches)

# TODO: If using this package, at least do some checks to convince self that
# on average, matches are actually pretty close together.
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

belief_t <- x[matches$treat, "happening"]
belief_c <- x[matches$control, "happening"]

# Pretty low. 
mean(belief_t - belief_c)






