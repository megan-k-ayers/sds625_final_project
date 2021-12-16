# The purpose of this script is to match counties that did experience an extreme
# weather event in 2019 to counties that did not, based on the covariates
# identified using LASSO in clustering.R. I will match counties using the 
# MatchIt R package: 
# https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html

rm(list = ls())
library(MatchIt)
library(ggplot2)
set.seed(947)

# Read in merged and cleaned data, continue only with columns to use for
# matching analysis
x <- read.csv("./processed_data/merged_data.csv")
x <- x[, c("state", "county", "prop_dem", "hisp_ttl_pop", "white_nonhisp_pop",
           "ttl_votes", "prop_lib", "happening", "weather_flag")]

# Matching all counties that experienced weather event in 2019 (i.e.
# any_weather == T) with counties that did not based on covariates.
# Use method = "optimal" because this is "optimal in the sense that that sum of
# the absolute pairwise distances in the matched sample is as small as
# possible" (package documentation). This is advantageous because "it is less
# likely that extreme within-pair distances will be large". 
m <- matchit(weather_flag ~ .,
             data = x[, !names(x) %in% c("state", "county", "happening")],
             method = "optimal")

summary(m)

before <- summary(m)$sum.all[, 3]  # Std. mean diff of covariates before match
after <- summary(m)$sum.matched[, 3]  # " " after match
n <- length(before)

match_assess <- data.frame(name = c(names(before), names(after)),
                           value = c(before, after),
                           when = c(rep("before", n), rep("after", n)))
match_assess$when <- factor(match_assess$when, levels = c("before", "after"))

# Plot before and after covariate means
(balance_plot <- ggplot(data = match_assess,
                       aes(y = reorder(name, value), x = value)) +
  geom_col(aes(fill = when), position = "dodge") +
  xlab("Standardized mean difference") +
  ylab("Covariate name") +
  ggtitle("Standardized mean difference between counties that experienced extreme
weather and those that didn't, before and after matching") +
  scale_fill_manual(name = "",
                      values = c("dimgray", "cornflowerblue")) +
  theme_bw())

ggsave("./writeups/images/matching_balance_plot.png", plot = balance_plot,
       device = "png", width = 8, height = 4, units = "in", bg = "white")

# I'm calling these "treat" and "control" just for intuitive naming, although
# I am NOT claiming that this is a causal inference analysis.
# The matchit function outputs the rows of those treated in match.matrix with
# treated unit row numbers as the row names, and their pairs as the matrix
# entry.
matches <- data.frame(treat = rownames(m$match.matrix),
                      control = m$match.matrix)


# Some checks to convince myself that this did something somewhat reliable...
x_t <- x[matches[, 1], ]
x_c <- x[matches[, 2], ]

# Some matches definitely look as though they are
# better than others. If I had more time I could try to implement this myself.
x[matches[1, 1], ]; x[matches[1, 2], ]  # Pretty different...
x[matches[47, 1], ]; x[matches[47, 2], ]
x[matches[242, 1], ]; x[matches[242, 2], ]  # Closer...

# Pretty low, positive difference (happening is a percentage, not a proportion).
mean(x_t$happening - x_c$happening)

# Perform a paired t-test
t.test(x_t$happening, x_c$happening, paired = TRUE)

# Check assumptions
hist(x_t$happening - x_c$happening, breaks = 30)

png(filename = "./writeups/images/matched_pair_qq.png",
    width = 800, height = 500, pointsize = 20)
qqnorm(x_t$happening - x_c$happening,
       main = "Normal Q-Q Plot: Matched pair difference
in climate change belief")
dev.off()



# --------- Repeating with hurricane occurrence instead of all weather --------- 
x <- read.csv("./processed_data/merged_data.csv")
x <- x[, c("state", "county", "prop_dem", "hisp_ttl_pop", "white_nonhisp_pop",
           "ttl_votes", "prop_lib", "happening", "hurricane_flag")]

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

# Not amazing, again some are better than others. Perfection is probably not
# possible!
x[matches[1, 1], ]; x[matches[1, 2], ]
x[matches[47, 1], ]; x[matches[47, 2], ]
x[matches[102, 1], ]; x[matches[102, 2], ]
x[matches[150, 1], ]; x[matches[150, 2], ]

# Much larger positive difference, interesting, but not sure how much I trust
# the matching!
mean(x_t$happening - x_c$happening)

# Perform a paired t-test. Even more significant than before, but again not
# feeling great about the matching, and would be more interesting if I could
# compare with other individual weather events.
t.test(x_t$happening, x_c$happening, paired = TRUE)
