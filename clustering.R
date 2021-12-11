# The purpose of this script is to try to cluster counties by using discrete
# divisions of the continuous variables identified as strong covariates in
# analysis_prep.R. Using these clusters I will perform hypothesis testing on
# their average proportions of belief in climate change.

rm(list = ls())

# Read in merged and cleaned data
x <- read.csv("./processed_data/merged_data.csv")

# ------------------ Discretizing the continuous variables ------------------

# Age: Culturally, I think it is safe to say that we recognize a distinction
# between young adults (18-35) and other adults, so this is how I will divide
# up the median age variable (could also split out middle aged and older adults
# but there are not many counties with median age older than 56)
x$med_age_g <- ifelse(x$med_age <= 35, "YA", "MA")
table(x$med_age_g)

# Race: The Census reported in their 2019 population estimates that 76.3% of
# the national population is "White alone", and 18.5% is Hispanic or Latino.
# In favor of minimizing the amount of clusters, I will move forward just
# splitting counties into those above/below the national White proportion
# since this appeared to be most predictive.
# https://www.census.gov/quickfacts/fact/table/US/PST045219
x$white_pop_g <- ifelse(x$non_white_pop < 1 - 0.763, "high", "low")
table(x$white_pop_g)

# Income: The Census reported in their 2019 population estimates that the 
# national median income is $62,843, so split counties into those with median
# incomes above and below this.
# https://www.census.gov/quickfacts/fact/table/US/PST045219
x$med_income_g <- ifelse(x$med_income < 62843, "low", "high")
table(x$med_income_g)



# These groups define 16 clusters...
y <- x[, c("state", "county", "weather_flag", "happening", "poli_lean",
           "med_age_g", "white_pop_g", "med_income_g")]

grid <- expand.grid(unique(y$weather_flag), unique(y$poli_lean),
                    unique(y$med_age_g), unique(y$white_pop_g),
                    unique(y$med_income_g))
names(grid) <- c("weather_flag", "poli_lean", "med_age_g", "white_pop_g",
                 "med_income_g")

y$group <- 0
grid$n <- 0
grid$grp_var <- 0
for (i in 1:nrow(grid)) {
  row <- grid[i, ]
  these <- which(y$poli_lean == row$poli_lean &
                   y$med_age_g == row$med_age_g &
                   y$white_pop_g == row$white_pop_g &
                   y$med_income_g == row$med_income_g &
                   y$weather_flag == row$weather_flag)
  y[these, "group"] <- i
  grid[i, "n"] <- length(these)
  grid[i, "grp_var"] <- var(y[these, "happening"])
}

# Some of these groups contain very few counties, and recall that an ANOVA
# assumption is similar variances between comparison groups. Drop pairs (groups
# the same except for any_weather) that have larger variance > 2*(min variance)
grid

drop_grps <- sapply(1:(nrow(grid)/2), function(i){
  return( max(grid[c(i, i+1), "grp_var"]) > 2*min(grid[c(i, i+1), "grp_var"]) )
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
# established that the other covariates have a strong impact on climate change
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
# Surprisingly, first significant coefficient indicates that experiencing
# extreme weather makes you LESS likely to belief in climate change... This is
# comparing counties that are red, older, whiter, and lower income (the
# largest group).
# The second significant coefficient indicates the reverse for counties that are
# red, older, lower income, but have a larger non-white population.
# Don't trust this method very much in either case - the bins feel too reductive
# and most of the groups are lost due to having too few cases.
res$p_bh <- p.adjust(res$p, method = "hochberg")
res[order(res$p_bh), ]



