# The purpose of this script is to try using just a linear model to explore
# the relationship between climate change belief, the identified
# covariates from the LASSO procedure in clustering.R, and experiencing an
# extreme weather event.

# I will use more of the variables here than in matching.R (in the matching
# procedure, I was worried about worsening the quality of the matches by
# including even more of the covariates).

rm(list = ls())

# Read in merged and cleaned data
x <- read.csv("./processed_data/merged_data.csv")

x <- x[, c("county", "state", "weather_flag", "hurricane_flag", "happening",
           "prop_dem", "hisp_ttl_pop", "white_nonhisp_pop",
           "ttl_votes", "asian_pop", "black_pop", "mult_race_pop", "prop_lib",
           "med_age")]

# Recalculate the transformations from clustering.R
x$hisp_ttl_pop_sqrt <- sqrt(x$hisp_ttl_pop)
x$asian_pop_sqrt <- sqrt(x$asian_pop)
x$black_pop_sqrt <- sqrt(x$black_pop)
x$mult_race_pop_sqrt <- sqrt(x$mult_race_pop)
x$prop_lib_sqrt <- sqrt(x$prop_lib)


# ----------------- MODELING -----------------

lm_fit <- lm(happening ~ prop_dem + hisp_ttl_pop_sqrt + white_nonhisp_pop +
               ttl_votes + asian_pop_sqrt + black_pop_sqrt +
               mult_race_pop_sqrt + + prop_lib_sqrt + med_age + weather_flag,
             data = x)

summary(lm_fit)

# Another small, positive value as an estimate for how experiencing an extreme
# weather event could have increased climate change beliefs. Note that this
# is larger than the matching analysis value.
lm_fit$coefficients["weather_flagTRUE"]

# Checking model assumptions...
# Save model diagnostic plots for report appendix
png(filename = "./writeups/images/regression_residuals_hist.png",
    width = 800, height = 500, pointsize = 20)
# Normality of residuals looks good
hist(lm_fit$residuals, breaks = 40, xlab = "Residuals",
     main = "Regression residuals normality check")
dev.off()

png(filename = "./writeups/images/regression_variance_check.png",
    width = 800, height = 500, pointsize = 20)
# Heteroscedaciditicitidy looks fine
plot(lm_fit$fitted.values, lm_fit$residuals, xlab = "Fitted Values",
     ylab = "Residuals", main = "Regression heteroscedasticity check")
dev.off()



# --------- Repeating with hurricane occurrence instead of all weather --------- 

lm_fit <- lm(happening ~ prop_dem + hisp_ttl_pop_sqrt + white_nonhisp_pop +
               ttl_votes + asian_pop_sqrt + black_pop_sqrt +
               mult_race_pop_sqrt + hurricane_flag, data = x)

summary(lm_fit)

# Another small, positive value, though larger than any weather
lm_fit$coefficients["hurricane_flagTRUE"]

# Checking model assumptions...
# Save model diagnostic plots for report appendix
png(filename = "./writeups/images/regression_residuals_hist_hurricane.png",
    width = 800, height = 500, pointsize = 20)
# Normality of residuals looks good
hist(lm_fit$residuals, breaks = 40, xlab = "Residuals",
     main = "Regression residuals normality check - HURRICANE")
dev.off()

png(filename = "./writeups/images/regression_variance_check_hurricane.png",
    width = 800, height = 500, pointsize = 20)
# Heteroscedaciditicitidy looks fine
plot(lm_fit$fitted.values, lm_fit$residuals, xlab = "Fitted Values",
     ylab = "Residuals", main = "Regression heteroscedasticity check - HURRICANE")
dev.off()

