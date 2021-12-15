# The purpose of this script is to try using just a linear model to explore
# the relationship between climate change belief, the identified covariates from
# analysis_prep.R, and experiencing an extreme weather event.

rm(list = ls())

# Read in merged and cleaned data
x <- read.csv("./processed_data/merged_data.csv")

lm_fit <- lm(happening ~ med_age + sqrt(asian_pop) + sqrt(hisp_ttl_pop) +
                + sqrt(non_white_pop) + log(med_income) + prop_dem +
                weather_flag, data = x)

summary(lm_fit)

# Another small, positive value as an estimate for how experiencing an extreme
# weather event could have increased climate change beliefs. Note that this
# is larger than the matching analysis value.
lm_fit$coefficients["weather_flagTRUE"]

# Checking model assumptions...
# Save model diagnostic plots for report appendix
dev.off()
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

rm(list = ls())

# Read in merged and cleaned data
x <- read.csv("./processed_data/merged_data.csv")

lm_fit <- lm(happening ~ med_age + sqrt(asian_pop) + sqrt(hisp_ttl_pop) +
               + sqrt(non_white_pop) + log(med_income) + prop_dem +
               hurricane_flag, data = x)

summary(lm_fit)

# Another small, positive value
lm_fit$coefficients["hurricane_flagTRUE"]

# Checking model assumptions...

# Normality of residuals looks good
hist(lm_fit$residuals, breaks = 40)

# Heteroscedaciditicitidy looks fine
plot(lm_fit$fitted.values, lm_fit$residuals)
