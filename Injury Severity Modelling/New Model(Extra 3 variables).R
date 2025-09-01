### Set working directory (adjust to your location)
setwd("/Users/anshukryadav/Downloads/CE687_Lab_Ordered_Response_52bc21f3-b49e-4afc-9c77-09d21dac5835")
library(tidyverse)
library(MASS)

### Load the dataset
dat <- read.csv("Michigan_Motorcycle_Non_Intersection_Data_Subset.csv", stringsAsFactors = TRUE)

### Create ordered response variable (Injury Severity)
dat$Injury_Severity <- 0
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Possible Injury (C)"] <- 1
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Suspected Minor Injury (B)"] <- 2
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Suspected Serious Injury (A)"] <- 3
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Fatal Injury (K)"] <- 4
dat$Injury_Severity <- as.factor(dat$Injury_Severity)

### Explanatory variables
dat$Urban <- as.numeric(dat$Rural.Urban.Area == "Urban")
dat$Pedestrian <- as.numeric(dat$Crash..Pedestrian == "Pedestrian Involved")
dat$Late_Night <- as.numeric(dat$Time.of.Day %in% c("12:00 Midnight - 12:59 AM",
                                                    "1:00 AM - 1:59 AM",
                                                    "2:00 AM - 2:59 AM",
                                                    "3:00 AM - 3:59 AM",
                                                    "4:00 AM - 4:59 AM"))
dat$Parked_Vehicle <- as.numeric(dat$Crash..Lane.Departure == "Parked Vehicle")

### Additional categorical variables
dat$X_LIGHT <- factor(dat$Lighting.Conditions)
dat$X_SUR_COND <- factor(dat$Road.Conditions)
dat$X_CRASH_TYPE <- factor(dat$Crash.Type)

### ----- Ordered Probit Model -----
m1_probit_updated <- polr(Injury_Severity ~ Speed.Limit.at.Crash.Site + Urban +
                            Pedestrian + Parked_Vehicle + Late_Night +
                            X_LIGHT + X_SUR_COND + X_CRASH_TYPE,
                          data = dat, method = "probit")

cat("----- Ordered Probit Model Summary -----\n")
summary(m1_probit_updated)

# Log-Likelihood, AIC, BIC
ll_probit <- logLik(m1_probit_updated)
aic_probit <- AIC(m1_probit_updated)
bic_probit <- BIC(m1_probit_updated)

# Null model for pseudo-R²
null_model_probit <- polr(Injury_Severity ~ 1, data = dat, method = "probit")
ll_null_probit <- logLik(null_model_probit)

# McFadden's Pseudo-R²
pseudoR2_probit <- 1 - as.numeric(ll_probit) / as.numeric(ll_null_probit)

cat("Log-Likelihood:", ll_probit, "\n")
cat("AIC:", aic_probit, "\n")
cat("BIC:", bic_probit, "\n")
cat("McFadden's Pseudo-R²:", pseudoR2_probit, "\n\n")

### ----- Ordered Logit Model -----
m1_logit_updated <- polr(Injury_Severity ~ Speed.Limit.at.Crash.Site + Urban +
                           Pedestrian + Parked_Vehicle + Late_Night +
                           X_LIGHT + X_SUR_COND + X_CRASH_TYPE,
                         data = dat, method = "logistic")

cat("----- Ordered Logit Model Summary -----\n")
summary(m1_logit_updated)

# Log-Likelihood, AIC, BIC
ll_logit <- logLik(m1_logit_updated)
aic_logit <- AIC(m1_logit_updated)
bic_logit <- BIC(m1_logit_updated)

# Null model for pseudo-R²
null_model_logit <- polr(Injury_Severity ~ 1, data = dat, method = "logistic")
ll_null_logit <- logLik(null_model_logit)

# McFadden's Pseudo-R²
pseudoR2_logit <- 1 - as.numeric(ll_logit) / as.numeric(ll_null_logit)

cat("Log-Likelihood:", ll_logit, "\n")
cat("AIC:", aic_logit, "\n")
cat("BIC:", bic_logit, "\n")
cat("McFadden's Pseudo-R²:", pseudoR2_logit, "\n")