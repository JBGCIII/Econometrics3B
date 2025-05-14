
#I've Installed the required package using the console not the code
# as instructed in Lab 1.R
# Load required libraries

# Load required libraries
library(Ecdat)
library(dynlm)      # To estimate AR(p) by OLS
library(forecast)    # For ACF
library(portes)      # For Ljung-Box test
library(tseries)     # For Jarque-Bera test

# Load the dataset and extract the 'income' variable as a time series
data("IncomeUK")
income_ts <- IncomeUK[, "income"]

# First difference of income (change in income)
d_income <- diff(income_ts)

# Use the embed function to create a matrix with lagged values
# dimension = 1 + 5 for AR(5) (1 for the dependent variable and 5 for the lags)
d_income_lags <- embed(d_income, dimension = 1 + 5)

# Convert to time series format with correct start and frequency
d_income_lags_ts <- ts(d_income_lags, start = c(1971, 2), frequency = 4)

# Estimate AR models using dynlm()
AR1 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2])        # AR(1)
AR2 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:3])      # AR(2)
AR3 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:4])      # AR(3)
AR4 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:5])      # AR(4)
AR5 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:6])      # AR(5)

# Summarize results for each AR model
summary(AR1)
summary(AR2)
summary(AR3)
summary(AR4)
summary(AR5)

# Calculate and compare AIC values for each model
AIC_values <- c(AIC(AR1), AIC(AR2), AIC(AR3), AIC(AR4), AIC(AR5))
cat("AIC Values:\n")
print(AIC_values)

# Identify the model with the lowest AIC
best_model_index <- which.min(AIC_values)
cat("The best model according to AIC is AR(", best_model_index, ")\n", sep = "")

