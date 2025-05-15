
#I've Installed the required package using the console not the code
# as instructed in Lab 1.R

#Problem 1
#Part I
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


#Part II
# Calculate and compare AIC values for each model
AIC_values <- c(AIC(AR1), AIC(AR2), AIC(AR3), AIC(AR4), AIC(AR5))
cat("AIC Values:\n")
print(AIC_values)

# Identify the model with the lowest AIC
best_model_index <- which.min(AIC_values)
cat("The best model according to AIC is AR(", best_model_index, ")\n", sep = "")

#Part III
#Ljung-Box test on residuals
#p-value < 0.05 → Reject the null → Residuals are autocorrelated
#p-value ≥ 0.05 → Do not reject null → Residuals not autocorrelated 
perform_ljungbox <- function(model, model_name) {
  # Estimate number of observations and parameters
  n <- nobs(model)
  param <- length(model$coefficients)
  
  # Start with rule-based lag
  q <- floor(0.75 * n^(1/3))
  # Ensure df > 0 and q >= param + 1
  if (q <= param) {q <- param + 1}
  # Set a minimum lag if needed (e.g., 5)
  q <- max(q, 5)
  df <- q - param
  
  lb_result <- LjungBox(model$residuals, lags = q, fitdf = param)
  cat("\nModel:", model_name, "\n")
  print(lb_result)
  
}
perform_ljungbox(AR1, "AR(1)") # 0.0044 Significant autocorrelation remains
perform_ljungbox(AR2, "AR(2)") # 0.0011 Significant autocorrelation remains
perform_ljungbox(AR3, "AR(3)") # 0.00028 Strong residual autocorrelation
perform_ljungbox(AR4, "AR(4)") # 0.2659 Residuals resemble white noise
perform_ljungbox(AR5, "AR(5)") # 0.0975 Residuals likely white noise

# Jarque-Bera test on wheter residuals are normally distributed
jarque.bera.test(AR1$residuals) # 0.01385 Residuals not normally distributed
jarque.bera.test(AR2$residuals) # 0.2152 Residuals appear normally distributed
jarque.bera.test(AR3$residuals) # 0.1477 Residuals appear normally distributed
jarque.bera.test(AR4$residuals) # 0.2747 Residuals appear normally distributed
jarque.bera.test(AR5$residuals) # 0.2141 Residuals appear normally distributed

#The best performing model appears to be AR(4) Passes both the Ljung-Box and Jarque-Bera tests, as well as being
# the best model according to AIC is AR(4)

