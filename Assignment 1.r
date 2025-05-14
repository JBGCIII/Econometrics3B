
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

# Function to perform Ljung-Box and Jarque-Bera tests
perform_tests <- function(model, model_name) {
  residuals <- model$residuals
  n <- length(residuals)
  q <- floor(0.75 * n^(1/3))  # Rule of thumb for choosing lags
  
  # Number of parameters estimated in the model
  param <- length(model$coefficients)
  
  # If q is less than or equal to the number of parameters, adjust q
  if (q <= param) {
    q <- q + param
  }
  
  # Ljung-Box Test
  LjungBox_test <- LjungBox(residuals, lags = q, fitdf = param)
  
  # Check if LjungBox_test is a list and contains a p-value
  if (is.list(LjungBox_test) && !is.null(LjungBox_test$p.value)) {
    LjungBox_pvalue <- LjungBox_test$p.value
  } else {
    # If not, extract the p-value from the correct index of the Ljung-Box result
    LjungBox_pvalue <- LjungBox_test[["p.value"]]
  }
  
  # Jarque-Bera Test
  jarque_bera_result <- jarque.bera.test(residuals)
  
  # Return a list of results
  return(list(
    model_name = model_name,
    LjungBox_pvalue = LjungBox_pvalue,
    JarqueBera_pvalue = jarque_bera_result$p.value,
    AIC_value = AIC(model)
  ))
}

# Perform tests for all models
results <- list()
results[[1]] <- perform_tests(AR1, "AR(1)")
results[[2]] <- perform_tests(AR2, "AR(2)")
results[[3]] <- perform_tests(AR3, "AR(3)")
results[[4]] <- perform_tests(AR4, "AR(4)")
results[[5]] <- perform_tests(AR5, "AR(5)")

# Convert results into a data frame for easier comparison
results_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(Model = x$model_name, 
             LjungBox_pvalue = x$LjungBox_pvalue, 
             JarqueBera_pvalue = x$JarqueBera_pvalue, 
             AIC_value = x$AIC_value)
}))

# Print the results
cat("\nResults of Ljung-Box and Jarque-Bera Tests along with AIC Values:\n")
print(results_df)