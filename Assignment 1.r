
#I've Installed the required package using the console not the code
# as instructed in Lab 1.R

# Load required libraries
library(Ecdat)
library(dynlm)      # to estimate AR(p) by OLS
library(forecast)    # for ACF
library(portes)      # for Ljung-Box test
library(tseries)     # for Jarque-Bera test

# Load the dataset and inspect the structure
data("IncomeUK")
View(IncomeUK)

# Extract the 'income' variable as a time series
income_ts <- IncomeUK[, "income"]

# First difference of income (change in income)
d_income <- diff(income_ts)

# Use embed function to create a matrix with lagged values
# dimension = 1 + 5 for AR(5) (1 for the dependent variable and 5 for the lags)
d_income_lags <- embed(d_income, dimension = 1 + 5)

# Convert to time series format with correct start and frequency
d_income_lags_ts <- ts(d_income_lags, start = c(1971, 2), frequency = 4)

# Estimate AR models using dynlm()

# AR(1) model (only lag 1)
AR1 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2])

# AR(2) model (lags 1 and 2)
AR2 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:3])

# AR(3) model (lags 1, 2, and 3)
AR3 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:4])

# AR(4) model (lags 1, 2, 3, and 4)
AR4 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:5])

# AR(5) model (lags 1, 2, 3, 4, and 5)
AR5 <- dynlm(d_income_lags_ts[, 1] ~ d_income_lags_ts[, 2:6])

# Summarize results for each AR model
summary(AR1)
summary(AR2)
summary(AR3)
summary(AR4)
summary(AR5)

############################################################################################

# Appears to be some correlation so let's do a Ljung-Box test. The null hypothesis of the 
# Ljung-Box test is that the population autocovariance function = 0 for lags h = 1,2,...,q but
# what is an appropriate q? q = 0.75*n^(1/3) rounded down to the nearest integer is a common 
# rule of thumb. We also need to specify the number of estimated parameters. The function 
# LjungBox from the portes package can be used to perform the Ljung-Box test. However it might
# be the case that the q obtained by the rule of thumb is less than or equal to the number of 
# estimated parameters and then we need to choose q = 0.75*n^(1/3) + the number of estimated 
# parameters. For example below we obtain q = 4 and have 2 estimated parameters but suppose we
# instead estimated an AR(3) model, then we would have had 4 estimated parameters and should 
# have chosen q = 4 + 4 instead.

q <- floor(0.75*nobs(model1)^(1/3)) # the floor function rounds down to an integer
param = length(model1$coefficients)
LjungBox(model1$residuals, lags = q, fitdf = param)

############################################################################################

# As a bonus we perform a Jarque-Bera test. The null hypothesis of the Jarque-Bera test is that
# the true distribution of the data is a normal distribution. The function jarque.bera.test() 
# from the tseries package can be used to perform a Jarque-Bera test.

hist(model1$residuals, # first take a look at the histogram
     probability = TRUE,
     main = "Model 1 Residuals",
     xlab ="")

jarque.bera.test(model1$residuals)

############################################################################################

# We now estimate an AR(1), AR(2), AR(3) and AR(4) process for the change in gdpjp. To make 
# sure that each model is estimated using the same number of observations we use the embed 
# command with dimension = 1 + 4.

d_gdpjp <- diff(Macrodat[,"gdpjp"])
d_gdpjp1 <- embed(d_gdpjp, dimension = 1 + 4)

# The embed command turns d_gdpjp from a time series to a matrix. To turn it back into a time 
# series we use the ts() function.

d_gdpjp1 <- ts(d_gdpjp1, start = c(1960,2), frequency = 4)

# we can now use the dynlm() function to estimate each AR() model by OLS. The original series 
# is regressed on the series "to the right".

AR1 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:2])
AR2 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:3])
AR3 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:4])
AR4 <- dynlm(d_gdpjp1[,1] ~ d_gdpjp1[,2:5])

# Summaries

summary(AR1)
summary(AR2)
summary(AR3)
summary(AR4)

# Note that AR4 is equivalent to:

formula2 <- d(gdpjp) ~ L(d(gdpjp)) + L(d(gdpjp),2) + L(d(gdpjp),3) + L(d(gdpjp),4)
model2 <- dynlm(formula2, Macrodat)
summary(model2)