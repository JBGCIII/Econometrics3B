############################################################################################

# Econometrics 3b - Spring 2025
# Lab 2
# Fredrik Runelöv (fredrik.runelov@su.se)

############################################################################################

# As always we start by loading all the packages we need. Type e.g. install.packages("dynlm") 
# in the console below to install.

library(dynlm) # to estimate AR(p) by OLS
library(forecast) # to estimate and plot the ACF
library(portes) # to perform Ljung-Box test
library(tseries) # to perform Jarque-Bera test

# Let's load the Macrodat dataset without loading the full Ecdat package since we only need 
# the dataset.

data(Macrodat, package = "Ecdat")

# The model for gdpjp (= real GDP for Japan) we want to estimate is an AR(1) process in 
# first-differences.

formula1 <- d(gdpjp) ~ L(d(gdpjp)) # d and L are internal dynlm functions

# Use dynlm from the dynlm package to estimate with OLS.

model1 <- dynlm(formula1, Macrodat)

# model1 is a list containing all the results of the regression and we can use the summary() 
# function from the base package to summarize the results.

summary(model1)

# NB! You do not want to use the lm() function for regressions with differences and lags. You 
# can do it but it's more complicated.

############################################################################################

# Now let's plot the residuals. I also save the plot in a pdf since that is the format Latex 
# likes the best.

pdf("residuals.pdf") # "open" a pdf-file
plot(model1$residuals,
     main = "Model 1",
     ylab = "Residual", 
     xlab = "", 
     col = "blue"
     )
abline(h = 0,
       col = "red",
       lty = "dashed"
       )
dev.off() # "close" and save file

# Appears to be some heteroskedasticity and autocorrelation. The Acf() function from the 
# forecast package plots the sample autocorrelation function.

Acf(model1$residuals)

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