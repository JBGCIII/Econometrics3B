

#Package installed using console
library(Ecdat)
library(dynlm) # to estimate AR(p) by OLS
library(forecast) # to estimate and plot the ACF
library(portes) # to perform Ljung-Box test
library(tseries) # to perform Jarque-Bera test




data("IncomeUK")
View(IncomeUK)


income_ts <- IncomeUK[, "income"]


data("IncomeUK")

str(IncomeUK)


head(IncomeUK)


income <- IncomeUK$income


