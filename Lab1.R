############################################################################################

# Econometrics 3b - Spring 2025
# Lab 1
# Fredrik Runelöv (fredrik.runelov@su.se)

############################################################################################

# Set the working directory so R knows where to look for files and save output.

setwd("C:\Users\fhsru\Desktop\Econometrics 3b (2025)\Labs\Lab 1")

# You never want setwd("") in your code. Best practice is to create a project which I'll 
# show you how to do.

############################################################################################

# The first thing I'm going to do is install the 'Ecdat' package which contains a number of 
# datasets. Packages provide R with essentially all it's functionality.

install.packages("Ecdat")

# NB! You never want install.packages() in your code and should type it into the console 
# instead. After installing a package you need to load it. The first code in your script 
# should be loading the packages that the rest of your code needs.

library(Ecdat)

# You can type data() to see which datasets are available via packages and then type e.g. 
# data(Macrodat) to load one. NB! These datasets are easy to access but rarely up to date 
# and mostly used as teaching examples.

data(Macrodat)

# Macrodat is a multivariate time series object so we can plot it with the plot() function.
# Everything you work with in R is an object of some type and how you can interact with an
# object depends on it's type.

plot(Macrodat)

# What do all these variables even mean? Need to check the documentation for Ecdat. When 
# you work with R you spend a lot of time looking at documentation for different packages.

############################################################################################

# Now I'm going to load a dataset from the excel file swedish_gdp.xlsx that I downloaded from 
# SCB. The data is seasonally adjusted quarterly Swedish GDP from 1993:Q1 to 2024:Q4 in 
# millions of SEK. To read excel files we need the 'readxl' package.

install.packages("readxl")
library(readxl)

# The read_excel() function saves the specified cells as a data frame. The first entry of 
# each column is treated as the name of the column unless you include col_names = FALSE.
# x <- y means create object x defined as y.

GDP <- read_excel("swedish_gdp.xlsx", range = "B52:B179", col_names = FALSE)

# The ts() function turns an object into a time series object starting at some date with 
# some frequency. The code below takes the data frame GDP and turns it into a time series 
# starting at 1993 with a frequency of 4. I end with /1000 so that the data is in billions
# rather than millions of SEK. NB! Everything I'm doing here requires knowledge about the 
# raw data.

GDP <- ts(GDP, start = 1993, frequency = 4)/1000

# The plot() function is an example of a command that would not work (well) if we didn't 
# first convert the data into a time series object.

plot(GDP, 
     main = "Swedish Quarterly GDP",
     ylab = "SEK billions", 
     xlab = "", 
     lwd = 2)

############################################################################################

# There is a clear upward trend in GDP so let's try a few ways to detrend it.
# Simplest way is to use the diff() function with differences = 1 to take first differences.

GDP_delta <- diff(GDP, differences = 1)

# Note that we lost the first observation in our time series (1993:Q1). Plot looks 
# stationary. The number of differences required to obtain a stationary series is called the 
# order of integration of the series.

plot(GDP_delta,
     main = "Change in Swedish Quarterly GDP",
     ylab = "SEK billions", 
     xlab = "",
     lwd = 2)

############################################################################################

# Another simple way to detrend is to compute the % change since last year (i.e. four 
# quarters ago). The 'tfplot' package includes the annualizedGrowth() function.

install.packages("tfplot")
library(tfplot)

# annualizedGrowth() with lag = frequency of your series gives you the annual growth rate.

GDP_growth <- annualizedGrowth(GDP, lag = 4)

# Note that we now lost the first four observations in our time series (1993:Q1-Q4). Also 
# looks stationary.

plot(GDP_growth,
     main = "% change in Swedish Quarterly GDP since last year",
     ylab = "% change", 
     xlab = "",
     lwd = 2)

############################################################################################

# The 'mFilter' package includes the hpfilter() function which applies the HP filter to a 
# time series object. Remember that there is some debate about the HP filter and that there
# is no universally "correct" detrending procedure.

install.packages("mFilter")
library(mFilter)

# Note that hpfilter() sets the smoothness parameter lambda to 1600 by default (the most 
# common choice for quarterly data). To choose a different lambda we would specify 
# freq = lambda, e.g.  hpfilter(GDP, freq = 800). NB! Many functions such as hpfilter() 
# has default options that you need to be aware of, i.e. know what your functions do.

GDP_hp <- hpfilter(GDP)

# GDP_hp is a list of 10 objects. In the next plot I need to specify that it is the object 
# cycle (a time series) in the list GDP_hp that I want to plot.

plot(GDP_hp$cycle, 
     main = "Swedish Quarterly GDP (HP cycle w/ lambda = 1600)",
     ylab = "SEK billions", 
     xlab = "", 
     lwd = 2)

############################################################################################

# The final thing I want to mention is that you can create your own functions in R.
# The function below simply takes a number and squares it.

square <- function(x){
  x <- x^2
  return(x)
}