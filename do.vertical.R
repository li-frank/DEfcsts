####################################################################
####################################################################
#only load existing data: Source("load.R")
rm(list = ls())
memory.limit()

fcstType <- 'vertical'

#how many days out to predict? 
#should be >= 28 to minimize DoW effect in ARIMA 
testDays <- 120

#set training dates (regression & arima)
startYear <- 2010
startDoY <- 1 #for time series settings
trainStart <- as.Date(ISOdate(startYear,1,1))
#trainEnd <- as.Date(ISOdate(2015,5,24))
trainEnd <- as.Date(Sys.Date()-3)

#!test dates
testStart <- trainEnd + 1
testEnd <- testStart + testDays - 1
predict.dateRange <- seq(testStart,testEnd,by="day")

#start0 <- proc.time()
source("update.R")
source("load.R")
source(paste0(fcstType,"/filter.R"))
#pullTime <- proc.time()-start0; pullTime

#start <- proc.time()
source("variables.R")
#source("weather.R")
#source("regressions.R")
source("reg_lnGMV.R")
#source("plots.R")
source("arima.R")
source("forecast.R")