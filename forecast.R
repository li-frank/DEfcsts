library('gtools')
#library('TTR')
library('scales')
#############################################################################
monthError <- NULL
monthErrors <- NULL
quarterError <- NULL
quarterErrors <- NULL

for (vertName in slices){
  vertRegName <- make.names(vertName)
  #pull arimaGMV numbers
  arimaGMV <- exp(get(paste0(vertRegName,".arimaGMV"))$mean)
  #   
  #   #pull vertical indepedent variables
  #   fullDF <- get(paste0(vertRegName,".fullDF"))
  #   
  #   ##subset to prediction period
  #   predDF <- fullDF[fullDF$date <= testEnd 
  #                    & fullDF$date >= testStart,]
  #   predDF$ln_gmv <- NULL
  
  #pull vertical regression model & predict GMV
  reg <- get(paste0(vertRegName,".reg"))
  vertFcst <- predict(reg, newdata = predVar)
  assign(paste0(vertRegName,".lnGMVpred"), vertFcst, env = .GlobalEnv)
  gmv.reg <- data.frame(predict.dateRange, exp(vertFcst))
  names(gmv.reg) <- c("date","gmv")
  
  #get actuals & block out prediction period to calculate GMV from %
  actuals <- actualsAll <- get(paste0(vertRegName,".gmv"))
  names(actuals) <- c("date","gmv")
  gmv.actual <- actuals[which(findInterval(actuals$date, predict.dateRange)>0),] #date subset
  assign(paste0(vertRegName,".actual"), gmv.actual[1:365,], env = .GlobalEnv)  
  #actuals$gmv[which(findInterval(actuals$date, predict.dateRange)>0)] <- NA
  
  ##create crossed forecast
  qSum.reg <- sum(gmv.reg$gmv[1:testDays])
  qSum.arima <- sum(arimaGMV[1:testDays])
  regArima <- gmv.reg$gmv[1:testDays]*qSum.arima/qSum.reg
  #create actuals + fcst in DF together for errors
  fcst <- data.frame(gmv.reg[1:testDays,], arimaGMV[1:testDays], regArima, gmv.actual$gmv[1:testDays])
  names(fcst) <- c("date", "gmv.reg", "gmv.arima", "regArima", "gmv.actual")
  fcst$reg.error <- fcst$gmv.reg/fcst$gmv.actual - 1
  fcst$arima.error <- fcst$gmv.arima/fcst$gmv.actual - 1
  fcst$regArima.error <- fcst$regArima/fcst$gmv.actual - 1
  assign(paste0(vertRegName,".fcst"), fcst, env = .GlobalEnv)
  write.csv(fcst,paste0("CSVexport/",trainEnd,"_",vertRegName,".csv"))
  
  #monthly errors
  monthError[1] <- sum(fcst$regArima[1:30])/sum(fcst$gmv.actual[1:30]) - 1
  monthError[2] <- sum(fcst$regArima[31:60])/sum(fcst$gmv.actual[31:60]) - 1
  monthError[3] <- sum(fcst$regArima[61:91])/sum(fcst$gmv.actual[61:91]) - 1
  monthErrors <- rbind(monthErrors,monthError)
  
  quarterError <- sum(fcst$regArima[1:91])/sum(fcst$gmv.actual[1:91]) - 1
  quarterErrors <- rbind(quarterErrors, quarterError)
}

actual.gmv <- data.frame(H.G.actual, Elec.actual, P.A.actual, Fashion.actual)
names(actual.gmv) <- c("H&G", "Elec", "P&A", "Fashion")
write.csv(actual.gmv, "CSVexport/actualGMV.csv")

errors <- cbind(Fashion.fcst$regArima.error, Elec.fcst$regArima.error, H.G.fcst$regArima.error, P.A.fcst$regArima.error)
avgErrors <- rowMeans(errors)
absAvgErrors <- rowMeans(abs(errors))

# print(paste0("Average Abs Daily Vert Error: ", absAvgError))
# print(paste0("Average Abs Daily Error: ", mean(absAvgError)))
print(monthErrors)
print(quarterErrors)