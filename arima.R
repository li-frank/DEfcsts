library('forecast')

for (slice in slices){
  #get usable slice (no characters)
  sliceRegName <- make.names(slice)
  
  ##**arima for GMV level**## !undo
  ln_gmv.train <- get(paste0(sliceRegName,".trainDF"))$train.lnGMV
  #gmv.train <- exp(varGMV.reg$ln_gmv)
  ts <- ts(ln_gmv.train, frequency=52*7, start=c(startYear, startDoY))
  gmv.arima <- auto.arima(ts, seasonal=TRUE, D=1, trace=TRUE)
  assign(paste0(sliceRegName,".arima"), gmv.arima, env = .GlobalEnv)  
  fcst.arima <- forecast(gmv.arima)
  assign(paste0(sliceRegName,".arimaGMV"), fcst.arima, env = .GlobalEnv)
  
  #save under test date
  assign(paste0(sliceRegName,".arimaGMV",testStart), fcst.arima, env = .GlobalEnv)
  
}

arima.gmv <- exp(data.frame(H.G.arimaGMV$mean, Elec.arimaGMV$mean, P.A.arimaGMV$mean, Fashion.arimaGMV$mean))
names(arima.gmv) <- c("H&G", "Elec", "P&A", "Fashion")

fileName <- paste0(fcstType,"/CSVexport/arimaGMV.csv")
write.csv(arima.gmv, fileName)