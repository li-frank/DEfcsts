library('forecast')

for (vertName in slices){
  #get usable vertName (no characters)
  vertRegName <- make.names(vertName)
  
  ##**arima for GMV level**## !undo
  ln_gmv.train <- get(paste0(vertRegName,".trainDF"))$train.lnGMV
  #gmv.train <- exp(varGMV.reg$ln_gmv)
  ts <- ts(ln_gmv.train, frequency=52*7, start=c(startYear, startDoY))
  gmv.arima <- auto.arima(ts, seasonal=TRUE, D=1, trace=TRUE)
  assign(paste0(vertRegName,".arima"), gmv.arima, env = .GlobalEnv)  
  fcst.arima <- forecast(gmv.arima)
  assign(paste0(vertRegName,".arimaGMV"), fcst.arima, env = .GlobalEnv)
  
  #save under test date
  assign(paste0(vertRegName,".arimaGMV",testStart), fcst.arima, env = .GlobalEnv)
}

arima.gmv <- exp(data.frame(H.G.arimaGMV$mean, Elec.arimaGMV$mean, P.A.arimaGMV$mean, Fashion.arimaGMV$mean))
names(arima.gmv) <- c("H&G", "Elec", "P&A", "Fashion")
write.csv(arima.gmv, "CSVexport/arimaGMV.csv")