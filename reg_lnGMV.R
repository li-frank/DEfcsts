############################################################
varDF <- data.frame(DFdateRange, dow, woy, h., 
                    d7vars, d14vars, d21vars, payday, payday_x7, payday_7x
                    , weeklyGrowth
                    #                    , weather$index.14d, weather$sunTime, weather$avgRain
                    #                    , weather$index.30d, weather$index.90d, weather$index.180d, weather$index.364d 
                    #                    , weather$absIndex.30d, weather$absIndex.7dx30d, weather$absIndex.364d, weather$absIndex.7dx364d
                    #                    , weather$absDiff.7dx30d , weather$absDiff.7dx364d
)    

trainVar <- varDF[varDF$date >= trainStart
                  & varDF$date <= trainEnd,]
predVar <-  varDF[varDF$date >= testStart
                  & varDF$date <= testEnd,]

# 1. reg through verticals
regList <- NULL
coeffList <- NULL
fitList <- NULL
varRanks <- NULL

for (vertName in slices){
  #get usable vertName (no characters)
  vertRegName <- make.names(vertName)
  #filter for specific vertical
  vertGMV <- vertsGMV[vertsGMV$BIZ_FLAG==vertName, c("ckDate","gmv")]
  assign(paste0(vertRegName,".gmv"), vertGMV, env = .GlobalEnv)
  n <- nrow(vertGMV)
  
  #   #!is this needed? dependent variable log transform: 1 for all, 1 limited for regression
  #   ln_gmv <- data.frame(ln_gmv=log(vertGMV$gmv))
  #   assign(paste0(vertRegName,".lnGMV"), ln_gmv, env = .GlobalEnv)
  
  #limit vars and GMV for train dates
  train.lnGMV <- log(vertGMV[vertGMV$ckDate >= trainStart
                             & vertGMV$ckDate <=trainEnd,]$gmv)
  trainDF <- data.frame(trainVar, train.lnGMV)
  #save full DF to vertName
  assign(paste0(vertRegName,".trainDF"), trainDF, env = .GlobalEnv)
  
  #   #limit to regression training dates
  #   varGMV.reg <- varGMV[varGMV$date >= trainStart 
  #                        & varGMV$date <= trainEnd,]
  #   assign(paste0(vertRegName,".regDF"), varGMV.reg, env = .GlobalEnv)
  
  #*run full and base model regression for stepwise regression bounds
  fullModel <- lm(train.lnGMV ~ . - date, data=trainDF)
  assign(paste0(vertRegName,".fullReg"),fullModel)
  baseModel <- lm(train.lnGMV ~ dow + woy + h.
                  #                  + dailyGrowth
                  #                  + DEChristmasEve_7x + Easter_7x
                  , data=trainDF)
  assign(paste0(vertRegName,".baseReg"),baseModel)
  
  optimalModel <- step(baseModel, list(upper=fullModel, lower=baseModel), direction="forward", trace=FALSE
                       , k=log(n))                    
  assign(paste0(vertRegName,".reg"), optimalModel)
  assign(paste0(vertRegName,".varRanks"), optimalModel$anova)
  
  #create residuals table to figure out highest residuals
  
  #list of regressions
  regList <- append(regList,paste0(vertRegName,".reg"))
  
  #record coeffs
  coeffs0 <- summary(optimalModel)$coefficients[,1]
  coeffs <- data.frame(labels(coeffs0),coeffs0)
  names(coeffs) <- c("coeffName", paste0(make.names(vertName)))
  if (is.null(coeffList)){
    coeffList <- coeffs
  } else{
    #    names(coeffList) <- names(coeffs)
    coeffList <- merge(coeffList, coeffs, all=TRUE)
  }
  
  #record adjusted r-squared
  adjrsq <- round(unlist(summary(optimalModel)[9]),4)
  names(adjrsq) <- vertName
  fitList <- c(fitList,adjrsq)
  if (is.null(varRanks)){
    varRanks <- optimalModel$anova
  } else{
    varRanks <- merge(varRanks, optimalModel$anova, all=TRUE)
  }
  
  #record varRanks
  ########!
  
  #  predicted-residual chart
  #  predicts <- predict(optimalModel)
  #  resids <- resid(optimalModel)
  #  plot(predicts, resids)
  
  #time-residual chart
  resids <- resid(optimalModel)
  assign(paste0(vertRegName,".resid"), resids)
  
  #find outliers for date labels
  outliers <- which(abs(resids) > quantile(abs(resids),0.99))
  
  #resids.sma <- SMA(resids, 7)  
  fileLocation <- "C:/Users/frankli/Box Sync/DEvert/Plots/Resids/"
  fileName <- paste0(vertName,"Resids.pdf") 
  pdf(file=paste0(fileLocation,fileName), onefile=TRUE) 
  plot(trainDF$date,resids, 
       main=vertName, xlab="Date", ylab="Residuals")
  text(trainDF$date[outliers], resids[outliers], trainDF$date[outliers]
       , pos=2)
  dev.off()
  
}

print(fitList)