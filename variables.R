library('timeDate')
library('zoo')
# library('gtools')
# library('fBasics')
# library(leaps)
library(lubridate)
# library(quantmod)

# To do:
#   1. [done] Code carnival & labor day holiday and surrounding weeks
#   2. [done] Payday
#   3. Weather? Promotions? 2 weeks out? remaining non-holidays in Dec?
#   4. [done] check that holiday at fringes of years are coded (for predictions, label 2 years out)
#   5. add % of purchases from auctions (rolling)

#define date range
DFstart <- min(slicesGMV$ckDate)
DFend <- max(actual0$ckDate) + 2*365
DFdateRange <- data.frame(
  date=as.Date(
    seq(DFstart,DFend,by="day")))
DFdateRange.c <- c(
  as.Date(
    seq(DFstart,DFend,by="day")))
weeklyGrowth <- rep(1:ceiling(nrow(DFdateRange)/7), each=7, length.out=nrow(DFdateRange))

#define years
DFstartYear <- as.integer(format(DFstart,"%Y")) - 1
DFendYear <- as.integer(format(DFend,"%Y")) + 1
years <- c(DFstartYear:DFendYear); years

#*day of week (friday is intercept)
dow <- weekdays(DFdateRange.c)

#*week of year
woy.anchor <- DFstart
woys <- (as.integer(floor((DFdateRange.c-woy.anchor)/7)) %% 52) + 1
woy <- factor(woys)

#*holidays (Easter, Pentecost, DEAscension, DECorpusChristi, DEGermanUnity, DEChristmasEve, DENewYearsEve, ChristmasDay, NewYearsDay)
holidays <- c("Easter", "Pentecost", "DEAscension", 
              "DECorpusChristi", "DEGermanUnity", "DEChristmasEve", 
              "DENewYearsEve", "ChristmasDay", "NewYearsDay", "DECarnival", 
              "DELaborDay","BoxingDay","AllSaints","AllSaintsA","MothersDay","PentecostB", "Walpurgisnacht")
##create holiday matrix & create holiday 'DoW'
a <- NULL
holidates <- NULL
for (i in holidays){
  for (j in years){
    if (i=='DELaborDay') {
      date <- as.Date(paste0(j,"-05-01"))
      a <- data.frame(date,i)
      colnames(a) <- c("date","holiday"); head(holidates,10)
      #change dow to holiday; only do if within dow date range
      if(is.element(date,DFdateRange.c)){
        dowIndex <- date - DFstart + 1
        dow[dowIndex] <- "Holiday"
      }
    } else if (i=='DECarnival'){
      date <-  as.Date(holiday(j,"Easter"))-48
      a <- data.frame(date,i)
      colnames(a) <- c("date","holiday"); head(holidates,10)
      if(is.element(date,DFdateRange.c)){
        dowIndex <- date - DFstart + 1
        dow[dowIndex] <- "Holiday"
      }
    } else if (i=='Walpurgisnacht'){
      date <- as.Date(paste0(j,"-04-30"))
      a <- data.frame(date,i)
      colnames(a) <- c("date","holiday"); head(holidates,10)
      if(is.element(date,DFdateRange.c)){
        dowIndex <- date - DFstart + 1
        dow[dowIndex] <- "Holiday"
      }
    } else if (i=='AllSaintsA'){
      date <- as.Date(holiday(j,"AllSaints"))-1
      a <- data.frame(date,i)
      colnames(a) <- c("date","holiday"); head(holidates,10)
      if(is.element(date,DFdateRange.c)){
        dowIndex <- date - DFstart + 1
        #        dow[dowIndex] <- "Holiday"
      }
    } else if (i=='PentecostB'){
      date <- as.Date(holiday(j,"Pentecost"))+1
      a <- data.frame(date,i)
      colnames(a) <- c("date","holiday"); head(holidates,10)
      if(is.element(date,DFdateRange.c)){
        dowIndex <- date - DFstart + 1
        #        dow[dowIndex] <- "Holiday"
      }
    } else if (i=='MothersDay'){
      first = as.Date(paste0(j,"-05-01"))
      dow.MD = sapply(seq(0,6),function(x) wday(first+days(x)))
      firstSun = first + days(which(dow.MD==1)-1)
      date <- firstSun + 7
      a <- data.frame(date,i)
      colnames(a) <- c("date","holiday"); head(holidates,10)
      if(is.element(date,DFdateRange.c)){
        dowIndex <- date - DFstart + 1
        #        dow[dowIndex] <- "Holiday"
      }
    } else {
      date <- as.Date(holiday(j,i))
      a <- data.frame(date,i)   
      colnames(a) <- c("date","holiday"); head(holidates,10)
      if(is.element(date,DFdateRange.c)){
        dowIndex <- date - DFstart + 1
        dow[dowIndex] <- "Holiday"
      }
    }
    holidates <- rbind(holidates,a)
  }
}
colnames(holidates) <- c("date","holiday"); head(holidates,10)
holidates$year <- format(holidates$date, "%Y")
##CHECK IF HOLIDAYS MIGHT OVERLAP
##match holidays to date to create holiday variable
holiday.m <- merge(DFdateRange, holidates, by.y="date", all.x=TRUE)
h. <- factor(holiday.m$holiday, exclude=NULL); head(h.)
###make NA first so it's the intercept
h. <- factor(h., levels=rev(levels(h.)), exclude=NULL); head(h.)

#*7 days before and after holiday (14 days total)
d7_list <- NULL
d7vars <- data.frame(DFdateRange)
for (i in holidays){
  after_combo <- NULL
  before_combo <- NULL
  
  for (j in years){
    #Easter_2 is 2 days after Easter, x2_Easter is 2 days before easter
    ##days after
    after <- data.frame(paste0("x",seq(1:7)), 
                        as.Date(holidates[holidates$year==j & holidates$holiday==i,"date"])+seq(1:7))
    ##days before
    before <- data.frame(paste0(seq(1:7),"x"), 
                         as.Date(holidates[holidates$year==j & holidates$holiday==i,"date"])-seq(1:7))
    names(after) <- names(before) <- c("holiday","date")
    
    after_combo <- rbind(after_combo, after)
    before_combo <- rbind(before_combo, before)
    
    #   assign(paste0(i,"_7d"), rbind(paste0(i,"_7d"), after))
  }
  
  #assign(paste0(i,"_List"), after_combo)
  afterHol.cut <- merge(DFdateRange, after_combo, all.x=TRUE)
  a <- factor(afterHol.cut$holiday, exclude=NULL); head(a)
  a <- factor(a, levels=rev(levels(a)), exclude=NULL); head(a)
  assign(paste0(i,"_x7"), a,env = .GlobalEnv )
  
  #assign(paste0("x_",i,"List"), before_combo)
  beforeHol.cut <-  merge(DFdateRange, before_combo, all.x=TRUE)
  b <- factor(beforeHol.cut$holiday, exclude=NULL); head(b)
  b <- factor(b, levels=rev(levels(b)), exclude=NULL); head(b)
  assign(paste0(i,"_7x"), b,env = .GlobalEnv )
  
  d7_list <<- rbind(d7_list, paste0(i,"_7x"), paste0(i,"_x7"))
  d7vars <<- data.frame(d7vars, a, b)
  d7vars <<- rename(d7vars, c(a=paste0(i,"_x7"), b=paste0(i,"_7x")))
  d7vars$date <- NULL
}

################################14x variables
d14_list <- NULL
d14vars <- data.frame(DFdateRange)
for (i in holidays){
  before_combo <- NULL
  
  for (j in years){
    #Easter_2 is 2 days after Easter, x2_Easter is 2 days before easter
    ##days before
    before <- data.frame(paste0("14x"), 
                         as.Date(holidates[holidates$year==j & holidates$holiday==i,"date"])-seq(8,14))
    names(before) <- c("holiday","date")
    before_combo <- rbind(before_combo, before)
    
    #   assign(paste0(i,"_7d"), rbind(paste0(i,"_7d"), after))
  }
  
  #assign(paste0("x_",i,"List"), before_combo)
  beforeHol.cut <-  merge(DFdateRange, before_combo, all.x=TRUE)
  b <- factor(beforeHol.cut$holiday, exclude=NULL); head(b)
  b <- factor(b, levels=rev(levels(b)), exclude=NULL); head(b)
  assign(paste0(i,"_14x"), b,env = .GlobalEnv )
  
  d14_list <<- rbind(d14_list, paste0(i,"_14x"))
  d14vars <<- data.frame(d14vars, b)
  d14vars <<- rename(d14vars, c(b=paste0(i,"_14x")))
  d14vars$date <- NULL
}
################################21x variables
d21_list <- NULL
d21vars <- data.frame(DFdateRange)
for (i in holidays){
  before_combo <- NULL
  
  for (j in years){
    #Easter_2 is 2 days after Easter, x2_Easter is 2 days before easter
    ##days before
    before <- data.frame(paste0("21x"), 
                         as.Date(holidates[holidates$year==j & holidates$holiday==i,"date"])-seq(15,21))
    names(before) <- c("holiday","date")
    before_combo <- rbind(before_combo, before)
  }
  #assign(paste0("x_",i,"List"), before_combo)
  beforeHol.cut <-  merge(DFdateRange, before_combo, all.x=TRUE)
  b <- factor(beforeHol.cut$holiday, exclude=NULL); head(b)
  b <- factor(b, levels=rev(levels(b)), exclude=NULL); head(b)
  assign(paste0(i,"_21x"), b,env = .GlobalEnv )
  
  d21_list <<- rbind(d21_list, paste0(i,"_21x"))
  d21vars <<- data.frame(d21vars, b)
  d21vars <<- rename(d21vars, c(b=paste0(i,"_21x")))
  d21vars$date <- NULL
}
##########################################################################
#payday
##choose first date & last date; paydayA is 1st of month, paydayB is last of month
month1st <- seq(as.Date(timeFirstDayInMonth(DFstart)), as.Date(timeFirstDayInMonth(DFend)), by="month") 
month31st <- as.Date(alignMonthly(month1st, include.weekends=TRUE))
##subset so that dates are inside ranges
month1st <- subset(month1st, month1st >= DFstart)
month31st <- subset(month31st, month31st <= DFend)
month1st <- cbind(as.Date(month1st), "1st")
month31st <- cbind(as.Date(month31st), "31st")
colnames(month1st) <- c("date","marker")
paydates <- rbind(month1st, month31st)
paydays <- merge(DFdateRange, paydates, all.x=TRUE)
payday <- factor(paydays$marker, exclude=NULL)
payday <- factor(payday, levels=rev(levels(payday)), exclude=NULL); head(payday)

#week before and after payday
before_combo <- NULL
after_combo <- NULL
paydays$marker <- factor(paydays$marker, exclude=NULL)
for (i in paydays$date){
  marker <- paydays[paydays$date==i,]$marker
  if (marker == "1st") {
    ##days after
    after <- data.frame(paste0("x",seq(1:7)), 
                        as.Date(i+seq(1:7)))   
    names(after) <- c("payday","date")  
    after_combo <- rbind(after_combo, after)  
  }
  if (marker == "31st") {
    ##days before
    before <- data.frame(paste0(seq(1:7),"x"), 
                         as.Date(i-seq(1:7)))
    names(before) <- c("payday","date") 
    before_combo <- rbind(before_combo, before) 
  }
}
payday_x7 <- merge(DFdateRange, after_combo, all.x=TRUE)
a <- factor(payday_x7$payday, exclude=NULL); head(a)
a <- factor(a, levels=rev(levels(a)), exclude=NULL); head(a)
assign("payday_x7", a, env = .GlobalEnv)

payday_7x <- merge(DFdateRange, before_combo, all.x=TRUE)
b <- factor(payday_7x$payday, exclude=NULL); head(b)
b <- factor(b, levels=rev(levels(b)), exclude=NULL); head(b)
assign("payday_7x", b, env = .GlobalEnv )

# #EURUSD; getFX saves variable under 'EURUSD'
# getFX("EUR/USD",from=DFstart-90, to=min(DFend, DFstart+365*5-90))
# if(DFend-DFstart > 365*5){
#   fx1 <- EURUSD
#   getFX("EUR/USD",from=DFstart+365*5-89, to=min(DFend))
#   fx2 <- EURUSD
#   fx <- rbind(fx1,fx2)
# }
# 
# fx_7d <- (fx/SMA(fx,7) - 1)[-(1:90)]
# fx_14d <- (fx/SMA(fx,14) - 1)[-(1:90)]
# fx_28d <- (fx/SMA(fx,28) - 1)[-(1:90)]
# fx_56d <- (fx/SMA(fx,56) - 1)[-(1:90)]
# fx_84d <- (fx/SMA(fx,84) - 1)[-(1:90)]