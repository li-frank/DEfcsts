#pull existing table for base
source("load.R")

c <- teradataConnect(system="mozart")
head(actual0)

#check
actual0$ckDate <- as.Date(actual0$ckDate)
minDate <- as.Date(min(actual0$ckDate)); minDate
maxDate <- as.Date(max(actual0$ckDate)); maxDate
days <- as.numeric(maxDate - minDate + 1); days

#remove, reload
daysReload <- 14
reloadStart <- maxDate-daysReload; reloadStart
reloadStart <- paste0("'", reloadStart ,"'"); reloadStart
reloadEnd <- Sys.Date()-2; reloadEnd
reloadEnd <- paste0("'", reloadEnd ,"'"); reloadEnd

##remove
removePath <- 'SQL/removeDates.sql'
removeQuery <- paste(readLines(removePath), collapse=" ")
removeQuery <- gsub(':start_dt',reloadStart,removeQuery); removeQuery
removeQuery <- gsub(':table',actualTable,removeQuery); removeQuery
removed <- dbSendQuery(c,removeQuery)

##reload
reloadPath <- 'SQL/reload.sql'
reloadQuery <- paste(readLines(reloadPath), collapse=" ")
reloadQuery <- gsub(':start_dt',reloadStart,reloadQuery)
reloadQuery <- gsub(':end_dt',reloadEnd,reloadQuery); reloadQuery
reloadQuery <- gsub(':table',actualTable,reloadQuery); reloadQuery
reload <- dbSendQuery(c,reloadQuery)

dbDisconnect(c)