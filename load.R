library('ebaytd')
library(plyr)
################################################################################
# past to load data (don't use this, revised veresion conn.R)
c <- teradataConnect(system="mozart")

#pull actuals
actualTable <- 'p_csi_tbs_t.fl_DEvert_actuals'
actualQuery <- "select * from :table"
actualPull <- gsub(':table',actualTable,actualQuery); actualPull

#load data
actual0 <- dbGetQuery(c,actualPull)
save(actual0,file="actual0.RData")
dbDisconnect(c)
#################################################################################

# load("actual0.RData")
#date formatting 
actual0$ckDate <- as.Date(actual0$ckDate)

actual0.bk <- actual0