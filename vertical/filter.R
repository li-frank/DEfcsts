#verticals split
#######################################
#filter data
actual0.lim <- actual0[actual0$BIZ_FLAG == 'B2C',]

##gmv: by verticals
slicesGMV <- ddply(actual0.lim,
                   .(ckDate,VERTICAL),
                   summarise,gmv=sum(GMV_PLAN),
                   .progress = progress_text(char = "."))

slicesGMV.bk <- slicesGMV

unique(slicesGMV$VERTICAL)
slices <- c("H&G","Elec","P&A","Fashion")

#create individual datasets
sliceList <- NULL
for (slice in slices){
  assign(make.names(paste0(slice,".gmv"))
         , slicesGMV[slicesGMV$VERTICAL==slice, c("ckDate","gmv")]
         , env = .GlobalEnv)
  sliceList <- append(sliceList,make.names(paste0(slice,"_gmv")))
}
print(sliceList)

#######################################################
##gmv: all verticals
GMVagg <- ddply(actual0,
                .(ckDate),
                summarise,gmv=sum(GMV_PLAN),
                .progress = progress_text(char = "."))

GMVagg.bk <- GMVagg