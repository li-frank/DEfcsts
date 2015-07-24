#B2C/C2C split
#######################################
#filter data
#actual0.lim <- actual0[actual0$BIZ_FLAG == 'B2C',]

##gmv: by verticals
slicesGMV <- ddply(actual0,
                  .(ckDate,BIZ_FLAG),
                  summarise,gmv=sum(GMV_PLAN),
                  .progress = progress_text(char = "."))

slicesGMV.bk <- slicesGMV

unique(slicesGMV$BIZ_FLAG)
slices <- c("B2C","C2C")

#create individual datasets
sliceList <- NULL
for (slice in slices){
  assign(make.names(paste0(slice,".gmv")),slicesGMV[slicesGMV$BIZ_FLAG==slice,c("ckDate","gmv")])
  sliceList <- append(sliceList,make.names(paste0(slice,".gmv")))
}
print(sliceList)

#######################################################
##gmv: all verticals
GMVagg <- ddply(actual0,
                .(ckDate),
                summarise,gmv=sum(GMV_PLAN),
                .progress = progress_text(char = "."))

GMVagg.bk <- GMVagg