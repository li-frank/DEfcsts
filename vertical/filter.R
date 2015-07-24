#filter data
actual0.lim <- actual0[actual0$BIZ_FLAG == 'B2C',]

##gmv: by verticals
vertsGMV <- ddply(actual0.lim,
                  .(ckDate,VERTICAL),
                  summarise,gmv=sum(GMV_PLAN),
                  .progress = progress_text(char = "."))

vertsGMV.bk <- vertsGMV

unique(vertsGMV$VERTICAL)
slices <- c("H&G","Elec","P&A","Fashion")

#create individual datasets
vertList <- NULL
for (slice in slices){
  assign(make.names(paste0(slice,"_gmv")),vertsGMV[vertsGMV$VERTICAL==slice,c("ckDate","gmv")])
  vertList <- append(vertList,make.names(paste0(slice,"_gmv")))
}
print(vertList)

#######################################################
##gmv: all verticals
GMVagg <- ddply(actual0,
                .(ckDate),
                summarise,gmv=sum(GMV_PLAN),
                .progress = progress_text(char = "."))

GMVagg.bk <- GMVagg