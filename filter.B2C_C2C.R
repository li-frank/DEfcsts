#filter data
#actual0.lim <- actual0[actual0$BIZ_FLAG == 'B2C',]

##gmv: by verticals
vertsGMV <- ddply(actual0,
                  .(ckDate,BIZ_FLAG),
                  summarise,gmv=sum(GMV_PLAN),
                  .progress = progress_text(char = "."))

vertsGMV.bk <- vertsGMV

unique(vertsGMV$BIZ_FLAG)
top2vert <- c("B2C","C2C")

#create individual datasets
vertList <- NULL
for (vertName in top2vert){
  assign(make.names(paste0(vertName,"_gmv")),vertsGMV[vertsGMV$BIZ_FLAG==vertName,c("ckDate","gmv")])
  vertList <- append(vertList,make.names(paste0(vertName,"_gmv")))
}
print(vertList)

#######################################################
##gmv: all verticals
GMVagg <- ddply(actual0,
                .(ckDate),
                summarise,gmv=sum(GMV_PLAN),
                .progress = progress_text(char = "."))

GMVagg.bk <- GMVagg