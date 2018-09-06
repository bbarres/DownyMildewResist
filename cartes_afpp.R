###############################################################################
###############################################################################
#Mapping the different resistance for AFPP meeting
###############################################################################
###############################################################################

#loading the packages necessary for the analysis
library(rgdal)
library(plotrix)
library(classInt)
library(mapplots)

#Setting the right working directory
setwd("~/work/Rfichiers/Githuber/mildiou_mito_comp_data")


###############################################################################
#loading the geographical data
###############################################################################

#Setting the right working directory
setwd("~/work/Rfichiers/Githuber/carto_france")

#the geographical layer used here were downloaded on the IGN website (Geofla), 
#which is the French Institut of Geographic and forest information

#the smallest administrative unit in France: communes
commu<-readOGR(dsn="C:/Users/Benoit/Documents/Work/Rfichiers/Githuber/geo_data/ADE_1-1_SHP_LAMB93_FR",
               layer="COMMUNE")
class(commu)
slotNames(commu)
summary(commu@data)

#arrondissement are an intermediate
arrond<-readOGR(dsn="C:/Users/Benoit/Documents/Work/Rfichiers/Githuber/geo_data/ADE_1-1_SHP_LAMB93_FR",
                layer="ARRONDISSEMENT_DEPARTEMENTAL")

departe<-readOGR(dsn="C:/Users/Benoit/Documents/Work/Rfichiers/Githuber/geo_data/ADE_1-1_SHP_LAMB93_FR",
                 layer="DEPARTEMENT")

regions<-readOGR(dsn="C:/Users/Benoit/Documents/Work/Rfichiers/Githuber/geo_data/ADE_1-1_SHP_LAMB93_FR",
                 layer="REGION")

#isolate the information in the spatial data on the communes
db_commu<-commu@data
summary(db_commu)
db_arrond<-arrond@data
db_arrond$DEPARR<-paste(db_arrond$INSEE_DEP,db_arrond$INSEE_ARR)

#back to the data folder
setwd("~/work/Rfichiers/Githuber/mildiou_mito_comp_data")


###############################################################################
#loading the resistance data
###############################################################################

rez_list<-read.table("mildiou_afpp.txt",header=TRUE,sep="\t")
head(rez_list)
Raox_list<-rez_list[!is.na(rez_list$AOX) | 
                      (rez_list$AMISUL==1 & !is.na(rez_list$AMISUL)),]
Ramet_list<-rez_list[!is.na(rez_list$AMETOC),]
RametBM_list<-rez_list[!is.na(rez_list$S34L),]
Ramis_list<-rez_list[!is.na(rez_list$AMISUL),]
Rcyazo_list<-rez_list[!is.na(rez_list$CYAZO_SHAM),]
Rzoxa_list<-rez_list[!is.na(rez_list$Zoxamide),]
Rfluop_list<-rez_list[!is.na(rez_list$Fluopicolide),]


###############################################################################
#Ametoc Bio
###############################################################################

#in order to ease the count of different classes of resistance we turn the 
#variable of interest into a factor
Ramet_list$AMETOC<-as.factor(Ramet_list$AMETOC)

op<-par(mar=c(0,0,1,0),mfrow=c(1,3))

#for 2015
temp<-Ramet_list[Ramet_list$year==2015,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AMETOC,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AMETOC,temp$departement)[1,],
                "ResLost"=table(temp$AMETOC,temp$departement)[2,],
                "Res"=table(temp$AMETOC,temp$departement)[3,],
                "nb_fields"=colSums(table(temp$AMETOC,temp$departement)))
plot(departe,border="grey40",lwd=1,main="2015",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

#for 2016
temp<-Ramet_list[Ramet_list$year==2016,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AMETOC,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AMETOC,temp$departement)[1,],
                "ResLost"=table(temp$AMETOC,temp$departement)[2,],
                "Res"=table(temp$AMETOC,temp$departement)[3,],
                "nb_fields"=colSums(table(temp$AMETOC,temp$departement)))
plot(departe,border="grey40",lwd=1,main="2016",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

#for 2017
temp<-Ramet_list[Ramet_list$year==2017,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AMETOC,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AMETOC,temp$departement)[1,],
                "ResLost"=table(temp$AMETOC,temp$departement)[2,],
                "Res"=table(temp$AMETOC,temp$departement)[3,],
                "nb_fields"=colSums(table(temp$AMETOC,temp$departement)))
plot(departe,border="grey40",lwd=1,main="2017",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

par(op)


###############################################################################
#Ametoc BM (S34L)
###############################################################################

#same map with only arrondissement, but one map for each year
op<-par(mar=c(0,0,1,0),mfrow=c(1,3))

#for 2015
temp<-RametBM_list[RametBM_list$year==2015,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$S34L,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$S34L,temp$departement)[1,],
                "Res"=if(dim(table(temp$S34L,temp$departement))[1]==1)
                  rep(0,dim(table(temp$S34L,temp$departement))[2])
                else table(temp$S34L,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$S34L,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2016
temp<-RametBM_list[RametBM_list$year==2016,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$S34L,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$S34L,temp$departement)[1,],
                "Res"=if(dim(table(temp$S34L,temp$departement))[1]==1)
                  rep(0,dim(table(temp$S34L,temp$departement))[2])
                else table(temp$S34L,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$S34L,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2017
temp<-RametBM_list[RametBM_list$year==2017,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$S34L,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$S34L,temp$departement)[1,],
                "Res"=if(dim(table(temp$S34L,temp$departement))[1]==1)
                  rep(0,dim(table(temp$S34L,temp$departement))[2])
                else table(temp$S34L,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$S34L,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

par(op)


###############################################################################
#AOX maps BIO
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(1,3))

#for 2015
temp<-Raox_list[Raox_list$year==2015,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AOX,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AOX,temp$departement)[1,],
                "Res"=if(dim(table(temp$AOX,temp$departement))[1]==1)
                  rep(0,dim(table(temp$AOX,temp$departement))[2])
                else table(temp$AOX,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$AOX,temp$departement)))
plot(departe,border="grey40",lwd=1,main="2015",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","darkorchid1"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

#for 2016
temp<-Raox_list[Raox_list$year==2016,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AOX,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AOX,temp$departement)[1,],
                "Res"=if(dim(table(temp$AOX,temp$departement))[1]==1)
                  rep(0,dim(table(temp$AOX,temp$departement))[2])
                else table(temp$AOX,temp$departement)[2,],
                #this is a unique addition because there were only
                #specific resistant strains to amisulbrom in 2016
                "AmiResPos"=table(temp$AMISUL,temp$departement)[2,])
coorddep$nb_fields<-colSums(table(temp$AOX,temp$departement))+
  table(temp$AMISUL,temp$departement)[2,]
plot(departe,border="grey40",lwd=1,main="2016",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res,coorddep$AmiResPos),
         col=c("blue","darkorchid1","orange"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

#for 2017
temp<-Raox_list[Raox_list$year==2017,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AOX,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AOX,temp$departement)[1,],
                "Res"=if(dim(table(temp$AOX,temp$departement))[1]==1)
                  rep(0,dim(table(temp$AOX,temp$departement))[2])
                else table(temp$AOX,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$AOX,temp$departement)))
plot(departe,border="grey40",lwd=1,main="2017",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","darkorchid1"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

par(op)


###############################################################################
#AMISULBRON resistance map BIO
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(1,3))

#for 2015
temp<-Ramis_list[Ramis_list$year==2015,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AMISUL,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AMISUL,temp$departement)[1,],
                "Res"=if(dim(table(temp$AMISUL,temp$departement))[1]==1)
                  rep(0,dim(table(temp$AMISUL,temp$departement))[2])
                else table(temp$AMISUL,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$AMISUL,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","orange"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2016
temp<-Ramis_list[Ramis_list$year==2016,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AMISUL,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AMISUL,temp$departement)[1,],
                "Res"=if(dim(table(temp$AMISUL,temp$departement))[1]==1)
                  rep(0,dim(table(temp$AMISUL,temp$departement))[2])
                else table(temp$AMISUL,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$AMISUL,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","orange"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2017
temp<-Ramis_list[Ramis_list$year==2017,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$AMISUL,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$AMISUL,temp$departement)[1,],
                "Res"=if(dim(table(temp$AMISUL,temp$departement))[1]==1)
                  rep(0,dim(table(temp$AMISUL,temp$departement))[2])
                else table(temp$AMISUL,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$AMISUL,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","orange"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

par(op)


###############################################################################
#Cyazofamid biological resistance maps by departement
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(1,3))

#for 2015
temp<-Rcyazo_list[Rcyazo_list$year==2015,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$CYAZO_SHAM,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$CYAZO_SHAM,temp$departement)[1,],
                "Res"=if(dim(table(temp$CYAZO_SHAM,temp$departement))[1]==1)
                  rep(0,dim(table(temp$CYAZO_SHAM,temp$departement))[2])
                else table(temp$CYAZO_SHAM,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$CYAZO_SHAM,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","orange"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2016
temp<-Rcyazo_list[Rcyazo_list$year==2016,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$CYAZO_SHAM,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$CYAZO_SHAM,temp$departement)[1,],
                "Res"=if(dim(table(temp$CYAZO_SHAM,temp$departement))[1]==1)
                  rep(0,dim(table(temp$CYAZO_SHAM,temp$departement))[2])
                else table(temp$CYAZO_SHAM,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$CYAZO_SHAM,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(regions,add=TRUE,lwd=1.5)

#for 2017
temp<-Rcyazo_list[Rcyazo_list$year==2017,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$CYAZO_SHAM,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$CYAZO_SHAM,temp$departement)[1,],
                "Res"=if(dim(table(temp$CYAZO_SHAM,temp$departement))[1]==1)
                  rep(0,dim(table(temp$CYAZO_SHAM,temp$departement))[2])
                else table(temp$CYAZO_SHAM,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$CYAZO_SHAM,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

par(op)


###############################################################################
#Zoxamid biological resistance maps by departement
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(1,3))

#for 2015, no samples
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(regions,add=TRUE,lwd=1.5)

#for 2016
temp<-Rzoxa_list[Rzoxa_list$year==2016,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$Zoxamide,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$Zoxamide,temp$departement)[1,],
                "Res"=if(dim(table(temp$Zoxamide,temp$departement))[1]==1)
                  rep(0,dim(table(temp$Zoxamide,temp$departement))[2])
                else table(temp$Zoxamide,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$Zoxamide,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2017
temp<-Rzoxa_list[Rzoxa_list$year==2017,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$Zoxamide,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$Zoxamide,temp$departement)[1,],
                "Res"=if(dim(table(temp$Zoxamide,temp$departement))[1]==1)
                  rep(0,dim(table(temp$Zoxamide,temp$departement))[2])
                else table(temp$Zoxamide,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$Zoxamide,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

par(op)


###############################################################################
#Fluopicolid biological resistance maps by departement
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(1,3))

#for 2015, no samples
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(regions,add=TRUE,lwd=1.5)

#for 2016
temp<-Rfluop_list[Rfluop_list$year==2016,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$Fluopicolide,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$Fluopicolide,temp$departement)[1,],
                "Res"=if(dim(table(temp$Fluopicolide,temp$departement))[1]==1)
                  rep(0,dim(table(temp$Fluopicolide,temp$departement))[2])
                else table(temp$Fluopicolide,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$Fluopicolide,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2017
temp<-Rfluop_list[Rfluop_list$year==2017,]
#map summarizing the resistant and not resistant strains by department
#first we list the indices of the sampled department
ind_list<-which(departe@data$INSEE_DEP %in% 
                  colnames(table(temp$Fluopicolide,temp$departement)))
#building the table of barycentre coordinates of the list of department
coorddep<-data.frame("longitude"=departe@polygons[ind_list[1]][[1]]@labpt[1],
                     "latitude"=departe@polygons[ind_list[1]][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  c("longitude"=departe@polygons[ind_list[i]][[1]]@labpt[1],
                    "latitude"=departe@polygons[ind_list[i]][[1]]@labpt[2]))
}
coorddep<-cbind(coorddep,"nonR"=table(temp$Fluopicolide,temp$departement)[1,],
                "Res"=if(dim(table(temp$Fluopicolide,temp$departement))[1]==1)
                  rep(0,dim(table(temp$Fluopicolide,temp$departement))[2])
                else table(temp$Fluopicolide,temp$departement)[2,],
                "nb_fields"=colSums(table(temp$Fluopicolide,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

par(op)


