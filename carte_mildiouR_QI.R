###############################################################################
###############################################################################
#Mapping the different resistance to Quinone Inhibitors in France
###############################################################################
###############################################################################

#loading the packages necessary for the analysis
library(rgdal)
library(plotrix)
library(classInt)

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

setwd("~/work/Rfichiers/Githuber/mildiou_mito_comp_data")


###############################################################################
#loading the resistance data
###############################################################################

rez_list<-read.table("R_mildiouQI.txt",header=TRUE,sep="\t")
Raox_list<-rez_list[!is.na(rez_list$AOX),]
Ramet_list<-rez_list[!is.na(rez_list$AMETOC),]
RametBM_list<-rez_list[!is.na(rez_list$S34L),]

#first we merge the resistance table with the commune info
Raox_list<-merge(Raox_list,db_commu,by.x="INSES_CODE",by.y="INSEE_COM")
#in order to acces to the arrondissement ID, we create an individual ID for 
#each arrondissement combining INSEE_DEP and INSEE_ARR
Raox_list$DEPARR<-paste(Raox_list$INSEE_DEP,Raox_list$INSEE_ARR)
#then we merge the resistance table with the arrondissement info
Raox_list<-merge(Raox_list,db_arrond,by.x="DEPARR",by.y="DEPARR")

#one example of a map with the arrondissement and commune sampled
op<-par(mar=c(0,0,0,0))
plot(departe,border="grey60",lwd=0.1)
plot(arrond[arrond$ID %in% Raox_list$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(commu[commu$INSEE_COM %in% as.character(Raox_list$INSES_CODE),],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)
par(op)

#a map with only arrondissement, the different colors show the different 
#status of the AOX type resistance for all years
op<-par(mar=c(0,0,0,0))
plot(departe,border="grey60",lwd=0.1)
plot(arrond[arrond$ID %in% Raox_list$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% Raox_list$ID.y[Raox_list$AOX>9],],
     add=TRUE,col="orange",lwd=0.1)
plot(arrond[arrond$ID %in% Raox_list$ID.y[Raox_list$AOX>99],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)
par(op)

#same map with only arrondissement, but one map for each year
op<-par(mar=c(0,0,0,0),mfrow=c(1,3))
#for 2015
temp<-Raox_list[Raox_list$year==2015,]
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>9],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2016
temp<-Raox_list[Raox_list$year==2016,]
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>9],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2017
temp<-Raox_list[Raox_list$year==2017,]
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>9],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

par(op)


commu[commu$INSEE_COM %in% c("43033","63453"),]

#the path to access to the barycentre of the commune
commu@polygons[1][1]@labpt




