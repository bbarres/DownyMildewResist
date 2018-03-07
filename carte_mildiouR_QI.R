###############################################################################
###############################################################################
#Mapping the different resistance to Quinone Inhibitors in France
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


#select a commune using the INSES code
commu[commu$INSEE_COM %in% c("43033","63453"),]
#the path to access to the barycentre of the commune
commu@polygons[1][[1]]@labpt

#coordinates of the barycentre of the departement
departe@polygons[1][[1]]@labpt


###############################################################################
#loading the resistance data
###############################################################################

rez_list<-read.table("R_mildiouQI.txt",header=TRUE,sep="\t")
Raox_list<-rez_list[!is.na(rez_list$AOX),]
Ramet_list<-rez_list[!is.na(rez_list$AMETOC),]
RametBM_list<-rez_list[!is.na(rez_list$S34L),]
Ramis_list<-rez_list[!is.na(rez_list$AMISUL),]

###############################################################################
#AOX maps by arrondissements
###############################################################################

#first we merge the resistance table with the commune info
Raox_list<-merge(Raox_list,db_commu,by.x="INSES_CODE",by.y="INSEE_COM")
#in order to acces to the arrondissement ID, we create an individual ID for 
#each arrondissement combining INSEE_DEP and INSEE_ARR
Raox_list$DEPARR<-paste(Raox_list$INSEE_DEP,Raox_list$INSEE_ARR)
#then we merge the resistance table with the arrondissement info
Raox_list<-merge(Raox_list,db_arrond,by.x="DEPARR",by.y="DEPARR")

#one example of a map with the arrondissement and commune sampled
op<-par(mar=c(0,0,1,0))
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
plot(arrond[arrond$ID %in% Raox_list$ID.y[Raox_list$AOX>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)
par(op)

#same map with only arrondissement, but one map for each year
op<-par(mar=c(0,0,1,0),mfrow=c(2,3))

#for 2012
temp<-Raox_list[Raox_list$year==2012,]
plot(departe,border="grey60",lwd=0.1,main="2012")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2013
temp<-Raox_list[Raox_list$year==2013,]
plot(departe,border="grey60",lwd=0.1,main="2013")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2014
temp<-Raox_list[Raox_list$year==2014,]
plot(departe,border="grey60",lwd=0.1,main="2014")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2015
temp<-Raox_list[Raox_list$year==2015,]
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2016
temp<-Raox_list[Raox_list$year==2016,]
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2017
temp<-Raox_list[Raox_list$year==2017,]
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AOX>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

par(op)

#export pdf 10 x 6 inches


###############################################################################
#AOX maps by departement
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(2,3))

#for 2012
temp<-Raox_list[Raox_list$year==2012,]
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
plot(departe,border="grey60",lwd=0.1,main="2012")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2013
temp<-Raox_list[Raox_list$year==2013,]
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
plot(departe,border="grey60",lwd=0.1,main="2013")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2014
temp<-Raox_list[Raox_list$year==2014,]
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
plot(departe,border="grey60",lwd=0.1,main="2014")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

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
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

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
                "nb_fields"=colSums(table(temp$AOX,temp$departement)))
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

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
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

par(op)

#export pdf 10 x 6 inches


###############################################################################
#S34L maps
###############################################################################

#first we merge the resistance table with the commune info
RametBM_list<-merge(RametBM_list,db_commu,by.x="INSES_CODE",by.y="INSEE_COM")
#in order to acces to the arrondissement ID, we create an individual ID for 
#each arrondissement combining INSEE_DEP and INSEE_ARR
RametBM_list$DEPARR<-paste(RametBM_list$INSEE_DEP,RametBM_list$INSEE_ARR)
#then we merge the resistance table with the arrondissement info
RametBM_list<-merge(RametBM_list,db_arrond,by.x="DEPARR",by.y="DEPARR")

#a map with only arrondissement, the different colors show the different 
#status of the S34L genotype for all years
op<-par(mar=c(0,0,1,0))
plot(departe,border="grey60",lwd=0.1)
plot(arrond[arrond$ID %in% RametBM_list$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% RametBM_list$ID.y[RametBM_list$S34L==1],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)
par(op)

#same map with only arrondissement, but one map for each year
op<-par(mar=c(0,0,1,0),mfrow=c(1,3))
#for 2015
temp<-RametBM_list[RametBM_list$year==2015,]
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$S34L==1],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2016
temp<-RametBM_list[RametBM_list$year==2016,]
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$S34L==1],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2017
temp<-RametBM_list[RametBM_list$year==2017,]
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$S34L==1],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

par(op)

#export pdf 10 x 3 inches


###############################################################################
#S34L maps by departement
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

#export pdf 10 x 3 inches


###############################################################################
#Ametoc resistance maps
###############################################################################

#first we merge the resistance table with the commune info
Ramet_list<-merge(Ramet_list,db_commu,by.x="INSES_CODE",by.y="INSEE_COM")
#in order to acces to the arrondissement ID, we create an individual ID for 
#each arrondissement combining INSEE_DEP and INSEE_ARR
Ramet_list$DEPARR<-paste(Ramet_list$INSEE_DEP,Ramet_list$INSEE_ARR)
#then we merge the resistance table with the arrondissement info
Ramet_list<-merge(Ramet_list,db_arrond,by.x="DEPARR",by.y="DEPARR")

#a map with only arrondissement, the different colors show the different 
#status of the Ametoctradine target site resistance for all years
op<-par(mar=c(0,0,0,0))
plot(departe,border="grey60",lwd=0.1)
plot(arrond[arrond$ID %in% Ramet_list$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% Ramet_list$ID.y[Ramet_list$AMETOC>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)
par(op)

#same map with only arrondissement, but one map for each year
op<-par(mar=c(0,0,1,0),mfrow=c(2,2))
#for 2014
temp<-Ramet_list[Ramet_list$year==2014,]
plot(departe,border="grey60",lwd=0.1,main="2014")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMETOC>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2015
temp<-Ramet_list[Ramet_list$year==2015,]
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMETOC>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2016
temp<-Ramet_list[Ramet_list$year==2016,]
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMETOC>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2017
temp<-Ramet_list[Ramet_list$year==2017,]
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMETOC>0],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

par(op)


###############################################################################
#Biological resistant to ametoc maps by departement
###############################################################################

#in order to ease the count of different classes of resistance we turn the 
#variable of interest into a factor
Ramet_list$AMETOC<-as.factor(Ramet_list$AMETOC)

op<-par(mar=c(0,0,1,0),mfrow=c(2,2))

#for 2014
temp<-Ramet_list[Ramet_list$year==2014,]
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
plot(departe,border="grey60",lwd=0.1,main="2014")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

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
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

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
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

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
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

par(op)

#export pdf 6.66 x 6 inches


###############################################################################
#Biological and BM resistant to ametoc maps by departement
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(2,4))

#for 2014
temp<-Ramet_list[Ramet_list$year==2014,]
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
plot(departe,border="grey40",lwd=0.3,main="2014")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

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
plot(departe,border="grey40",lwd=0.3,main="2015")
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
plot(departe,border="grey40",lwd=0.3,main="2016")
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
plot(departe,border="grey40",lwd=0.3,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$ResLost,coorddep$Res),
         col=c("blue","orange","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)


#for 2014 BM
plot(departe,border="white",lwd=0.1,main="")
draw.pie(x=c(490000,490000,490000,490000,490000),
         y=c(6400000,6505000,6590000,6656500,6708000),
         z=cbind(c(0,0,0,0,0),c(2,2,2,2,2)),
         radius=sqrt(c(9,6,4,2,1))*18000,col="grey70",border=FALSE)
text(x=c(555000,555000,555000,555000,555000),
     y=c(6400000,6505000,6590000,6656500,6708000),
     labels=c("9 fields","6 fields","4 fields","2 fields","1 field"),
     cex=1,adj=c(0,0.5))#pos=4)
text(x=550000,y=6800000,labels="Legend",cex=1.5,font=2)

#for 2015 BM
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
plot(departe,border="grey40",lwd=0.3,main="2015")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

#for 2016 BM
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
plot(departe,border="grey40",lwd=0.3,main="2016")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

#for 2017 BM
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
plot(departe,border="grey40",lwd=0.3,main="2017")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

par(op)

#export pdf 13.33 x 6 inches


###############################################################################
#Amisulbrom maps by arrondissements
###############################################################################

#first we merge the resistance table with the commune info
Ramis_list<-merge(Ramis_list,db_commu,by.x="INSES_CODE",by.y="INSEE_COM")
#in order to acces to the arrondissement ID, we create an individual ID for 
#each arrondissement combining INSEE_DEP and INSEE_ARR
Ramis_list$DEPARR<-paste(Ramis_list$INSEE_DEP,Ramis_list$INSEE_ARR)
#then we merge the resistance table with the arrondissement info
Ramis_list<-merge(Ramis_list,db_arrond,by.x="DEPARR",by.y="DEPARR")


#a map with only arrondissement, the different colors show the different 
#status of the AMISUL type resistance for all years
op<-par(mar=c(0,0,1,0))
plot(departe,border="grey60",lwd=0.1)
plot(arrond[arrond$ID %in% Ramis_list$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% Ramis_list$ID.y[Ramis_list$AMISUL>0],],
     add=TRUE,col="orange",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)
par(op)

#same map with only arrondissement, but one map for each year
op<-par(mar=c(0,0,1,0),mfrow=c(2,3))

#for 2012
temp<-Ramis_list[Ramis_list$year==2012,]
plot(departe,border="grey60",lwd=0.1,main="2012")
plot(regions,add=TRUE,lwd=1.5)

#for 2013
temp<-Ramis_list[Ramis_list$year==2013,]
plot(departe,border="grey60",lwd=0.1,main="2013")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMISUL>0],],
     add=TRUE,col="orange",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2014
temp<-Ramis_list[Ramis_list$year==2014,]
plot(departe,border="grey60",lwd=0.1,main="2014")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMISUL>0],],
     add=TRUE,col="orange",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2015
temp<-Ramis_list[Ramis_list$year==2015,]
plot(departe,border="grey60",lwd=0.1,main="2015")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMISUL>0],],
     add=TRUE,col="orange",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2016
temp<-Ramis_list[Ramis_list$year==2016,]
plot(departe,border="grey60",lwd=0.1,main="2016")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMISUL>0],],
     add=TRUE,col="orange",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

#for 2017
temp<-Ramis_list[Ramis_list$year==2017,]
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$AMISUL>0],],
     add=TRUE,col="orange",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)

par(op)

#export pdf 10 x 6 inches


###############################################################################
#AMISULBRON biological resistance maps by departement
###############################################################################

op<-par(mar=c(0,0,1,0),mfrow=c(2,3))

#for 2012
plot(departe,border="grey60",lwd=0.1,main="2012")
plot(regions,add=TRUE,lwd=1.5)

#for 2013
temp<-Ramis_list[Ramis_list$year==2013,]
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
plot(departe,border="grey60",lwd=0.1,main="2013")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","orange"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

#for 2014
temp<-Ramis_list[Ramis_list$year==2014,]
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
plot(departe,border="grey60",lwd=0.1,main="2014")
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","orange"),
         radius=(sqrt(coorddep$nb_fields)*15000),labels=NA)

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

#export pdf 10 x 6 inches


###############################################################################
#Plotting pesticide sells at the departement level
###############################################################################

pesti2016<-read.table("pesticide2016.txt",header=TRUE,sep="\t",quote="")
pestameto<-pesti2016[pesti2016$substance=="ametoctradine",]
pestamisu<-pesti2016[pesti2016$substance=="amisulbrom",]
pestcyazo<-pesti2016[pesti2016$substance=="cyazofamid",]
pestfluop<-pesti2016[pesti2016$substance=="fluopicolide",]

#map for ametoctradine sells in 2016
temp<- merge(departe,pestameto,by.x="NOM_DEP", by.y="departement")
col<-findColours(classIntervals(temp$quantity, 8, style="quantile"),
                   smoothColors("tan",6,"tan4"))
#departement with 0 sells were lacking in the dataset so are considered
#as without any recorded sell
col[is.na(temp$quantity)]<-"white"
#legend
leg<-findColours(classIntervals(round(temp$quantity/1000,1),8,
                                style="quantile"),smoothColors("tan",6,"tan4"),
                   under="moins de", over="plus de",between="–",cutlabels=FALSE)

plot(departe,border="grey60",col=col,lwd=0.1,main="Ametoc 2016")
legend("right",fill=attr(leg, "palette"),
       legend=gsub("\\.", ",", names(attr(leg,"table"))),
       title = "Vente ametoc 2016 :")

#map for amisulbrom sells in 2016
temp<- merge(departe,pestamisu,by.x="NOM_DEP", by.y="departement")
col<-findColours(classIntervals(temp$quantity, 8, style="quantile"),
                 smoothColors("tan",6,"tan4"))
#departement with 0 sells were lacking in the dataset so are considered
#as without any recorded sell
col[is.na(temp$quantity)]<-"white"
#legend
leg<-findColours(classIntervals(round(temp$quantity/1000,1),8,
                                style="quantile"),smoothColors("tan",6,"tan4"),
                 under="moins de", over="plus de",between="–",cutlabels=FALSE)

plot(departe,border="grey60",col=col,lwd=0.1,main="Amisul 2016")
legend("right",fill=attr(leg, "palette"),
       legend=gsub("\\.", ",", names(attr(leg,"table"))),
       title = "Vente amisul 2016 :")

#map for cyazofamid sells in 2016
temp<- merge(departe,pestcyazo,by.x="NOM_DEP", by.y="departement")
col<-findColours(classIntervals(temp$quantity, 8, style="quantile"),
                 smoothColors("tan",6,"tan4"))
#departement with 0 sells were lacking in the dataset so are considered
#as without any recorded sell
col[is.na(temp$quantity)]<-"white"
#legend
leg<-findColours(classIntervals(round(temp$quantity/1000,1),8,
                                style="quantile"),smoothColors("tan",6,"tan4"),
                 under="moins de", over="plus de",between="–",cutlabels=FALSE)

plot(departe,border="grey60",col=col,lwd=0.1,main="Cyazo 2016")
legend("right",fill=attr(leg, "palette"),
       legend=gsub("\\.", ",", names(attr(leg,"table"))),
       title = "Vente ciazo 2016 :")

#map for fluopicolide sells in 2016
temp<- merge(departe,pestfluop,by.x="NOM_DEP", by.y="departement")
col<-findColours(classIntervals(temp$quantity, 8, style="quantile"),
                 smoothColors("tan",6,"tan4"))
#departement with 0 sells were lacking in the dataset so are considered
#as without any recorded sell
col[is.na(temp$quantity)]<-"white"
#legend
leg<-findColours(classIntervals(round(temp$quantity/1000,1),8,
                                style="quantile"),smoothColors("tan",6,"tan4"),
                 under="moins de", over="plus de",between="–",cutlabels=FALSE)

plot(departe,border="grey60",col=col,lwd=0.1,main="Fluop 2016")
legend("right",fill=attr(leg, "palette"),
       legend=gsub("\\.", ",", names(attr(leg,"table"))),
       title = "Vente fluop 2016 :")


###############################################################################
#THE END
###############################################################################


temp<-Ramet_list[Ramet_list$year==2017,]
plot(departe,border="grey60",lwd=0.1,main="2017")
plot(arrond[arrond$ID %in% temp$ID.y,],
     add=TRUE,col="blue",lwd=0.1)
plot(arrond[arrond$ID %in% temp$ID.y[temp$INSES_CODE %in% c("21480","51416")],],
     add=TRUE,col="red",lwd=0.1)
plot(regions,add=TRUE,lwd=1.5)


