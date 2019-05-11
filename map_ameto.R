###############################################################################
###############################################################################
#Mapping of the AOX-related resistance
###############################################################################
###############################################################################

#this script produce the maps of the Figure 3
source("load_mildew_data.R")


###############################################################################
#Biological and BM resistant to ametoc maps by departement
###############################################################################

#in order to ease the count of different classes of resistance we turn the 
#variable of interest into a factor
Ramet_list$AMETOC<-as.factor(Ramet_list$AMETOC)

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
plot(departe,border="grey40",lwd=1,main="2014",cex.main=1.5)
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

#for the figure legend
plot(departe,border="white",lwd=0.1,main="")
draw.pie(x=c(490000,490000,490000,490000,490000),
         y=c(6300000,6430000,6530000,6620000,6690000),
         z=cbind(c(0,0,0,0,0),c(2,2,2,2,2)),
         radius=sqrt(c(9,6,4,2,1))*18000,col="grey70",border=FALSE)
text(x=c(810000,810000,810000,810000,810000),
     y=c(6310000,6430000,6530000,6620000,6690000),
     labels=c("9 fields","6 fields","4 fields","2 fields","1 field  "),
     cex=1.5,adj=c(1,0.5))#pos=4)
text(x=610000,y=6900000,labels="Legend",cex=2,font=2)

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
plot(departe,border="grey40",lwd=1,main="2015",cex.main=1.5)
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
plot(departe,border="grey40",lwd=1,main="2016",cex.main=1.5)
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
plot(departe,border="grey40",lwd=1,main="2017",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","red"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

par(op)

#export pdf 13.33 x 6 inches


###############################################################################
#END
###############################################################################