##############################################################################/
##############################################################################/
#Mapping of the AOX-related resistance
##############################################################################/
##############################################################################/

#this script produce the maps of the Figure 2
source("load_mildew_data.R")


##############################################################################/
#AOX map by departement####
##############################################################################/

#First, we reorder the datatable by ID (not really necesarry)
Raox_list<-Raox_list[order(Raox_list$sample_ID),]

op<-par(mar=c(0,0,1,0),mfrow=c(2,4))

#the first panel is left empty for the figure legend
plot(departe,border="white",lwd=0.1,main="")

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
plot(departe,border="grey40",lwd=1,main="2012",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","darkorchid1"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

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
plot(departe,border="grey40",lwd=1,main="2013",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","darkorchid1"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

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
plot(departe,border="grey40",lwd=1,main="2014",cex.main=1.5)
plot(regions,add=TRUE,lwd=1.5)
draw.pie(x=coorddep$longitude,y=coorddep$latitude,
         z=cbind(coorddep$nonR,coorddep$Res),
         col=c("blue","darkorchid1"),
         radius=(sqrt(coorddep$nb_fields)*18000),labels=NA)

#for the figure legend
plot(departe,border="white",lwd=0.1,main="")
draw.pie(x=c(490000,490000,490000,490000,490000),
         y=c(6300000,6450000,6570000,6655000,6720000),
         z=cbind(c(0,0,0,0,0),c(2,2,2,2,2)),
         radius=sqrt(c(14,10,5,2,1))*18000,col="grey70",border=FALSE)
text(x=c(820000,820000,820000,820000,820000),
     y=c(6300000,6450000,6570000,6655000,6720000),
     labels=c("14 fields","10 fields","5 fields","2 fields","1 field  "),
     cex=1.5,adj=c(1,0.5),family="",font=1)#pos=4)
text(x=630000,y=6900000,labels="Legend",cex=2,font=2)

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

#export pdf 13.33 x 6 inches


##############################################################################/
#END
##############################################################################/