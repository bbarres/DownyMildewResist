##############################################################################/
##############################################################################/
#Mapping of the AOX-related resistance
##############################################################################/
##############################################################################/

#this code produce a map that is not displayed in the manuscript
source("load_mildew_data.R")


##############################################################################/
#Amisulbrom maps by departements####
##############################################################################/

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


##############################################################################/
#END
##############################################################################/