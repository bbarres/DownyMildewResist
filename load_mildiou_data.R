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
library(rgeos)


###############################################################################
#loading the geographical data
###############################################################################

#the geographical layer used here were downloaded on the IGN website (Geofla), 
#which is the French Institut of Geographic and forest information

#the smallest administrative unit in France: communes
load("data/commu.RData")
load("data/departe.RData")
load("data/regions.Rdata")

#isolate the information in the spatial data on the communes
db_commu<-commu@data
summary(db_commu)


###############################################################################
#loading the resistance data
###############################################################################

rez_list<-read.table("data/R_mildiouQI2.txt",header=TRUE,sep="\t")
Raox_list<-rez_list[!is.na(rez_list$AOX) | 
                      (rez_list$AMISUL==1 & !is.na(rez_list$AMISUL)),]
Ramet_list<-rez_list[!is.na(rez_list$AMETOC),]
RametBM_list<-rez_list[!is.na(rez_list$S34L),]
Ramis_list<-rez_list[!is.na(rez_list$AMISUL),]


###############################################################################
#END
###############################################################################



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