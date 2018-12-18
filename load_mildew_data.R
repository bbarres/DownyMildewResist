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

#the geographical layer used here were downloaded on the IGN (the French 
#Institut of Geographic and forest information) website: 
#http://professionnels.ign.fr/adminexpress and then turned into a .RData file
#The version of the dataset used here is the "Edition Novembre 2017"
#These data are under an open licence: 
#https://www.etalab.gouv.fr/wp-content/uploads/2014/05/Licence_Ouverte.pdf

#loading the different administrative unit levels in France
load("data/commu.RData")
load("data/departe.RData")
load("data/regions.Rdata")

#isolate the information in the spatial data on the communes
db_commu<-commu@data
summary(db_commu)


###############################################################################
#loading the resistance data
###############################################################################

rez_list<-read.table("data/R_mildiouQI2.txt",header=TRUE,sep="\t",colClasses="factor")
Raox_list<-rez_list[!is.na(rez_list$AOX) | 
                      (rez_list$AMISUL==1 & !is.na(rez_list$AMISUL)),]
Ramet_list<-rez_list[!is.na(rez_list$AMETOC),]
RametBM_list<-rez_list[!is.na(rez_list$S34L),]
Ramis_list<-rez_list[!is.na(rez_list$AMISUL),]


###############################################################################
#END
###############################################################################