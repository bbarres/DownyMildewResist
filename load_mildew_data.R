##############################################################################/
##############################################################################/
#Mapping the different resistance to Quinone Inhibitors in France
##############################################################################/
##############################################################################/

#loading the packages necessary for the analysis
library(rgdal)
library(plotrix)
library(classInt)
library(mapplots)
library(rgeos)
library(visreg)
library(lme4)
library(tidyr)
library(dplyr)
library(drc)
library(gdata)
library(RColorBrewer)
library(scales)


##############################################################################/
#loading the geographical data####
##############################################################################/

#the geographical layer used here were downloaded on the IGN (the French 
#Institut of Geographic and forest information) website: 
#http://professionnels.ign.fr/adminexpress and then turned into a .RData file
#The version of the dataset used here is the "Edition Novembre 2017"
#These data are under an open licence: 
#https://www.etalab.gouv.fr/wp-content/uploads/2014/05/Licence_Ouverte.pdf

#loading the different administrative unit levels in France
load("data/departe.RData")
load("data/regions.Rdata")


##############################################################################/
#loading the resistance data####
##############################################################################/

rez_list<-read.table("data/R_mildiouQI3.txt",header=TRUE,sep="\t",
                     colClasses="factor")
Raox_list<-rez_list[!is.na(rez_list$AOX) | 
                      (rez_list$AMISUL==1 & !is.na(rez_list$AMISUL)),]
Ramet_list<-rez_list[!is.na(rez_list$AMETOC),]
RametBM_list<-rez_list[!is.na(rez_list$S34L),]
Ramis_list<-rez_list[!is.na(rez_list$AMISUL),]


##############################################################################/
#loading and preparing the AOX resistance data for supplementary material####
##############################################################################/

#we load the raw data
datamilsub<-read.table("data/Amisul_base.txt",header=TRUE,sep="\t", 
                       check.names=FALSE)
datamilsub<-gather(datamilsub,'0','0.01','0.1','1','10','100',
                   key="dose",value="perc_sp")
datamilsub$dose<-as.numeric(datamilsub$dose)

#subsetting the data according to the SHAM addition
datnoSHAM<-datamilsub[datamilsub$sham=="no",]
datnoSHAM<-drop.levels(datnoSHAM)
infsamp<-datnoSHAM[datnoSHAM$dose==0,1:4]
datwiSHAM<-datamilsub[datamilsub$sham=="yes",]
datwiSHAM<-drop.levels(datwiSHAM)


##############################################################################/
#Writing info session for reproducibility####
##############################################################################/

sink("session_info.txt")
print(sessioninfo::session_info())
sink()
#inspired by an R gist of François Briatte: 
#https://gist.github.com/briatte/14e47fb0cfb8801f25c889edea3fcd9b


##############################################################################/
#END
##############################################################################/