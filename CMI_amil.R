###############################################################################
###############################################################################
#Supplementary material code for amisulbrom base-line
###############################################################################
###############################################################################

#this script is used for the regression analysis of the frequency of the 
#populations containing AOX-related resistant strains and to produce Figure 1
#loading the packages necessary for the analysis
library(tidyr)
library(drc)


###############################################################################
#loading and preparing the AOX resistance data
###############################################################################

#we load the raw data
datamilsub<-read.table("data/Amisul_base.txt",header=TRUE,sep="\t", 
                       check.names=FALSE)
datamilsub<-gather(datamilsub,'0','0.01','0.1','1','10','100',
                   key="dose",value="perc_sp")
datamilsub$dose<-as.numeric(datamilsub$dose)

#subsetting the data according to the SHAM addition
datnoSHAM<-datamilsub[datamilsub$sham=="no",]
datwiSHAM<-datamilsub[datamilsub$sham=="yes",]




cmiREZ<-data.frame("sample_ID"=character(),"ED50"=numeric())

for (i in 1: dim(table(datnoSHAM$sample_ID))[1]) {
  temp.m1<-drm(perc_sp~dose,
               data=datnoSHAM[datnoSHAM$sample_ID==names(table(datnoSHAM$sample_ID))[i],],
               fct=EXD.2())
  temp<-ED(temp.m1,99,bound=FALSE)
  tempx<-data.frame("sample_ID"=names(table(datnoSHAM$sample_ID))[i],
                    "MIC"=temp[1])
  cmiREZ<-rbind(cmiREZ,tempx)
}


###############################################################################
#END
###############################################################################