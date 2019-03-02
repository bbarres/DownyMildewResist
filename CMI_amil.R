###############################################################################
###############################################################################
#Supplementary material code for amisulbrom base-line
###############################################################################
###############################################################################

#this script is used for the computation of the MIC of the different 
#populations tested with amisulbrom, in order to justify the DD used in the 
#manuscript for amisulbrom
#loading the packages necessary for the analysis
library(tidyr)
library(dplyr)
library(drc)
library(gdata)
library(RColorBrewer)
library(scales)


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
datnoSHAM<-drop.levels(datnoSHAM)
infsamp<-datnoSHAM[datnoSHAM$dose==0,1:4]
datwiSHAM<-datamilsub[datamilsub$sham=="yes",]
datwiSHAM<-drop.levels(datwiSHAM)


###############################################################################
#computing the MIC for bioassay without SHAM
###############################################################################

cmiREZ<-data.frame("sample_ID"=character(),"MIC"=numeric())

for (i in 1: dim(table(datnoSHAM$sample_ID))[1]) {
  temp.m1<-drm(perc_sp~dose,
               data=datnoSHAM[datnoSHAM$sample_ID==
                                names(table(datnoSHAM$sample_ID))[i],],
               fct=EXD.3(),lowerl=c(-1,70,0.01),upperl=c(2,130,100))
  plot(temp.m1)
  temp<-ED(temp.m1,99.9999999,bound=FALSE)
  tempx<-data.frame("sample_ID"=names(table(datnoSHAM$sample_ID))[i],
                    "MIC"=temp[1])
  cmiREZ<-rbind(cmiREZ,tempx)
}

plot(log(cmiREZ$MIC))
plot(log(sort(cmiREZ$MIC)),col=)
abline(0,0,col="red",lwd=3)
#how many samples have a MIC above 1 mg/l?
summary(cmiREZ$MIC>1)


###############################################################################
#computing the MIC for bioassay with SHAM
###############################################################################

cmiREZSHAM<-data.frame("sample_ID"=character(),"MIC-SHAM"=numeric())

for (i in 1: dim(table(datwiSHAM$sample_ID))[1]) {
  temp.m1<-drm(perc_sp~dose,
               data=datwiSHAM[datwiSHAM$sample_ID==
                                names(table(datwiSHAM$sample_ID))[i],],
               fct=EXD.3(),lowerl=c(-1,70,0.01),upperl=c(2,130,100))
  plot(temp.m1)
  temp<-ED(temp.m1,99.9999999,bound=FALSE)
  tempx<-data.frame("sample_ID"=names(table(datwiSHAM$sample_ID))[i],
                    "MIC-SHAM"=temp[1])
  cmiREZSHAM<-rbind(cmiREZSHAM,tempx)
}

plot(log(cmiREZSHAM$MIC))
plot(log(sort(cmiREZSHAM$MIC)),col=)
abline(0,0,col="red",lwd=3)
#how many samples have a MIC above 1 mg/l?
summary(cmiREZSHAM$MIC>1)


###############################################################################
#preparing the output data and producing the supplementary figures
###############################################################################

#merging the information on sampled populations and their computed MIC
infsamp<-merge(infsamp,cmiREZ,by="sample_ID")
infsamp<-merge(infsamp,cmiREZSHAM,by="sample_ID",all.x=TRUE)
#adding a color column that correspond to the sampling year
infsamp$coloV<-as.factor(infsamp$year)
levels(infsamp$coloV)<-brewer.pal(4,"Dark2")

op<-par(mfrow=c(3,1),mar=c(1,6.1,1,1))
plot(log(sort(infsamp$MIC)),pch=19,cex=1.5,las=1,axes=FALSE,
     ann=FALSE,col=alpha(infsamp[order(infsamp$MIC),"coloV"],0.5))
abline(log(1),0,col="red",lwd=3)
axis(1,labels=FALSE,lwd=4,font=2,lwd.ticks=0)
axis(2,at=log(c(0.2,1,10,100,500,1500)),cex.axis=1.5,
     labels=c("0.2","1","10","100","500","1500"),lwd=4,font=2,las=1)
box(bty="o",lwd=4)
title(main=NULL,xlab="Index of Population",ylab="MIC (mg/L)",cex.lab=2,
      font.lab=2)
legend(8,log(1500),
       legend=c("2012","2013","2014","2015"),
       pch=19,col=levels(infsamp$coloV),bg=levels(infsamp$coloV),
       bty="n",cex=2,pt.cex=3,xpd=TRUE,text.font=2,
       ncol=1,x.intersp=2.5,y.intersp=0.8)

plot(log(arrange(infsamp,year,MIC)$MIC),pch=19, cex=1.5,las=1,axes=FALSE,
     ann=FALSE,col=alpha(arrange(infsamp,year,MIC)$coloV,0.5))
abline(log(1),0,col="red",lwd=3)
axis(1,labels=FALSE,lwd=4,font=2,lwd.ticks=0)
axis(2,at=log(c(0.2,1,10,100,500,1500)),cex.axis=1.5,
     labels=c("0.2","1","10","100","500","1500"),lwd=4,font=2,las=1)
box(bty="o",lwd=4)
title(main=NULL,xlab="Index of Population",ylab="MIC (mg/L)",cex.lab=2,
      font.lab=2)
legend(8,log(1500),
       legend=c("2012","2013","2014","2015"),
       pch=19,col=levels(infsamp$coloV),bg=levels(infsamp$coloV),
       bty="n",cex=2,pt.cex=3,xpd=TRUE,text.font=2,
       ncol=1,x.intersp=2.5,y.intersp=0.8)

par(mar=c(5.1,6.1,1,1))
plot(log(arrange(infsamp,year,MIC)$MIC)[arrange(infsamp,year,MIC)$year==2013],
     pch=19,cex=2,col=alpha(levels(infsamp$coloV)[2],0.8),ann=FALSE,
     axes=FALSE)
points(log(arrange(infsamp,year,MIC)$MIC.SHAM)
       [arrange(infsamp,year,MIC)$year==2013],pch=17,cex=2,
       col=alpha(brewer.pal(7,"Accent")[5],0.8))
abline(log(1),0,col="red",lwd=3)
for (i in 1:length(cmiREZSHAM$MIC.SHAM)) {
  arrows(x0=i,
         y0=log(arrange(infsamp,year,MIC)$MIC)
         [arrange(infsamp,year,MIC)$year==2013][i],
         x1=i,
         y1=log(arrange(infsamp,year,MIC)$MIC.SHAM)
         [arrange(infsamp,year,MIC)$year==2013][i],
         angle=20,length=0.06,lwd=1.5,col=alpha(brewer.pal(8,"Dark2")[8],1))
}
axis(1,labels=FALSE,lwd=4,font=2,lwd.ticks=0)
axis(2,at=log(c(0.2,1,10,100,500,1500)),cex.axis=1.5,
     labels=c("0.2","1","10","100","500","1500"),lwd=4,font=2,las=1)
box(bty="o",lwd=4)
title(main=NULL,xlab="Index of Population",ylab="MIC (mg/L)",cex.lab=2,
      font.lab=2)
legend(1,log(1500),
       legend=c("2013 amisulbrom","2013 amisulbrom with SHAM"),
       pch=c(19,17),col=c(levels(infsamp$coloV)[2],brewer.pal(7,"Accent")[5]),
       bg=c(levels(infsamp$coloV)[2],brewer.pal(7,"Accent")[5]),
       bty="n",cex=2,pt.cex=3,xpd=TRUE,text.font=2,
       ncol=1,x.intersp=2.5,y.intersp=0.8)
par(op)

#export to pdf 7 x 14 inches


###############################################################################
#END
###############################################################################