###############################################################################
###############################################################################
#Evolution of AOX resistance through year and within season
###############################################################################
###############################################################################

#this script is used for the regression analysis of the frequency of the 
#populations containing AOX-related resistant strains and to produce Figure 1
#loading the packages necessary for the analysis
library(visreg)
library(lme4)


###############################################################################
#loading and preparing the AOX resistance data
###############################################################################

#we load the raw data
rez_list<-read.table("data/R_mildiouQI3.txt",header=TRUE,sep="\t")
#we subset the data to keep only samples with information regarding
#the AOX resistance
Raox_list<-rez_list[!is.na(rez_list$AOX) | 
                      (rez_list$AMISUL==1 & !is.na(rez_list$AMISUL)),]
#we then remove the 2 samples that display specific resistance to amisulbrom
#(see the main text of the article for more details)
Raox_listU<-Raox_list[is.na(Raox_list$AMISUL) | Raox_list$AMISUL==0,]
#and then we create a new variable 'dayofyear' which is the day of the year 
#when the sampling took place
Raox_listU$dayofyear<-as.POSIXlt((as.Date(Raox_listU$sampling_date)))$yday
#then we add a column for the day of the year when the first sample was 
#collected
Raox_listU$firstsampY<-
  c(rep(min(Raox_listU[Raox_listU$year==2012,"dayofyear"]),
        table(Raox_listU$year)[1]),
    rep(min(Raox_listU[Raox_listU$year==2013,"dayofyear"]),
        table(Raox_listU$year)[2]),
    rep(min(Raox_listU[Raox_listU$year==2014,"dayofyear"]),
        table(Raox_listU$year)[3]),
    rep(min(Raox_listU[Raox_listU$year==2015,"dayofyear"]),
        table(Raox_listU$year)[4]),
    rep(min(Raox_listU[Raox_listU$year==2016,"dayofyear"]),
        table(Raox_listU$year)[5]),
    rep(min(Raox_listU[Raox_listU$year==2017,"dayofyear"]),
        table(Raox_listU$year)[6]))
#finaly we create a variable 'dayEpid' which is the number of days after the 
#first sampling of the year
Raox_listU$dayEpid<-Raox_listU$dayofyear-Raox_listU$firstsampY


###############################################################################
#generalized linear mixed-model
###############################################################################

#we perform a logistic regression in order to test if the day of the epidemic, 
#and the year have an impact on the proportion of population displaying AOX 
#resistance results to bioassay
#to take into account some of the potential geographical autocorrelation, 
#we build a model with the "departement" a french administrative division as 
#a random effect in the model
Raox_listU$year<-Raox_listU$year-2011
AOX.mod<-glmer(AOX~dayEpid + year + (1|departement), family=binomial,
            data=Raox_listU)
summary(AOX.mod)
AOX.mod@beta

op<-par(mfrow=c(1,2),oma=c(0,1,0,0),mar=c(5,5,1,1))
#some visualisation of the regression results
visreg(AOX.mod,"year",rug=2,scale="response",jitter=TRUE,by="AOX",
       overlay=TRUE,partial=FALSE,xlab="Year",ylab="P(AOX-related resistance)",
       legend=FALSE,ylim=c(0,0.7),cex.lab=1.5,bty="n",axes=FALSE)
box(lwd=3)
axis(side=1,lwd.ticks=2,labels=c("2012","2013","2014","2015","2016","2017"),
     at=c(1,2,3,4,5,6),cex.axis=1.5)
axis(side=2,lwd.ticks=2,cex.axis=1.5,las=1)
visreg(AOX.mod,"dayEpid",rug=2,scale="response",jitter=TRUE,by="AOX",
       overlay=TRUE,partial=FALSE,xlab="Day of the epidemic season",
       ylab="P(AOX-related resistance)",legend=FALSE,ylim=c(0,0.7),cex.lab=1.5,
       bty="n",axes=FALSE)
box(lwd=3)
axis(side=1,lwd.ticks=2,cex.axis=1.5)
axis(side=2,lwd.ticks=2,cex.axis=1.5,las=1)
par(op)

#export in pdf 12 x 6 inches


###############################################################################
#additionnal code - generalized linear model (no random effect)
###############################################################################

#because the magnitude of the random effect explained by the 'departement' 
#administrative division, a simpler glm work as well
AOX.mod2<-glm(AOX~dayEpid+year,family="binomial",data=Raox_listU)
summary(AOX.mod2)

op<-par(mfrow=c(1,2))
#some visualisation of the regression results
visreg(AOX.mod2,"year",rug=2,scale="response",jitter=TRUE,by="AOX",
       overlay=TRUE,partial=FALSE,xlab="Year",ylab="P(AOX resistant)",
       legend=FALSE,ylim=c(0,0.7))
#export in pdf 8 x 6 inches
visreg(AOX.mod2,"dayEpid",rug=2,scale="response",jitter=TRUE,by="AOX",
       overlay=TRUE,partial=FALSE,xlab="Day of the epidemic season",
       ylab="P(AOX resistant)",legend=FALSE,ylim=c(0,0.7))
#export in pdf 8 x 6 inches
par(op)
#export in pdf 12 x 6 inches


#some other plot
barplot(table(Raox_listU$AOX,Raox_listU$year),beside=TRUE)
barplot(table(Raox_listU$AOX,Raox_listU$dayEpid),beside=TRUE)


###############################################################################
#THE END
###############################################################################