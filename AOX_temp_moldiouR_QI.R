###############################################################################
###############################################################################
#Evolution of AOX resistance through year and within season
###############################################################################
###############################################################################

#loading the packages necessary for the analysis
library(visreg)
library(lme4)

#Setting the right working directory
setwd("~/work/Rfichiers/Githuber/mildiou_mito_comp_data")


###############################################################################
#loading and preparing the AOX resistance data
###############################################################################

#we load the raw data
rez_list<-read.table("R_mildiouQI2.txt",header=TRUE,sep="\t")
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
#Is AOX resistant strains more common by the end of the season?
###############################################################################


#we plot the AOX resistance status according to the day of the year of
#sampling
plot(Raox_listU$AOX~Raox_listU$dayofyear)
boxplot(Raox_listU$dayofyear~Raox_listU$AOX)

#we plot the AOX resistance statut according to the number of day after 
#the beginning of the epidemic
plot(Raox_listU$AOX~Raox_listU$dayEpid)
boxplot(Raox_listU$dayEpid~Raox_listU$AOX)

#we perform a logistic regression in order to test if the day of the year, 
#the year and the departement (geographical subdivisions) have an impact 
#on the proportion of population displaying AOX resistance results to 
#bioassay
AOX.mod1<-glm(AOX~dayofyear+year+departement,family="binomial",
              data=Raox_listU)
summary(AOX.mod1)
AOX.mod1<-glm(AOX~dayofyear+year,family="binomial",
              data=Raox_listU)
summary(AOX.mod1)

#the model with the number of day after the beginning of the infection is 
#slightly more "informative" and it makes more biological sens
AOX.mod2comp<-glm(AOX~dayEpid*year,family="binomial",data=Raox_listU)
AOX.mod2<-glm(AOX~dayEpid+year,family="binomial",data=Raox_listU)
anova(AOX.mod2,AOX.mod2comp,test="Chisq")
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
plot(table(Raox_listU$AOX,Raox_listU$year)[2,]/
       colSums(table(Raox_listU$AOX,Raox_listU$year))~
       as.numeric(names(table(Raox_listU$AOX,Raox_listU$year)[2,])))
barplot(table(Raox_listU$AOX,Raox_listU$dayEpid),beside=TRUE)


###############################################################################
#generalized linear mixed-model
###############################################################################

#to take into account some of the potential geographical autocorrelation, 
#we build a model with the "département" a french administrative division as 
#a random effect in the model
Raox_listU$year<-ordered(Raox_listU$year)
Raox_listU$year<-Raox_listU$year-2011
mod3<-glmer(AOX~dayEpid * year + (1|departement), family=binomial,
            data=Raox_listU)
summary(mod3)
mod3<-glmer(AOX~dayEpid + year + (1|departement), family=binomial,
            data=Raox_listU)
summary(mod3)
mod3@beta


###############################################################################
#THE END
###############################################################################




plot(density(Raox_listU[Raox_listU$AOX==0,"dayEpid"]))
lines(density(Raox_listU[Raox_listU$AOX==1,"dayEpid"]),col="red")

plot(density(Raox_listU[Raox_listU$AOX==0 & 
                          Raox_listU$year==2012,"dayEpid"]))
lines(density(Raox_listU[Raox_listU$AOX==1 & 
                           Raox_listU$year==2012,"dayEpid"]),col="red")

plot(density(Raox_listU[Raox_listU$AOX==0 & 
                          Raox_listU$year==2013,"dayEpid"]))
lines(density(Raox_listU[Raox_listU$AOX==1 & 
                           Raox_listU$year==2013,"dayEpid"]),col="red")

plot(density(Raox_listU[Raox_listU$AOX==0 & 
                          Raox_listU$year==2014,"dayEpid"]))
lines(density(Raox_listU[Raox_listU$AOX==1 & 
                           Raox_listU$year==2014,"dayEpid"]),col="red")

plot(density(Raox_listU[Raox_listU$AOX==0 & 
                          Raox_listU$year==2015,"dayEpid"]))
lines(density(Raox_listU[Raox_listU$AOX==1 & 
                           Raox_listU$year==2015,"dayEpid"]),col="red")

plot(density(Raox_listU[Raox_listU$AOX==0 & 
                          Raox_listU$year==2016,"dayEpid"]))
lines(density(Raox_listU[Raox_listU$AOX==1 & 
                           Raox_listU$year==2016,"dayEpid"]),col="red")

plot(density(Raox_listU[Raox_listU$AOX==0 & 
                          Raox_listU$year==2017,"dayEpid"]))
lines(density(Raox_listU[Raox_listU$AOX==1 & 
                           Raox_listU$year==2017,"dayEpid"]),col="red")

