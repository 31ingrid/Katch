setwd("/Users/ingridspies/admbtest/Katch")
setwd("inst")
source("../R/read-rep.R")
system("make") 
#optional if that doesn't work system("./Katch -nox") #also -ind for which input .dat
Katch=read.rep("../inst/Katch.rep") 

#Katch_sel is the run with just selectivities changed not tx file

#Read this in before running plots
if( (Katch$assessment=1)|(Katch$assessment=2) )#1 and 2 are BSAI and GOA ATF assessments, respectively
{
 lens=c(10, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 75, 95)
 midpoints=c(12.5,16.5,18.5,20.5,22.5,24.5,26.5,28.5,30.5,32.5,34.5,36.5,38.5,41,44,47,50,53,56,59,62,65,68,72,85)
}
if (Katch$assessment=3)#76	length	bins	from	15-90	cm
{
 lens=c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,100)
 midpoints=(lens+.5)[-77];midpoints[76]=95
}
#plots
sel();bio();mort();rec();shelf_LF();slope_LF();AI_LF();shelf_AF();slope_AF();AI_AF();

library(ggplot2);library(tidyr)
sel=function(){
Ages=rep(seq(1,Katch$nages_read,1),8)
Selectivity=c(Katch$Fishsel_fem,Katch$Fishsel_mal,Katch$Survsel_fem[1,],Katch$Survsel_mal[1,],Katch$Survsel_fem[2,],Katch$Survsel_mal[2,],Katch$Survsel_fem[3,],Katch$Survsel_mal[3,])
Sex=c(rep(c(rep("Female",Katch$nages_read),rep("Male",Katch$nages_read)),4))
Survey=c(rep("Fishery",2*Katch$nages_read),rep("Shelf",2*Katch$nages_read),rep("Slope",2*Katch$nages_read),rep("Aleutians",2*Katch$nages_read))
seldat=data.frame(Ages,Selectivity,Sex,Survey)
seldat$Survey=factor(seldat$Survey,levels=c("Fishery","Shelf","Slope","Aleutians"),ordered=TRUE)

ggplot(data=seldat)+aes(x=Ages,y=Selectivity,group=Sex,color=Sex)+geom_line()+facet_wrap(~Survey)
}

#compare biomass, fsb
bio=function(){
Year=c(seq(Katch$Styr,Katch$Endyr,1))
Female_spawning_biomass=Katch$fspbio
Total_biomass=Katch$pred_bio
bio=data.frame(Year,Female_spawning_biomass,Total_biomass)
bio1=gather(bio,value="Biomass",key="type",Female_spawning_biomass,Total_biomass)
ggplot(bio1,aes(x=Year,y=Biomass,color=type))+geom_line()
}

plot(seq(1976,2016,1),Katch$Mort_est_fem[,8],type="l",ylab="Biomass (t)",xlab="Year",cex.axis=1.4,cex.lab=1.4)
lines(seq(1976,2016,1),Katch_sel$Mort_est_fem[,8],col=1,lty=2)
lines(seq(1976,2016,1),Katch2016_15_1b_ji$Mort_est_fem[,8],col=1,lty=3)
legend("topright",c("2016 model","selectivity bounds","selectivity + conv. matrix"),col=c(1,1,1),lty=c(3,2,1))

plot(seq(1976,2011,1),2*Katch$Numbers_fem[1:36,1],type="l",ylab="Biomass (t)",xlab="Year",cex.axis=1.4,cex.lab=1.4,ylim=c(0,8e8))
lines(seq(1976,2011,1),2*Katch_sel$Numbers_fem[1:36,1],col=1,lty=2)
lines(seq(1976,2011,1),2*Katch2016_15_1b_ji$Numbers_fem[1:36,1],col=1,lty=3)
legend("bottomright",c("2016 model","selectivity bounds","selectivity + conv. matrix"),col=c(1,1,1),lty=c(3,2,1))


#fishing mortality on fully selected fish [,8]
mort=function(){
 Year=c(seq(Katch$Styr,Katch$Endyr,1))
 Female_Mortality=round(Katch$Mort_est_fem[,8],3)#BSAI Katch: 8 year old females have a peak in fishery selectivity and the shelf survey has age 8 as fully selected
 mor=data.frame(Year,Female_Mortality)
 ggplot(mor,aes(x=Year,y=Female_Mortality))+geom_line()
}

#recruitment
rec=function(){
 Year=c(seq(Katch$Styr,Katch$Endyr-5,1))
 Recruitment=Katch$Numbers_fem[1:length(seq(Katch$Styr,Katch$Endyr-5,1)),1]#remove last 5 years of recruitments
 Rec=data.frame(Year,Recruitment)
 ggplot(Rec,aes(x=Year,y=Recruitment))+geom_line()
}

#what do likelihoods look like?
Katch$catch_like;Katch$surv_like;Katch$sel_like;Katch$age_like_survey1;Katch$rec_like;Katch$length_like_fishery;Katch$length_like_survey

shelf_LF=function(){
Proportion=c(Katch$Obs_srv_lengthcomp_mal[1:Katch$nobs_srv_length[1],],-1*Katch$Obs_srv_lengthcomp_fem[1:Katch$nobs_srv_length[1],])
Length=rep(rep(midpoints,each=Katch$nobs_srv_length[1]),2)
Year=rep(as.numeric(Katch$Yrs_srv_lengthcomp[1,]),length(Proportion)/length(as.numeric(Katch$Yrs_srv_lengthcomp[1,])))
Sex=c(rep("Males",length(Year)/2),rep("Females",length(Year)/2))
Predicted=c(Katch$Pred_srv_lengthcomp_mal[1:Katch$nobs_srv_length[1],],-1*Katch$Pred_srv_lengthcomp_fem[1:Katch$nobs_srv_length[1],])
Shelf=data.frame(Year,Proportion,Length,Predicted,Sex)
p1=ggplot(data=Shelf,aes(x=Length,y=Proportion,group=Sex,color=Sex))+geom_bar(stat="Identity") + facet_wrap(~Year)+geom_line(data=Shelf,aes(x=Length,y=Predicted))
p1
}

#plot slope length frequencies
slope_LF=function(){
yr=as.numeric(Katch$Yrs_srv_lengthcomp[2,])
yr_good=yr[!is.na(yr)]
n=length(yr_good)
Year=rep(yr_good,2*length(midpoints))
Proportion=c(Katch$Obs_srv_lengthcomp_mal[(Katch$nobs_srv_length[1]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]),],-1*Katch$Obs_srv_lengthcomp_fem[(Katch$nobs_srv_length[1]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]),])
Length=rep(rep(midpoints,each=n),2)
Predicted=c(Katch$Pred_srv_lengthcomp_mal[(Katch$nobs_srv_length[1]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]),],-1*Katch$Pred_srv_lengthcomp_fem[(Katch$nobs_srv_length[1]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]),])
Sex=c(rep("Males",n*length(midpoints)),rep("Females",n*length(midpoints)))
Slope=data.frame(Year,Proportion,Length,Predicted,Sex)

p2=ggplot(data=Slope,aes(x=Length,y=Proportion,group=Sex,color=Sex))+geom_bar(stat="Identity") + facet_wrap(~Year)+geom_line(data=Slope,aes(x=Length,y=Predicted))
p2
}

#plot Aleutians length frequencies
AI_LF=function(){
yr=as.numeric(Katch$Yrs_srv_lengthcomp[3,])
yr_good=yr[!is.na(yr)]
n=length(yr_good)
Year=rep(yr_good,2*length(midpoints))
Proportion=c(Katch$Obs_srv_lengthcomp_mal[(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+Katch$nobs_srv_length[3]),],-1*Katch$Obs_srv_lengthcomp_fem[(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+Katch$nobs_srv_length[3]),])
Length=rep(rep(midpoints,each=n),2)
Predicted=c(Katch$Pred_srv_lengthcomp_mal[(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+Katch$nobs_srv_length[3]),],-1*Katch$Pred_srv_lengthcomp_fem[(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+1):(Katch$nobs_srv_length[1]+Katch$nobs_srv_length[2]+Katch$nobs_srv_length[3]),])
Sex=c(rep("Males",n*length(midpoints)),rep("Females",n*length(midpoints)))
Aleutians=data.frame(Year,Proportion,Length,Predicted,Sex)
p3=ggplot(data=Aleutians,aes(x=Length,y=Proportion,group=Sex,color=Sex))+geom_bar(stat="Identity") + facet_wrap(~Year)+geom_line(data=Aleutians,aes(x=Length,y=Predicted))
p3
}

#Plot shelf age frequencies (Kamchatka has 1 year of AI ages and 2 years of slope ages) so shelf_AF() plots kamchatka slope.
#no shelf age data for Kamchatka
shelf_AF=function(){
 Age=rep(seq(1,Katch$nages_read,1),2)
 Year=rep(as.numeric(Katch$Yrs_srv_age[1,]),each=Katch$nages_read)
 Proportion=c(t(Katch$Obs_srv_agecomp_mal[1:Katch$nobs_srv_age[1],]),-1*t(Katch$Obs_srv_agecomp_fem[1:Katch$nobs_srv_age[1],]))
 Predicted=c(t(Katch$Pred_srv_agecomp_mal[1:Katch$nobs_srv_age[1],]),-1*t(Katch$Pred_srv_agecomp_fem[1:Katch$nobs_srv_age[1],]))
 Sex=c(rep("Males",Katch$nobs_srv_age[1]*Katch$nages_read),rep("Females",Katch$nobs_srv_age[1]*Katch$nages_read))
 Shelf=data.frame(Age,Year,Proportion,Predicted,Sex)
 
 p1=ggplot(data=Shelf)+aes(x=Age,y=Proportion,group=Sex,color=Sex)+geom_bar(stat="Identity") + facet_wrap(~Year)+geom_line(data=Shelf,aes(x=Age,y=Predicted))
 p1
}

#Plot slope age frequencies #this plots Kamchatka AI age data
slope_AF=function(){
 Age=rep(seq(1,Katch$nages_read,1),2*Katch$nobs_srv_age[2])
 yr=as.numeric(Katch$Yrs_srv_age[2,])
 yr_good=yr[!is.na(yr)]
 n=length(yr_good)
 Year=rep(yr_good,2*Katch$nages_read)
 Proportion=c(Katch$Obs_srv_agecomp_mal[(Katch$nobs_srv_age[1]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]),],-1*Katch$Obs_srv_agecomp_fem[(Katch$nobs_srv_age[1]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]),])
 Predicted=c(Katch$Pred_srv_agecomp_mal[(Katch$nobs_srv_age[1]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]),],-1*Katch$Pred_srv_agecomp_fem[(Katch$nobs_srv_age[1]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]),])
 Sex=c(rep("Males",Katch$nages_read),rep("Females",Katch$nages_read))
 Slope=data.frame(Age,Year,Proportion,Predicted,Sex)
 p1=ggplot(data=Slope)+aes(x=Age,y=Proportion,group=Sex,color=Sex)+geom_bar(stat="Identity") + facet_wrap(~Year)+geom_line(data=Slope,aes(x=Age,y=Predicted))
 p1
}

#Plot AI age frequencies
AI_AF=function(){
 Age=rep(seq(1,Katch$nages_read,1),2*Katch$nobs_srv_age[3])
 yr=as.numeric(Katch$Yrs_srv_age[3,])
 yr_good=yr[!is.na(yr)]
 n=length(yr_good)
 Year=rep(yr_good,2*Katch$nages_read)
 Proportion=c(Katch$Obs_srv_agecomp_mal[(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+Katch$nobs_srv_age[3]),],-1*Katch$Obs_srv_agecomp_fem[(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+Katch$nobs_srv_age[3]),])
 Predicted=c(Katch$Pred_srv_agecomp_mal[(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+Katch$nobs_srv_age[3]),],-1*Katch$Pred_srv_agecomp_fem[(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+1):(Katch$nobs_srv_age[1]+Katch$nobs_srv_age[2]+Katch$nobs_srv_age[3]),])
 Sex=c(rep("Males",2*Katch$nages_read),rep("Females",2*Katch$nages_read))
 AI=data.frame(Age,Year,Proportion,Predicted,Sex)
 p1=ggplot(data=AI)+aes(x=Age,y=Proportion,group=Sex,color=Sex)+geom_bar(stat="Identity") + facet_wrap(~Year)+geom_line(data=AI,aes(x=Age,y=Predicted))
 p1
}


