#######################
# This script builds the data set for Study 2 reported
# in the paper "Partisan Elites as Culprits?"
########################

#load required packages
require(foreign)
require(car)

#load original data file containing study 2
expdat<-read.dta('original_data.dta')

####
# Constructing and recoding variables.

#party identification (measured with question tapping vote intention)
expdat$VK<-NA
expdat$VK[as.numeric(expdat$q5a)%in%c(3,8)|as.numeric(expdat$q5b)%in%c(3,8)]<-1	#Conservatives and Liberals are now in opposition and are coded as "1"	
expdat$VK[as.numeric(expdat$q5a)%in%c(1,2)|as.numeric(expdat$q5b)%in%c(1,2)]<-0	#Socialdemocrats and Social liberals are now in goverment and are coded as "0" 
table(expdat$VK)

#interpretations of budget decifit
expdat$budgetint<-NA
expdat$budgetint[!is.na(expdat$q9a)]<-expdat$q9a[!is.na(expdat$q9a)]
expdat$budgetint[!is.na(expdat$q9b)]<-expdat$q9b[!is.na(expdat$q9b)]
expdat$budgetint<-recode(expdat$budgetint,'1=5;2=4;4=2;5=1;6=NA');expdat$budgetint=(expdat$budgetint-1)/4	#dropping missing cases and rescaling to 0-1 scale

#interpretations of unemployment
expdat$unempint<-NA
expdat$unempint[!is.na(expdat$q10a)]<-expdat$q10a[!is.na(expdat$q10a)]
expdat$unempint[!is.na(expdat$q10b)]<-expdat$q10b[!is.na(expdat$q10b)]
expdat$unempint<-recode(expdat$unempint,'1=5;2=4;4=2;5=1;6=NA');expdat$unempint=(expdat$unempint-1)/4		#dropping missing cases and rescaling to 0-1 scale


#treatment indicator for experiment on budget deficits
expdat$Btreat<-NA
expdat$Btreat[!is.na(expdat$q9b)&is.na(expdat$q9a)]<-"noproblem"
expdat$Btreat[is.na(expdat$q9b)&!is.na(expdat$q9a)]<-"problem"

#treatment indicator for experiment on unemployment
expdat$Utreat<-NA
expdat$Utreat[!is.na(expdat$q10b)&is.na(expdat$q10a)]<-"noproblem"
expdat$Utreat[is.na(expdat$q10b)&!is.na(expdat$q10a)]<-"problem"


#extract variables above and additional covariates (used for robustness) to build the data set
keep<-c("RecordNo","VK","budgetint","unempint","Btreat","Utreat","education","region","gender","mobile_device","tot_time","age")

expdat<-expdat[,colnames(expdat)%in%keep]

#save data set
save(expdat,file="study2.Rdata")


###
# END