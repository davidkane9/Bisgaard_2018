###########################
# SCRIPT USED FOR EXTRACTING MAIN VARIABLES FROM THE FIVE-WAVE PANEL DATA
#
# NOTE: Original data file is in "wide" format and the code below extracts the variables of interest from
# the original file, rearranges the data frame into "long" format, and recodes the variables.
##########################


#load required packages
library(foreign)
library(car)
library(plyr)
library(plm)

#load original data file
paneldat<-read.dta("original_data.dta",convert.factors=FALSE)

#generate more tractable ID variable
paneldat$id<-NA 
paneldat$id<-1:length(paneldat$id) 

#rename outcome variables into more consistent format to reshape data frame more easily (number after "." indicates panel wave)
paneldat<-rename(paneldat,c(
	"q27"="ui.1","q27_efter"="ui.2","q27_iii"="ui.3","q27_iv"="ui.4","q27_v"="ui.5",
	"q29"="bi.1","q29_efter"="bi.2","q29_iii"="bi.3","q29_iv"="bi.4","q29_v"="bi.5",
	"q28"="bb.1","q28_efter"="bb.2","q28_iii"="bb.3","q28_iv"="bb.4","q28_v"="bb.5",
	"q5"="party.1","q5_efter"="party.2","q5_iii"="party.3","q5_iv"="party.4","q5_v"="party.5",
	"q1"="vote.1","q1_efter"="vote.2","q1_iii"="vote.3","q1_iv"="vote.4","q1_v"="vote.5",
	"q2"="vlean.1","q2_efter"="vlean.2","q2_iii"="vlean.3","q2_iv"="vlean.4","q2_v"="vlean.5",
	"q7"="partynear.1","q7_efter"="partynear.2","q7_iii"="partynear.3","q7_iv"="partynear.4","q7_v"="partynear.5",
	"q6"="partystr.1","q6_efter"="partystr.2","q6_iii"="partystr.3","q6_iv"="partystr.4","q6_v"="partystr.5",
	"q49_iii"="follow_media","q68_iii"="poltot_media",
	"q50_iii"="timer","q51_iii"="berlingske","q52_iii"="BT","q53_iii"="Børsen","q54_iii"="EB",
	"q55_iii"="Information","q56_iii"="KD","q57_iii"="JP","q58_iii"="METRO","q59_iii"="POL","q60_iii"="URBAN","q61_iii"="local",
	"q63_iii"="DR1","q64_iii"="TV2","q65_iii"="TV2NEWS","q66_iii"="DR2","q67_iii"="DRupdate"	
	))


#construct timeinvariant PID indicator based on wave 1 answers

paneldat$partytimeinvar<-NA
paneldat$partytimeinvar[paneldat$party.1 %in% c(3,8) | paneldat$partynear.1 %in% c(3,8)]<-0 #3 and 8 are the incumbent parties ("Venstre" and "Konservative")
paneldat$partytimeinvar[paneldat$party.1 %in% c(1,4) | paneldat$partynear.1 %in% c(1,4)]<-1 #1 and 4 are main opposition parties ("Socialdemokraterne" and "Socialistisk folkeparti")
#Note: PID measure includes leaners (captured by "partynear.1")


#construct additional measures of party identification used for robustness checks

#strength of PID measured in wave 1
paneldat$partystrW1<-paneldat$partystr.1

#PID measure with all parties (including leaners)
paneldat$partyAll<-NA
paneldat$partyAll[paneldat$party.1 %in% c(1) | paneldat$partynear.1 %in% c(1)]<-"S"
paneldat$partyAll[paneldat$party.1 %in% c(2) | paneldat$partynear.1 %in% c(2)]<-"RV"
paneldat$partyAll[paneldat$party.1 %in% c(3) | paneldat$partynear.1 %in% c(3)]<-"C"
paneldat$partyAll[paneldat$party.1 %in% c(4) | paneldat$partynear.1 %in% c(4)]<-"SF"
paneldat$partyAll[paneldat$party.1 %in% c(5) | paneldat$partynear.1 %in% c(5)]<-"LA"
paneldat$partyAll[paneldat$party.1 %in% c(6) | paneldat$partynear.1 %in% c(6)]<-"Kr"
paneldat$partyAll[paneldat$party.1 %in% c(7) | paneldat$partynear.1 %in% c(7)]<-"DF"
paneldat$partyAll[paneldat$party.1 %in% c(8) | paneldat$partynear.1 %in% c(8)]<-"V"
paneldat$partyAll[paneldat$party.1 %in% c(9) | paneldat$partynear.1 %in% c(9)]<-"Ø"
paneldat$partyAll[paneldat$partynear.1 %in% c(11,13)]<-"None"
paneldat$partyAll[paneldat$partynear.1 %in% c(12)]<-"Refuse"
paneldat$partyAll[paneldat$partynear.1 %in% c(10)|paneldat$party.1 %in% c(10)]<-"Other"

#Alternate measure based on vote intention also from wave 1
paneldat$voteall<-NA
paneldat$voteall[paneldat$vote.1 %in% c(3,8)|paneldat$vlean.1 %in% c(3,8)]<-0
paneldat$voteall[paneldat$vote.1 %in% c(1,4)|paneldat$vlean.1 %in% c(1,4)]<-1


#Construct a wave 1 measure for the dependent variable (used for robustness)
paneldat$biinvar<-paneldat$bi.1

#Constructing and cleaning media exposure measures (used for robustness)
paneldat$timer<-recode(paneldat$timer,'9=NA')-1	
paneldat$berlingske<-recode(paneldat$berlingske,'9=NA')-1
paneldat$BT<-recode(paneldat$BT,'9=NA')-1
paneldat$Børsen<-recode(paneldat$Børsen,'9=NA')-1
paneldat$EB<-recode(paneldat$EB,'9=NA')-1
paneldat$Information<-recode(paneldat$Information,'9=NA')-1
paneldat$KD<-recode(paneldat$KD,'9=NA')-1
paneldat$JP<-recode(paneldat$JP,'9=NA')-1
paneldat$METRO<-recode(paneldat$METRO,'9=NA')-1
paneldat$POL<-recode(paneldat$POL,'9=NA')-1
paneldat$URBAN<-recode(paneldat$URBAN,'9=NA')-1
paneldat$local<-recode(paneldat$local,'9=NA')-1
paneldat$DR1<-recode(paneldat$DR1,'9=NA')-1
paneldat$TV2<-recode(paneldat$TV2,'9=NA')-1
paneldat$TV2NEWS<-recode(paneldat$TV2NEWS,'9=NA')-1
paneldat$DR2<-recode(paneldat$DR2,'9=NA')-1
paneldat$DRupdate<-recode(paneldat$DRupdate,'9=NA')-1


####
# RESHAPING DATA FRAME

#specify subset of variables to keep for main analysis
media_names<-c("timer","berlingske","BT","Børsen","EB","Information","KD","JP","METRO","POL","URBAN","local","DR1",
	"TV2","TV2NEWS","DR2","DRupdate")
keep<-c("id",media_names,"follow_media","poltot_media","voteall","voteF","uiinvar","biinvar","partytimeinvar","partyAll","partystrW1","sex","age","occupation","education","income",paste("ui.",1:5,sep=""),
	paste("bi.",1:5,sep=""),paste("bb.",1:5,sep=""))

#clean data frame of excess variables
new<-paneldat[,(names(paneldat) %in% keep)] 
names(new);dim(new);dim(paneldat)


#reshape data frame from WIDE to LONG/stacked version
dat<-reshape(new,direction="long",
	idvar="id",sep=".",varying=keep[c(-1:-32)])
names(dat);dim(dat)

#Include an indicator for survey wave
dat$time<-as.factor(dat$time);is.factor(dat$time)


###############################################
# RECODING and CONSTRUCTING VARIABLES FURTHER #
# (now in stacked format)

#strength of PID
dat$partystrW1<-recode(dat$partystrW1,'3=0;2=0')
table(dat$partystrW1)

#Strength measure that includes leaners as "weak partisans"
dat$partystrAll[dat$partystrW1==1]<-1
dat$partystrAll[dat$partystrW1==0 | is.na(dat$partystrW1)]<-0
table(dat$partystrAll)

#education
dat$educationClean<-NA
dat$educationClean<-as.numeric(as.factor(dat$education)) 				#gets rid of long category names
dat$educationClean<-recode(dat$educationClean,'3=1;1=2;2=3;4=3;5=4;7=4;6=5') 		#rearrange into broader, ordered categories
table(dat$education,dat$educationClean)							#check whether rank is OK

#income
table(dat$income) #Note: a pretty substantial part chose the "do not want to report" option, income is thus used as categorical in the analysis to retain these individuals

#gender
dat$sex<-as.numeric(as.factor(dat$sex))-1 #male is 1

#recoding dependent variable: interpretation of budget deficit
dat$bi<-recode(dat$bi,"1=5;2=4;4=2;5=1;6=NA");dat$bi<-(dat$bi-1)/4 				#coding D/K into missing, reversing scale (high values=big problem, and rescaling to 0-1 scale)
dat$biinvar<-recode(dat$biinvar,"1=5;2=4;4=2;5=1;6=NA");dat$biinvar<-(dat$biinvar-1)/4 	#same procedure for the wave 1 measure (robustness)

#recoding placebo measure (intepretations of unemployment)
dat$ui<-recode(dat$ui,"1=5;2=4;4=2;5=1;6=NA");dat$ui<-(dat$ui-1)/4				#coding D/K into missing, reversing scale (high values=big problem, and rescaling to 0-1 scale)

#questions tapping factual beliefs about budget deficit (used for robustness)
dat$bb<-recode(dat$bb,"9=NA");dat$bb<-(dat$bb-1)/7 							#D/K is dropped and item rescaled from 0-1


#constructing variable tapping exposure to party cue 
dat$partycue<-NA
dat$partycue[dat$time %in% c(1,2)]<-0									#wave 1 and 2 are before the change in party cues
dat$partycue[dat$time %in% c(3,4,5)]<-1									#wave 3,4 and 5 are after.
table(dat$partycue,dat$time)											#checks coding



#########################
# BALANCING THE PANEL	#

dat.balance<-dat[!dat$id %in% (dat[is.na(dat$bi)|is.na(dat$ui),]$id),] 	#discards individuals who did not participate in all 5 waves or answered D/K on DV or placebo outcome
dim(dat.balance)										#check dimensions of data frame
table(dat.balance$partytimeinvar,dat.balance$time) 				#check if no. of units is the same for each survey wave

rm(new)											#removes temporary data set



#constructing time-counter to capture time-trends in panel models.
#note: "thedates" also includes the date for Restoration act (19 May, 2010) and
#the entries below for each survey wave are the day in the middle of the fielding period

thedates<-c("2010-02-22","2010-04-05","2010-05-19","2010-06-15","2011-01-11","2011-06-25")
thedates<-as.Date(thedates)

#calculate difference in days relative to Restoration Act
diffdays=NA
for(i in 1:length(thedates)) diffdays[i]<-thedates[i]-thedates[3]
diffdays

#construct time counter, "MonchC", which captures distance in time (in 30 days) to restoration act from each survey wave
dat.balance$monthC=NA;dat$monthC=NA
for(i in 1:5) dat.balance$monthC[dat.balance$time==i]=(diffdays[-3][i])/30
for(i in 1:5) dat$monthC[dat$time==i]=(diffdays[-3][i])/30
table(dat.balance$time,dat.balance$monthC) #check with original "time" variable



#Store data files

save(dat.balance,file="study1.Rdata")				#store the balanced panel data (long format)
save(dat,file="study1_unbalanced.Rdata")			#store the unbalanced data (long format)

paneldat<-paneldat[colnames(paneldat)%in%c("time",keep)]	#drop excess variables in wide format version
save(paneldat,file="study1_wide.Rdata")				#store the data in wide format



####
#END OF SCRIPT