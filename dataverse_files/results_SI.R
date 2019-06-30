#######################
# This script reproduces the supplemental analyses 
# reported in the Supporting Information appendix
# for the article "Partisan Elites as Culprits?"
#
#######################

#NOTE: REMEMBER TO SET WORKING DIRECTORY TO FOLDER CONTAINING REPLICATION FILES
#setwd("myreplicationfolder")


#load required packages (make sure that all packages are installed and loaded correctly)
library(foreign)
library(memisc)
library(car)
library(lmtest)
library(sandwich)
library(stargazer)
library(xtable)
library(irr)
library(quanteda)
library(pglm)
library(lme4)


###########################
#SECTION A: Description of Key Measures 
load("study1.RData")
load("study2.RData")

#descriptive statistics for listed variables

#Perception of budget deficit (Study 1)
mean(dat.balance$bi)
sd(dat.balance$bi)

#Perception of unemployment (Study 1)
mean(dat.balance$ui)
sd(dat.balance$ui)

#Party identification (Study 1)
mean(dat.balance$partytimeinvar,na.rm=T)

#Education (Study 1)
mean(dat.balance$educationClean)
sd(dat.balance$educationClean)

#Age (Study 1)
mean(dat.balance$age)
sd(dat.balance$age)

#Sex (Study 1)
mean(dat.balance$sex)

#Perception of budget deficit (Study 2)
mean(expdat$budgetint[!is.na(expdat$VK)],na.rm=T)
sd(expdat$budgetint[!is.na(expdat$VK)],na.rm=T)

#Perception of unemployment (Study 2)
mean(expdat$unempint[!is.na(expdat$VK)],na.rm=T)
sd(expdat$unempint[!is.na(expdat$VK)],na.rm=T)

#Party identification (Study 2)
mean(expdat$VK,na.rm=T)



###########################
#SECTION B: Manual Coding of Party Cues

#load data on statements from the Prime Minister
load('statements.RData')

###
#Reproduce results in Table S2
#note: the values in the "mentions" column in the table has been retrieved from an online search in the infomedia data base (see SI B.1 for more information)

mentions<-c(333,339,204,305,244)	
quotes<-table(statement$month)	#import total no. of quotes from the Statements dataset
ratio<-quotes/mentions			#compute quote/mention ratio

table_s2<-cbind(mentions,quotes,ratio)
table_s2<-rbind(table_s2,c(sum(mentions),sum(quotes),NA))
rownames(table_s2)<-c("January","February","March","April","May","Total")
table_s2

###
# Reproduce results in Table S3

tab<-table(statement$month,statement$newsp)
t<-table(statement$newsp)
table_s3<-rbind(tab,t)
colnames(table_s3)<-c("Berlingske","Information","Jyllands-Posten","Politiken")
rownames(table_s3)<-c("January","February","March","April","May","Total")
table_s3

###
# Reproduce results in Table S4

#cleaning codes from coder 1, 2, 3, and 4.
statement$c1=recode(statement$c1,'2=1;3=1;4=0;5=0;-11=0;-99=NA')
statement$c2=recode(statement$c2,'2=1;3=1;4=0;5=0;-11=0;-99=NA')
statement$c3=recode(statement$c3,'2=1;3=1;4=0;5=0;-11=0;-99=NA')
statement$c4=recode(statement$c4,'2=1;3=1;4=0;5=0;-11=0;-99=NA')

#Note: The commands above recodes the classifications from the coders into binary outcomes where: 
#	 1=PM says deficit is a problem 
#	 0=PM does not talk about issue/says it is not a problem 
#	 In addition, -99 are coded as missing and denotes articles that where not relevant (e.g. comics, op-eds)	

tab<-cbind(table(statement$c1),table(statement$c2),table(statement$c3),table(statement$c4))
table_s4<-rbind(tab,colSums(tab))
colnames(table_s4)<-c("Coder 1","Coder 2","Coder 3","Coder 4")
table_s4


###
# Reproduce Krippendorfs reliability coefficient (reported in text, SI section B.3)

kripp.alpha(rbind(statement$c1,statement$c2,statement$c4,statement$c3),method="nominal")


###
# Reproduce results in Table S5

agree_rate<-function(x1,x2) sum(diag(prop.table(table(x1,x2)))) #simple function for calculating agreement rate

agree<-c(agree_rate(statement$c1,statement$c2),
	agree_rate(statement$c1,statement$c3),
	agree_rate(statement$c1,statement$c4),
	agree_rate(statement$c2,statement$c3),	
	agree_rate(statement$c2,statement$c4),
	agree_rate(statement$c3,statement$c4))

table_s5<-matrix(NA,nc=4,nr=4)
table_s5[2:4,1]<-c(agree[1:3])
table_s5[3:4,2]<-c(agree[4:5])
table_s5[4,3]<-c(agree[6])
colnames(table_s5)<-c("Coder 1","Coder 2","Expert 1","Expert 2")
rownames(table_s5)<-c("Coder 1","Coder 2","Expert 1","Expert 2")

table_s5



###
# Reproduce Figure S1

#over-time trend for different coders
out_c1<-aggregate(c1~month,data=statement,mean)
out_c2<-aggregate(c2~month,data=statement,mean)
out_c3<-aggregate(c3~month,data=statement,mean)
out_c4<-aggregate(c4~month,data=statement,mean)

#Construct vector of common agreement among coders (result shown in main article)
com_in<-cbind(statement$c1,statement$c2,statement$c3,statement$c4)	#set up in matrix form
statement$code_total<-apply(com_in,1,FUN=mean) 					#take the average of codes over rows	
out_total<-aggregate(code_total~month,data=statement,mean)			#aggregate result by month


#building Panel A in Figure S1 (proportion)

par(omd=c(0,1,0,1))
plot(out_c1,type='l',xaxt='n',xlab="",ylab="",yaxt='n',ylim=c(0,.6))
	lines(out_c2,lty=2)
	lines(out_c3,lty=3)
	lines(out_c4,lty=4)
	axis(1,1:5,labels=c("Jan","Feb","Mar","Apr","May"))
	axis(2,las=2)
	axis(2,at=seq(0,1,.025),tck=-0.025,labels=NA)
	legend('topleft',lty=c(1:4,1),lwd=c(1,1,1,1,2),legend=c("Coder 1","Coder 2","Expert 1","Expert 2","Average"),bty='n')
	mtext(2,text='Propotion',line=2.5)
	lines(out_total,lwd=2)
	mtext(3,text="A",font=2,cex=3,line=2)


#building Panel B in Figure S1 (raw frequency)

out_c1<-aggregate(c1~month,data=statement,sum)
out_c2<-aggregate(c2~month,data=statement,sum)
out_c3<-aggregate(c3~month,data=statement,sum)
out_c4<-aggregate(c4~month,data=statement,sum)
out_total<-aggregate(code_total~month,data=statement,sum)

plot(out_c1,type='l',xaxt='n',xlab="",ylab="",yaxt='n',ylim=c(0,15))
	lines(out_c2,lty=2)
	lines(out_c3,lty=3)
	lines(out_c4,lty=4)
	axis(1,1:5,labels=c("Jan","Feb","Mar","Apr","May"))
	axis(2,las=2)
	axis(2,at=seq(0,20,1),tck=-0.025,labels=NA)
	legend('topleft',lty=c(1:4,1),lwd=c(1,1,1,1,2),legend=c("Coder 1","Coder 2","Expert 1","Expert 2","Average"),bty='n')
	mtext(2,text='Frequency',line=2.5)
	lines(out_total,lwd=2)
	mtext(3,text="B",font=2,cex=3,line=2)



###
# Reproducing Figure S2

#aggregate text from PM's quote up to two documents based on coding

textAg=c(paste(as.character(statement$text[which(statement$code_total>.5)]),collapse=" "),
	paste(as.character(statement$text[which(statement$code_total<.5)]),collapse=" "))

#build corpus
corp	<-corpus(textAg,language="danish")
corp

#construct design feature matrix (words x documents matrix)
textDfm	<-dfm(corp,language="danish",encoding="UTF-8",stem=FALSE,
	remove=c(stopwords("danish"),"\"","-",",","730"))

textDfm[,colSums(textDfm)>5] # exclude infrequent terms

#calculate the logged risk ratio for each word (using +1 smoothing)
log_ratio <- log2( ((colSums(textDfm[1,])+1)/(sum(textDfm[1,])+1)) / ((colSums(textDfm[2,])+1)/(sum(textDfm[2,])+1)) )

log_ratio_sorted <- sort(log_ratio) #sort words according to logged risk ratio
howmany=15	#how many words (highest and lowest log_odds should be plotted?

w0<-log_ratio_sorted[1:howmany]
w1<-log_ratio_sorted[(length(log_ratio_sorted)-howmany):length(log_ratio_sorted)]
w1	#words most predictive of PM saying budget is a problem
w0	#words most predictive of PM saying budget is not a problem

x=c(w0,w1)	#collapse to one vector (for plotting)

#build figure

par(omd=c(.1,.85,0,1))
plot(x,1:length(x),xaxt='n',yaxt='n',ylab="",xlab="",xlim=c(-5,6))
abline(h=1:length(x),lty=3,col="grey60")
abline(v=0,lty=2)
axis(2,1:length(x),labels=names(x),las=2,cex.axis=.8)
axis(1)
mtext(1,text="Log(Risk Ratio)",line=2)
points(x[1:howmany],1:howmany,pch=21,col="grey70",bg="grey70",cex=1)
points(x[(howmany+1):(2*howmany+1)],(howmany+1):(2*howmany+1),pch=21,col="grey30",bg="grey30",cex=1)
#add english translation
word<-c("enough","china","denmark","between","challenge","new","do","children","without","same","looks","more","just","so","seen","headline","realistic","growth","collect","interest rate","grow","minority government","find","tax","weakest","bill","the more","proposal","faster","the bill","overdraft")
axis(4,1:length(x),labels=word,las=2,cex.axis=.8)




####
# SECTION C: Media Coverage of the Restoration Act

###
#reproducing Figure S3

#load data on search results for different search strings
load('media.RData')

par(mfrow=c(4,3),omd=c(0,1,0,1))
panelnm<-toupper(letters)[1:(ncol(mdat)-1)]
for (i in 1:(ncol(mdat)-1)){
x=mdat[,i+1]
plot(x,type='l',ylab="Number of News Items",xlab="Month",xaxt='n',ylim=c(0,max(x,na.rm=T)+.5*sd(x,na.rm=T)))
mtext(3,text=panelnm[i],font=2,cex=1.25,padj=-.75)
#abline(v=5,lty=2)
axis(1,at=1:nrow(mdat),labels=unique(mdat$month))
points(x,pch=21,bg='black')
}


###
#reproducing Figure S4

#load data on daily search results
load("media_daily.RData")
str(daily_dat)

#build figure
par(omd=c(0,1,0,1),mfrow=c(1,1))
plot(daily_dat[,1],daily_dat[,2],type="l",xlim=c(1,31),xaxt='n',yaxt='n',xlab="Date in May 2010",ylab="Frequency")
	axis(1,at=c(1,seq(5,31,5)))
	axis(1,at=1:31,tck=-.025,labels=NA)
	axis(2,las=2)
	abline(v=19,lty=2)
	text(x=15,y=300,label="Restoration Act\nis proposed",pos=2,cex=.8)
	arrows(15,300,18.5,350,length=.1)
	abline(v=25,lty=2)
	text(pos=4,x=25,y=300,label="The Act is agreed\nupon in parliament",cex=.8)
	lines(daily_dat[,3],lty=4)
par(omd=c(0,1,0,1))
	legend("topleft",legend=c("Full","Restricted"),xpd=T,
	cex=.8,title="Search String:",
	lty=c(1,4))



######
# SECTION D: Panel Attrition and Missingness

#load the five wave panel data in wide format
load("study1_wide.Rdata")
ls()				#check if data loaded correctly
str(paneldat)

##
#results shown in Table S8

#Participation in waves (first column)
c1<-c(table(is.na(paneldat$ui.1))[1],
	table(is.na(paneldat$ui.2))[1],
	table(is.na(paneldat$ui.3))[1],
	table(is.na(paneldat$ui.4))[1],
	table(is.na(paneldat$ui.5))[1])

#participation in waves conditional on participating in all consecutive waves (second column)
c2<-c(table(is.na(paneldat$ui.1))[1],
	table(is.na(paneldat$ui.2[!is.na(paneldat$ui.1)]))[1],
	table(is.na(paneldat$ui.3[!is.na(paneldat$ui.1)&!is.na(paneldat$ui.2)]))[1],
	table(is.na(paneldat$ui.4[!is.na(paneldat$ui.1)&!is.na(paneldat$ui.2)&!is.na(paneldat$ui.3)]))[1],
	table(is.na(paneldat$ui.5[!is.na(paneldat$ui.1)&!is.na(paneldat$ui.2)&!is.na(paneldat$ui.3)&!is.na(paneldat$ui.4)]))[1])

#participation in waves conditional on prior participation and being a opp/gov partisan (third column)
c3<-c(table(is.na(paneldat$ui.1[!is.na(paneldat$partytimeinvar)]))[1],
	table(is.na(paneldat$ui.2[!is.na(paneldat$partytimeinvar)&!is.na(paneldat$ui.1)]))[1],
	table(is.na(paneldat$ui.3[!is.na(paneldat$partytimeinvar)&!is.na(paneldat$ui.1)&!is.na(paneldat$ui.2)]))[1],
	table(is.na(paneldat$ui.4[!is.na(paneldat$partytimeinvar)&!is.na(paneldat$ui.1)&!is.na(paneldat$ui.2)&!is.na(paneldat$ui.3)]))[1],
	table(is.na(paneldat$ui.5[!is.na(paneldat$partytimeinvar)&!is.na(paneldat$ui.1)&!is.na(paneldat$ui.2)&!is.na(paneldat$ui.3)&!is.na(paneldat$ui.4)]))[1])

table_S8<-cbind(1:5,c1,c2,c3)
rownames(table_S8)<-NULL
colnames(table_S8)<-c("Wave","Completed Interviews","Respondents in All Prior Waves","Partisans in All Prior Waves")
table_S8 #note: the table has been edited slightly as it appears in the SI.

#calculation of attrition rate (reported in text)
1206/2902 	#for all respondents
813/2015	#for partisans


###
#reproducing Figure S5
#trend in missingness conditional on PID

#load unbalanced panel data 
load("study1_unbalanced.RData")
str(dat) #check data

dim(dat)
dat<-dat[!is.na(dat$partytimeinvar),] #dropping missing cases on PID variable in wave 1 (incl. those who do not affiliate with the parties in question)
dim(dat)

#generate misssing variable
dat$miss<-NA
dat$miss[is.na(dat$bi)]<-1 #missing on DV includes both D/K answers and dropout after wave 1
dat$miss[!is.na(dat$bi)]<-0

#calculate missingness conditional on PID and time of interview
miss.out<-matrix(NA,nc=3,nr=5)
for(i in 1:5) miss.out[i,]=c(mean(dat$miss[dat$time==i],na.rm=T),mean(dat$miss[dat$time==i&dat$partytimeinvar==0],na.rm=T),mean(dat$miss[dat$time==i&dat$partytimeinvar==1],na.rm=T))
#miss.out


par(mfrow=c(1,1),omd=c(0,1,0,1)) 			#sets plotting region
x<-c(22/28,2+5/30,4+15/31,11+11/31,16+25/30) 	#sets correct spacing on x-axis to match unequal number of days pr month
jit=.1 							#adds space to separate the points in the plot

#build plot
plot(x,miss.out[,2],ylim=c(.1,.6),xaxt='n',xlab="",ylab="Percent Missing",col="white")
	abline(v=3,lty=3)
	lines(x-jit,miss.out[,2],lty=1);points(x-jit,miss.out[,2],pch=21,bg="white")
	lines(x+jit,miss.out[,3],lty=2);points(x+jit,miss.out[,3],pch=21,bg="black")
	axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=1)
	legend('bottomright',legend=c("Government identifiers","Opposition identifiers"),bty='n',pch=21,pt.bg=c("white","black"),lty=1:2)



###
#Reproducing results in Table S9

m1<-lm(miss~partytimeinvar*partycue,data=dat[dat$time%in%2:3,])
	names(m1$coefficients)<-c("Constant","Opposition Identifier (t=1)","Cue","Cue x Opposition Id. (t=1)")
m2<-lm(miss~partytimeinvar*partycue+biinvar,data=dat[dat$time%in%2:3,])
	names(m2$coefficients)<-c("Constant","Opposition Identifier (t=1)","Cue","Deficit (t=1)","Cue x Opposition Id. (t=1)")
m3<-lm(miss~partytimeinvar*partycue+biinvar*partytimeinvar,data=dat[dat$time%in%2:3,])
	names(m3$coefficients)<-c("Constant","Opposition Identifier (t=1)","Cue","Deficit (t=1)","Cue x Opposition Id. (t=1)","Deficit (t=1) x Opp. Id. (t=1)")
m4<-lm(miss~partytimeinvar*partycue*biinvar,data=dat[dat$time%in%2:3,])
	names(m4$coefficients)<-c("Constant","Opposition Identifier (t=1)","Cue","Deficit (t=1)","Cue x Opposition Id.","Deficit (t=1) x Opp. Id. (t=1)","Cue x Deficit (t=1)","Cue x Deficit (t=1) x Opposition Id. (t=1)")

summary(m1) #column 1 in Table S9
nobs(m1)	#number of observations in column 1, Table S9
summary(m2) #column 2 in Table S9
nobs(m2)	#number of observations in column 2, Table S9
summary(m3) #column 3 in Table S9
nobs(m3)	#number of observations in column 3, Table S9
summary(m4) #column 4 in Table S9
nobs(m4)	#number of observations in column 4, Table S9

########
# SECTION E: Partisan Selection into News Outlets

###
#reproducing Table S10 (descriptives on media exposure measure)

#load 
load("study1.RData")

#define names of media outlets
media_names<-c("timer","berlingske","BT","Borsen","EB","Information","KD","JP","METRO","POL","URBAN","local","DR1",
	"TV2","TV2NEWS","DR2","DRupdate")

#calculate distribution on each exposure measure
n=NA
exposure<-matrix(NA,nr=length(media_names),nc=2)
for (i in 1:length(media_names)){
	y1=dat.balance[,media_names[i]]
	x1=dat.balance$partytimeinvar
	exposure[i,]<-aggregate(y1~x1,cbind(y1,x1),mean)$y1
	n[i]=sum(!is.na(dat.balance[!is.na(dat.balance$partytimeinvar)&dat.balance$time==3,media_names[i]]))
}


#assign row and column names
cnames2<-c("24timer","Berlingske Tidende","B.T.","B\\oe rsen","Ekstra bladet","Information","Kristeligt Dagblad",
	"Jyllands-Posten","MetroXpress","Politiken","Urban","Local paper",
	"DR1","TV 2","TV 2 News","DR2","DR Update")
rownames(exposure)<-cnames2
colnames(exposure)<-c("Gov.","Opp.")

exposure_diff<-cbind(exposure,exposure[,1]-exposure[,2],n)  #calculate difference in exposure between partisan groups and insert as 3rd column in table
colnames(exposure_diff)[3]<-"Diff"					#set appropriate column name
exposure_diff_sorted<-exposure_diff[order(exposure_diff[,1],decreasing=T),] #sort matrix

#print table
exposure_diff_sorted[c(1,2,3,7,8,5,6,9,13,17,4,10,11,12,14,15,16),] 


###
# Figure S6

#construct index for total exposure to conservative newspapers
dat.balance$media_CONprint<-NA
dat.balance$media_CONprint<-(dat.balance$berlingske+dat.balance$Borsen+dat.balance$JP)/3
summary(dat.balance$media_CONprint)

#construct index for total exposure to public service broadcasting
dat.balance$media_publicTV<-NA
dat.balance$media_publicTV<-(dat.balance$DR1+dat.balance$TV2+dat.balance$TV2NEWS)/3
#hist(dat.balance$media_publicTV)


#build figure comparing the distribution of news exposure among government identifiers
d=density(dat.balance$media_publicTV[dat.balance$time==3&dat.balance$partytimeinvar==0],bw=.4,na.rm=T,from=0,to=7)
plot(d,main="",ylim=c(0,.8),xlim=c(0,7),xaxt='n',xlab="Average exposure (days/week)",yaxt='n')
polygon(x=c(d$x,rev(d$x)),y=c(d$y,rep(0,length(d$y))),col=rgb(1,1,.5,alpha=.1),border=T)
d=density(dat.balance$media_CONprint[dat.balance$time==3&dat.balance$partytimeinvar==0],bw=.4,na.rm=T,from=0,to=7)
lines(d,lty=2)
polygon(x=c(d$x,rev(d$x)),y=c(d$y,rep(0,length(d$y))),col=rgb(0,.1,.1,alpha=.1),border=T,lty=1)
abline(h=0)
axis(1,at=0:7)
axis(2,las=2)
legend('topright',pch=22,pt.cex=2,pt.bg=c(rgb(1,1,.5,alpha=.1),rgb(0,.1,.1,alpha=.1)),legend=c("Public service broadcasting","Conservative newspapers"),bty='n')


#how many government identifiers never read any of the conservative papers? (commented in text)
table(dat.balance$media_CONprint[dat.balance$time==3&dat.balance$partytimeinvar==0]==0)
72/(177+72)

###
# Reproduce Figure S7

#construct indicator where 1=exposed at least once a week to one conservative newspaper, 0=never exposed
dat.balance$media_anyexp<-recode(dat.balance$media_CONprint,'0=0;0.1:7=1')
table(dat.balance$media_anyexp[dat.balance$time==3&dat.balance$partytimeinvar==0])



#calculate mean perception of budget deficit conditional on time, PID and exposure to conservative newspapers
out<-aggregate(bi~media_anyexp*time*partytimeinvar,data=dat.balance,mean)

SE=function(x)sd(x)/sqrt(length(x)-1)#define simple function for calculating naive SEs
se<-aggregate(bi~media_anyexp*time*partytimeinvar,data=dat.balance,SE)



#build plot
x<-c(22/28,2+5/30,4+15/31,11+11/31,16+25/30) #sets correct spacing on x-axis to match unequal number of days pr month
y=out$bi[out$partytimeinvar==0&out$media_anyexp==0]
plot(x,y,ylim=c(0.45,.7),xaxt='n',main="",xaxt='n',yaxt='n',xlab="",ylab="",pch=21,bg="black")
lines(x,y)
s=se$bi[se$partytimeinvar==0&se$media_anyexp==0]
segments(x,y+1.96*s,x,y-1.96*s)
y=out$bi[out$partytimeinvar==0&out$media_anyexp==1]
jit=.25
points(x+jit,y)
lines(x+jit,y,lty=2)
s=se$bi[se$partytimeinvar==0&se$media_anyexp==1]
segments(x+jit,y+1.96*s,x+jit,y-1.96*s)
legend('bottomright',lty=c(2,1),legend=c("Exposed (n=177 / N=885)","Never exposed (n=72 / N=360)"),bty='n',title="Exposure to conservative newspapers:",cex=.8)
axis(2,las=2)
mtext(2,text="Perception of budget deficit",line=3.5)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=1)

###
# Reproduce results in Table S11

#model 1
fit0<-plm(bi~partycue*media_anyexp
	,data=dat.balance[dat.balance$partytimeinvar==0&dat.balance$time%in%c(2,3),],index=c("id","time"),model="within")
names(fit0$coefficients)<-c("Party Cue","Party Cue x Con. News")
summary(fit0)

#model 2
fit1<-plm(bi~partycue*media_anyexp+monthC
	,data=dat.balance[dat.balance$partytimeinvar==0,],index=c("id","time"),model="within")
names(fit1$coefficients)<-c("Party Cue","Time","Party Cue x Con. News")
summary(fit1)

#model 3
fit2<-plm(bi~partycue*media_anyexp+monthC*media_anyexp
	,data=dat.balance[dat.balance$partytimeinvar==0,],index=c("id","time"),model="within")
names(fit2$coefficients)<-c("Party Cue","Time","Party Cue x Con. News","Time x Con. News")
summary(fit2)


#model 4
fit3<-plm(bi~partycue*media_anyexp+monthC*partycue*media_anyexp
	,data=dat.balance[dat.balance$partytimeinvar==0,],index=c("id","time"),model="within")
names(fit3$coefficients)<-c("Party Cue","Time","Party Cue x Con. News","Time x Party Cue","Time x Con. News","Time x Con. News x Party Cue")
summary(fit3)



########
# SECTION F: Strength of Party Identification as a Moderator

###
# Reproduce Table S12

#overall distribution on strength of party identification for gov. and opp.  party identifiers
within<-table(dat.balance$partytimeinvar[dat.balance$time==1],dat.balance$partystrAll[dat.balance$time==1]) 	#within partisan groups
total<-table(dat.balance$partystrAll[dat.balance$time==1&!is.na(dat.balance$partytimeinvar)]) 			#total

#combine to table
table_s12<-cbind(rbind(within,total),c(rowSums(within),sum(within)))
colnames(table_s12)<-c("Weak","Strong","Total")
rownames(table_s12)<-c("Gov. ID","Opp. ID","Total")
table_s12 #note: text has been edited slightly in the SI


###
# Reproduce Figure S8

#break down trend in perceptions of budget deficit for gov. and opp. ID and strength of ID
out<-aggregate(bi~partytimeinvar*time*partystrAll,data=dat.balance,mean) 

#build plot
par(mfrow=c(1,1))
y=out$bi[out$partytimeinvar==0&out$partystrAll==0]
plot(x,y,ylim=c(0.45,.75),xaxt='n',yaxt='n',ylab=NA,xlab="")
lines(x,y,lty=2)
y=out$bi[out$partytimeinvar==0&out$partystrAll==1]
points(x,y)
lines(x,y,lty=1)
y=out$bi[out$partytimeinvar==1&out$partystrAll==1]
points(x,y,pch=21,bg="black")
lines(x,y,lty=1)
y=out$bi[out$partytimeinvar==1&out$partystrAll==0]
points(x,y,pch=21,bg="black")
lines(x,y,lty=2)
legend('bottomright',lty=c(2,1),legend=c("Weak ID","Strong ID"),bty="n")
legend('topright',pch=c(1,21),legend=c("Government ID","Opposition ID"),bty="n",pt.bg=c("white","black"))
axis(2,las=2)
mtext(2,line=3.5,text="Perception of budget deficit")
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2)


###
# Reproduce results in Table S13
#NOTE: order of variables is not exactly the same as in the SI.

#model 1
fit0<-plm(bi~partycue*partystrAll*partytimeinvar
	,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="pooling") #fits model
names(fit0$coefficients)<-c("Constant","Party Cue","ID Strength","Opposition ID","Party Cue x ID strength","Party Cue x Opposition ID","Opposition ID x ID Strength","Party Cue x Opp. ID x ID Strength")
fit0$vcov<-vcovHC(fit0,cluster="group")						#calculates cluster-robust SEs
summary(fit0)


#model 2
fit1<-plm(bi~partycue*partystrAll*partytimeinvar+monthC
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit1$coefficients)<-c("Constant","Party Cue","ID Strength","Opposition ID","Time","Party Cue x ID strength","Party Cue x Opposition ID","Opposition ID x ID Strength","Party Cue x Opp. ID x ID Strength")
fit1$vcov<-vcovHC(fit1,cluster="group")						#calculates cluster-robust SEs
summary(fit1)

#model 3
fit2<-plm(bi~partycue*partystrAll*partytimeinvar+monthC*partytimeinvar
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit2$coefficients)<-c("Constant","Party Cue","ID Strength","Opposition ID","Time","Party Cue x ID strength","Party Cue x Opposition ID","Opposition ID x ID Strength","Time x Opposition ID","Party Cue x Opp. ID x ID Strength")
fit2$vcov<-vcovHC(fit2,cluster="group")						#calculates cluster-robust SEs
summary(fit2)

#model 4
fit3<-plm(bi~partycue*partystrAll*partytimeinvar+monthC*partytimeinvar*partycue
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit3$coefficients)<-c("Constant","Party Cue","ID Strength","Opposition ID","Time","Party Cue x ID strength","Party Cue x Opposition ID","Opposition ID x ID Strength","Time x Opposition ID","Time x Party Cue","Party Cue x Opp. ID x ID Strength","Time x Party Cue x Opposition ID")
fit3$vcov<-vcovHC(fit3,cluster="group")						#calculates cluster-robust SEs
summary(fit3)



####################
# SECTION G: Aggregate Trends and Trends for all Partisan Groups
#
#NOTE:	Dataverse might be unable to encode the Danish letter "Ø". If there are issues with 
#		running the code below please search and replace the letter. 
#########

#load the unbalanced panel data
load("study1_unbalanced.RData")

###
# Reproduce Table S14

#distribution of respondents on the PID question over time
t2<-table(dat$partyAll[!is.na(dat$bi)],dat$time[!is.na(dat$bi)])
tot<-apply(t2,2,FUN=sum)
grand<-rbind(t2,tot)
order<-c("C","V","DF","LA","RV","Kr","S","SF","Ø","None","Other","Refuse","tot") #note: set order of parties as it appears in the SI (note that some party labels are letters whereas others are abbreviations, fixed below)
grand<-grand[order,] #reorder to match presentation in SI

add_all<-c(table(dat.balance$partyAll[dat.balance$time==1]),sum(table(dat.balance$partyAll[dat.balance$time==1])))
names(add_all)[13]<-"tot"

grand<-cbind(grand,add_all[order]) #add column for all waves (balanced panel)
rownames(grand)<-c("C","V","O","I","B","K","A","F","Ø","None","Other","Refuse","Total") #sets correct party letter
colnames(grand)<-c(paste("Wave",1:5),"All Waves")
grand #note table is rearranged slightly in the SI


###
# Reproduce Figure S9

pnames<-unique(out$partyAll) #get party names for plotting

SE=function(x)sd(x)/sqrt(length(x)-1)
out<-aggregate(bi~partyAll*time,data=dat,mean)
out.se<-aggregate(bi~partyAll*time,data=dat,SE)

x<-c(22/28,2+5/30,4+15/31,11+11/31,16+25/30) #sets correct spacing on x-axis to match unequal number of days pr month

## PANEL A in Figure S9##
par(omd=c(0,1,0,1))
y=out$bi[out$partyAll=="S"]
plot(x,y,ylim=c(.45,.75),xlim=c(-1,18),main="",xaxt="n",yaxt="n",ylab="",xlab="",col="white")
mtext(3,text="A",line=1.5,cex=3.5,font=2)

	#Add all party group trends:
	y=out$bi[out$partyAll=="S"]
	points(x,y,pch=21,bg="black");lines(x,y)
	text(x[1],y[1],pos=2,"A",cex=.8)

	y=out$bi[out$partyAll=="RV"]
	points(x,y,pch=21,bg="black");lines(x,y,lty=3)
	text(x[5],y[5],pos=4,"B",cex=.8)

	y=out$bi[out$partyAll=="Ø"]
	points(x,y,pch=21,bg="black");lines(x,y)
	text(x[1],y[1],pos=2,"Ø",cex=.8)

	y=out$bi[out$partyAll=="SF"]
	points(x,y,pch=21,bg="black");lines(x,y)
	text(x[1],y[1]-.01,pos=2,"F",cex=.8)

	y=out$bi[out$partyAll=="V"]
	points(x,y);lines(x,y,lty=2)
	text(x[1],y[1],pos=2,"V",cex=.8)

	y=out$bi[out$partyAll=="C"]
	points(x,y);lines(x,y,lty=2)
	text(x[1],y[1],pos=2,"C",cex=.8)

	#too few cases for LA
	#y=out$bi[out$partyAll=="LA"]
	#points(x,y);lines(x,y,lty=2)
	#text(x[1],y[1],pos=2,"C",cex=.8)

	y=out$bi[out$partyAll=="None"]
	points(x,y,pch=21,bg="grey80");lines(x,y,lty=3)
	text(x[5],y[5],pos=4,"None",cex=.8,xpd=T)

	y=out$bi[out$partyAll=="DF"]
	points(x,y);lines(x,y,lty=2)
	text(x[1],y[1],pos=2,"O",cex=.8)

axis(2,las=2)
mtext(2,text="Perception of budget deficit",line=3)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=.8)


## PANEL B ##
out<-aggregate(bi~time,data=dat,mean) #get the aggregate pattern
out.se<-aggregate(bi~time,data=dat,SE)

#build plot
y=out$bi
plot(x,y,ylim=c(.45,.75),xlim=c(0,18),main="",xaxt="n",yaxt="n",ylab="",xlab="",col="black")
mtext(3,text="B",line=1.5,cex=3.5,font=2)

segments(x,y-2*out.se$bi,x,y+2*out.se$bi)
lines(x,y)
points(x,y,pch=21,bg="grey80")

axis(2,las=2)
mtext(2,text="Perception of budget deficit",line=3)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=.8)



###
# Reproduce Figure S10
# (same analysis but on the balanced panel)

out<-aggregate(bi~partyAll*time,data=dat.balance,mean)
out.se<-aggregate(bi~partyAll*time,data=dat.balance,SE)

##PANEL A##
x<-c(22/28,2+5/30,4+15/31,11+11/31,16+25/30) #sets correct spacing on x-axis to match unequal number of days pr month

par(omd=c(0,1,0,1))
y=out$bi[out$partyAll=="S"]

plot(x,y,ylim=c(.45,.75),xlim=c(-1,18),main="",xaxt="n",yaxt="n",ylab="",xlab="",col="white")
mtext(3,text="A",line=1.5,cex=3.5,font=2)

	#add partisan group trends
	y=out$bi[out$partyAll=="S"]
	points(x,y,pch=21,bg="black");lines(x,y)
	text(x[1],y[1],pos=2,"A",cex=.8)

	y=out$bi[out$partyAll=="RV"]
	points(x,y,pch=21,bg="black");lines(x,y,lty=3)
	text(x[5],y[5],pos=4,"B",cex=.8)

	y=out$bi[out$partyAll=="Ø"]
	points(x,y,pch=21,bg="black");lines(x,y)
	text(x[1],y[1],pos=2,"Ø",cex=.8)

	y=out$bi[out$partyAll=="SF"]
	points(x,y,pch=21,bg="black");lines(x,y)
	text(x[1],y[1]-.01,pos=2,"F",cex=.8)

	y=out$bi[out$partyAll=="V"]
	points(x,y);lines(x,y,lty=2)
	text(x[1],y[1],pos=2,"V",cex=.8)

	y=out$bi[out$partyAll=="C"]
	points(x,y);lines(x,y,lty=2)
	text(x[1],y[1],pos=2,"C",cex=.8)

	#too few cases for LA
	#y=out$bi[out$partyAll=="LA"]
	#points(x,y);lines(x,y,lty=2)
	#text(x[1],y[1],pos=2,"C",cex=.8)

	y=out$bi[out$partyAll=="None"]
	points(x,y,pch=21,bg="grey80");lines(x,y,lty=3)
	text(x[5],y[5],pos=4,"None",cex=.8,xpd=T)

	y=out$bi[out$partyAll=="DF"]
	points(x,y);lines(x,y,lty=2)
	text(x[1],y[1],pos=2,"O",cex=.8)

axis(2,las=2)
mtext(2,text="Perception of budget deficit",line=3)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=.8)

### PANEL B###
out<-aggregate(bi~time,data=dat.balance,mean) #calculate aggregate pattern (across all respondents)
out.se<-aggregate(bi~time,data=dat.balance,SE)

#build plot
y=out$bi
plot(x,y,ylim=c(.45,.75),xlim=c(0,18),main="",xaxt="n",yaxt="n",ylab="",xlab="",col="black")
mtext(3,text="B",line=1.5,cex=3.5,font=2)
segments(x,y-2*out.se$bi,x,y+2*out.se$bi)
lines(x,y)
points(x,y,pch=21,bg="grey80")
axis(2,las=2)
mtext(2,text="Perception of budget deficit",line=3)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=.8)


###
# SECTION H: Full Model Output for Table 1

dat.balance$income<-relevel(factor(dat.balance$income),ref='100.000 - 199.999 kr.') #same reference category for "income" as reported in the article


#reproduce Table S15
fit0<-plm(bi~income+educationClean+sex+age+
	partytimeinvar*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time")
	,model="pooling")

#set correct variabel names
names(fit0$coefficients)<-c("Constant","Income: 200.000-299.999 DKK","Income: 300.000-399.999 DKK",
	"Income: 400.000-499.999 DKK","Income: 500.000-599.999 DKK","Income: 600.000-699.999 DKK","Income: 700.000 or above",
	"Income: Up to 99.999 DKK","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Cue x Opposition Id.")

#calculate and insert cluster-robust SEs in model object
fit0$vcov<-vcovHC(fit0,cluster="group") 

#print model results
summary(fit0)

#stargazer(fit0,
#	title="The conditional effect of party cues on partisans' perceptions of the budget deficit. Unstandardized OLS regression estimates.",
#	label="study1tab",
#	no.space=T,star.cutoffs=c(.05,.01,.001),
#	table.placement="H")

#####
# SECTION I: Panel Models Using Perceptions of Unemployment (Placebo Outcome) as the Dependent Variable
#####
#Reproduce Table S16

#model 1
fit4<-plm(ui~income+educationClean+sex+age+
	partytimeinvar*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time")
	,model="pooling") #fits model
names(fit4$coefficients)<-c("Constant","Income: 200.000-299.999 DKK","Income: 300.000-399.999 DKK",
	"Income: 400.000-499.999 DKK","Income: 500.000-599.999 DKK","Income: 600.000-699.999 DKK","Income: 700.000 or above",
	"Income: Up to 99.999 DKK","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Cue x Opposition Id.")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)

#model 2
fit5<-plm(ui~income+educationClean+sex+age	
	+partytimeinvar*partycue+monthC
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit5$coefficients)<-c("Constant","Income: 200.000-299.999 DKK","Income: 300.000-399.999 DKK",
	"Income: 400.000-499.999 DKK","Income: 500.000-599.999 DKK","Income: 600.000-699.999 DKK","Income: 700.000 or above",
	"Income: Up to 99.999 DKK","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.")
fit5$vcov<-vcovHC(fit5,cluster="group") #gets cluster-robust SEs
summary(fit5)

#model 3
fit6<-plm(ui~income+educationClean+sex+age	
	+partytimeinvar*partycue+partytimeinvar*monthC
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit6$coefficients)<-c("Constant","Income: 200.000-299.999 DKK","Income: 300.000-399.999 DKK",
	"Income: 400.000-499.999 DKK","Income: 500.000-599.999 DKK","Income: 600.000-699.999 DKK","Income: 700.000 or above",
	"Income: Up to 99.999 DKK","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.")
fit6$vcov<-vcovHC(fit6,cluster="group") #gets cluster-robust SEs
summary(fit6)

#model 4
fit7<-plm(ui~income+educationClean+sex+age	
	+partytimeinvar*partycue*monthC
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit7$coefficients)<-c("Constant","Income: 200.000-299.999 DKK","Income: 300.000-399.999 DKK",
	"Income: 400.000-499.999 DKK","Income: 500.000-599.999 DKK","Income: 600.000-699.999 DKK","Income: 700.000 or above",
	"Income: Up to 99.999 DKK","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.","Time x Cue","Time x Cue x Opp. Id.")
fit7$vcov<-vcovHC(fit7,cluster="group") #gets cluster-robust SEs
summary(fit7)

###
# SECTION J: Robustness to Different Coding Strategies and Data Characteristics
###


###
# Reproduces Figure S11

#Balanced panel / vote intention
biMean=matrix(NA,nc=2,nr=5)
biSE=matrix(NA,nc=2,nr=5)
for(t in 1:5){
biMean[t,]=c(mean(dat.balance$bi[dat.balance$time==t&dat.balance$voteall==0],na.rm=T),mean(dat.balance$bi[dat.balance$time==t&dat.balance$voteall==1],na.rm=T))
biSE[t,]=c(sd(dat.balance$bi[dat.balance$time==t&dat.balance$voteall==0],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$voteall==0])),sd(dat.balance$bi[dat.balance$time==t&dat.balance$voteall==1],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$voteall==1])))
}
biMean
biSE

#unbalanced panel / vote intention
biMeanUNB=matrix(NA,nc=2,nr=5)
biSEUNB=matrix(NA,nc=2,nr=5)
for(t in 1:5){
biMeanUNB[t,]=c(mean(dat$bi[dat$time==t&dat$voteall==0],na.rm=T),mean(dat$bi[dat$time==t&dat$voteall==1],na.rm=T))
biSEUNB[t,]=c(sd(dat$bi[dat$time==t&dat$voteall==0],na.rm=T)/sqrt(length(dat$bi[dat$time==t&dat$voteall==0])),sd(dat$bi[dat$time==t&dat$voteall==1],na.rm=T)/sqrt(length(dat$bi[dat$time==t&dat$voteall==1])))
}
biMeanUNB
biSEUNB

#unbalanced panel / PID
biMeanUNBPID=matrix(NA,nc=2,nr=5)
biSEUNBPID=matrix(NA,nc=2,nr=5)
for(t in 1:5){
biMeanUNBPID[t,]=c(mean(dat$bi[dat$time==t&dat$partytimeinvar==0],na.rm=T),mean(dat$bi[dat$time==t&dat$partytimeinvar==1],na.rm=T))
biSEUNBPID[t,]=c(sd(dat$bi[dat$time==t&dat$partytimeinvar==0],na.rm=T)/sqrt(length(dat$bi[dat$time==t&dat$partytimeinvar==0])),sd(dat$bi[dat$time==t&dat$partytimeinvar==1],na.rm=T)/sqrt(length(dat$bi[dat$time==t&dat$partytimeinvar==1])))
}
biMeanUNBPID
biSEUNBPID


#balanced panel / PID
biMeanPID=matrix(NA,nc=2,nr=5)
biSEPID=matrix(NA,nc=2,nr=5)
for(t in 1:5){
biMeanPID[t,]=c(mean(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T),mean(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T))
biSEPID[t,]=c(sd(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0])),sd(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1])))
}
biMeanPID
biSEPID

#plotting#

par(mfrow=c(1,1),omd=c(0,1,0,1))
x<-c(22/28,2+5/30,4+15/31,11+11/31,16+25/30) #sets correct spacing on x-axis to match unequal number of days pr month
tt=.9 #controls text size
pp=1 #controls point size
jit=.1 #adds space to separate the points in the plot

plot(x,biMean[,1],ylim=c(.5,.7),xaxt='n',yaxt='n',ylab=NA,xlab=NA)
lines(x,biMean[,1])
points(x+jit,biMean[,2],pch=21,bg="black",cex=pp);lines(x+jit,biMean[,2])

lines(x+jit,biMeanUNB[,1],lty=2);points(x+jit,biMeanUNB[,1])
lines(x+jit,biMeanUNBPID[,1],lty=3);points(x+jit,biMeanUNBPID[,1])
lines(x+jit,biMeanPID[,1],lty=4);points(x+jit,biMeanPID[,1])

lines(x,biMeanUNB[,2],lty=2);points(x,biMeanUNB[,2],pch=21,bg='black')
lines(x,biMeanUNBPID[,2],lty=3);points(x,biMeanUNBPID[,2],pch=21,bg='black')
lines(x,biMeanPID[,2],lty=4);points(x,biMeanPID[,2],pch=21,bg='black')

abline(v=3+19/31,lty=2)
arrows(5.1,.675,3+19/31,.65,length=.1)
text(5,.68,"Restoration Act",pos=4,cex=.9)

axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=tt)
#axis(1,at=11,label=2011,padj=-1,cex.axis=tt,las=2)
axis(2,las=2,cex.axis=tt)
mtext(2,text='Perception of budget deficit',padj=-5,cex=tt)
legend('topright',legend=c('Government Identifier','Opposition Identifier'),cex=tt,bty='n',pch=21,pt.bg=c('white','black'))
legend('bottomright',legend=c("Balanced Panel / Vote intention","Unbalanced Panel / Vote intention","Unbalanced Panel / Party Identification","Balanced Panel / Party Identification"),bty='n',cex=.8,lty=1:4)


###
# Reproduce Table S17
# Model results for: Party identification / Unbalanced Panel

#Model (1): two-period pre/post
fit1<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue,data=dat[dat$time%in%c(2,3),],index=c("id","time"),model="pooling")
names(fit1$coefficients)<-c("Constant","Income: 200.000-299.999 DKK","Income: 300.000-399.999 DKK",
	"Income: 400.000-499.999 DKK","Income: 500.000-599.999 DKK","Income: 600.000-699.999 DKK","Income: 700.000 or above",
	"Income: Up to 99.999 DKK","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Cue x Opposition Id.")
fit1$vcov<-vcovHC(fit1,cluster="group") #gets cluster-robust SEs
summary(fit1)


#Model (2): five wave common slope
fit2<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC,data=dat,index=c("id","time"),model="pooling")
names(fit2$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.")
fit2$vcov<-vcovHC(fit2,cluster="group") #gets cluster-robust SEs
summary(fit2)

#Model (3): Allowing for differential trends for partisan groups
fit3<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC
	,data=dat,index=c("id","time"),model="pooling")
names(fit3$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.")
fit3$vcov<-vcovHC(fit3,cluster="group") #gets cluster-robust SEs
summary(fit3)

#Model (4): Allowing for differential pre/post trends for partisan groups
fit4<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC*partycue
	,data=dat,index=c("id","time"),model="pooling")
names(fit4$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.","Time x Cue","Time x Cue x Opp. Id.")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)


###
# Reproduce Table S18
# Vote Intention / Unbalanced Panel #

#Model (1): two-period pre/post
fit1<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue,data=dat[dat$time%in%c(2,3),],index=c("id","time"),model="pooling")
names(fit1$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Cue x Opposition vote")
fit1$vcov<-vcovHC(fit1,cluster="group") #gets cluster-robust SEs
summary(fit1)


#Model (2): five wave common slope
fit2<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue+monthC,data=dat,index=c("id","time"),model="pooling")
names(fit2$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Time","Cue x Opposition vote")
fit2$vcov<-vcovHC(fit2,cluster="group") #gets cluster-robust SEs
summary(fit2)

#Model (3): Allowing for differential trends for partisan groups
fit3<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue+monthC+voteall*monthC
	,data=dat,index=c("id","time"),model="pooling")
names(fit3$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Time","Cue x Opposition vote","Time x Opposition vote")
fit3$vcov<-vcovHC(fit3,cluster="group") #gets cluster-robust SEs
summary(fit3)

#Model (4): Allowing for differential pre/post trends for partisan groups
fit4<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue+monthC+voteall*monthC*partycue
	,data=dat,index=c("id","time"),model="pooling")
names(fit4$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Time","Cue x Opposition vote","Time x Opposition vote","Time x Cue","Time x Cue x Opp. vote")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)





###
# Reproduce Table S19
# vote intention / balanced Panel

#Model (1): two-period pre/post
fit1<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="pooling")
names(fit1$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Cue x Opposition vote")
fit1$vcov<-vcovHC(fit1,cluster="group") #gets cluster-robust SEs
summary(fit1)


#Model (2): five wave common slope
fit2<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue+monthC,data=dat.balance,index=c("id","time"),model="pooling")
names(fit2$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Time","Cue x Opposition vote")
fit2$vcov<-vcovHC(fit2,cluster="group") #gets cluster-robust SEs
summary(fit2)

#Model (3): Allowing for differential trends for partisan groups
fit3<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue+monthC+voteall*monthC
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit3$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Time","Cue x Opposition vote","Time x Opposition vote")
fit3$vcov<-vcovHC(fit3,cluster="group") #gets cluster-robust SEs
summary(fit3)

#Model (4): Allowing for differential pre/post trends for partisan groups
fit4<-plm(bi~income+educationClean+sex+age+	
	voteall*partycue+monthC+voteall*monthC*partycue
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit4$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition vote","Cue","Time","Cue x Opposition vote","Time x Opposition vote","Time x Cue","Time x Cue x Opp. vote")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)



###
# SECTION K: Pooled OLS, Fixed Effects, and Random Effects Models Return Identical Results
###

###
# Reproduce Table S20
# NOTE: For presentational reasons, the results have been rearranged and simplified 
# in Table S20. The code below returns all the estimates from the models. 


#POOLED OLS

#Model (1): two-period pre/post
fit1<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="pooling")
names(fit1$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Cue x Opposition Id.")
fit1$vcov<-vcovHC(fit1,cluster="group") #gets cluster-robust SEs
summary(fit1)


#Model (2): five wave common slope
fit2<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC,data=dat.balance,index=c("id","time"),model="pooling")
names(fit2$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.")
fit2$vcov<-vcovHC(fit2,cluster="group") #gets cluster-robust SEs
summary(fit2)

#Model (3): Allowing for differential trends for partisan groups
fit3<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit3$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.")
fit3$vcov<-vcovHC(fit3,cluster="group") #gets cluster-robust SEs
summary(fit3)

#Model (4): Allowing for differential pre/post trends for partisan groups
fit4<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC*partycue
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit4$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.","Time x Cue","Time x Cue x Opp. Id.")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)



#RANDOM EFFECTS

#Model (1): two-period pre/post
fit1<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="random")
names(fit1$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Cue x Opposition Id.")
fit1$vcov<-vcovHC(fit1,cluster="group") #gets cluster-robust SEs
summary(fit1)


#Model (2): five wave common slope
fit2<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC,data=dat.balance,index=c("id","time"),model="random")
names(fit2$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.")
fit2$vcov<-vcovHC(fit2,cluster="group") #gets cluster-robust SEs
summary(fit2)

#Model (3): Allowing for differential trends for partisan groups
fit3<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC
	,data=dat.balance,index=c("id","time"),model="random")
names(fit3$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.")
fit3$vcov<-vcovHC(fit3,cluster="group") #gets cluster-robust SEs
summary(fit3)

#Model (4): Allowing for differential pre/post trends for partisan groups
fit4<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC*partycue
	,data=dat.balance,index=c("id","time"),model="random")
names(fit4$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.","Time x Cue","Time x Cue x Opp. Id.")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)



#FIXED EFFECTS

#Model (1): two-period pre/post
fit1<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="within")
names(fit1$coefficients)<-c("Cue","Cue x Opposition Id.")
fit1$vcov<-vcovHC(fit1,cluster="group") #gets cluster-robust SEs
summary(fit1)


#Model (2): five wave common slope
fit2<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC,data=dat.balance,index=c("id","time"),model="within")
names(fit2$coefficients)<-c("Cue","Time","Cue x Opposition Id.")
fit2$vcov<-vcovHC(fit2,cluster="group") #gets cluster-robust SEs
summary(fit2)

#Model (3): Allowing for differential trends for partisan groups
fit3<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC
	,data=dat.balance,index=c("id","time"),model="within")
names(fit3$coefficients)<-c("Cue","Time","Cue x Opposition Id.","Time x Opposition Id.")
fit3$vcov<-vcovHC(fit3,cluster="group") #gets cluster-robust SEs
summary(fit3)

#Model (4): Allowing for differential pre/post trends for partisan groups
fit4<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC*partycue
	,data=dat.balance,index=c("id","time"),model="within")
names(fit4$coefficients)<-c("Cue","Time","Cue x Opposition Id.","Time x Opposition Id.","Time x Cue","Time x Cue x Opp. Id.")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)



###
# SECTION L: Correcting for Time Trends
###

#Table S21
#The models below give the full model output with estimates for control variables

#Model (1): two-period pre/post
fit1<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="pooling")
names(fit1$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Cue x Opposition Id.")
fit1$vcov<-vcovHC(fit1,cluster="group") #gets cluster-robust SEs
summary(fit1)


#Model (2): five wave common slope
fit2<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC,data=dat.balance,index=c("id","time"),model="pooling")
names(fit2$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.")
fit2$vcov<-vcovHC(fit2,cluster="group") #gets cluster-robust SEs
summary(fit2)

#Model (3): Allowing for differential trends for partisan groups
fit3<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit3$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.")
fit3$vcov<-vcovHC(fit3,cluster="group") #gets cluster-robust SEs
summary(fit3)

#Model (4): Allowing for differential pre/post trends for partisan groups
fit4<-plm(bi~income+educationClean+sex+age+	
	partytimeinvar*partycue+monthC+partytimeinvar*monthC*partycue
	,data=dat.balance,index=c("id","time"),model="pooling")
names(fit4$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Time","Cue x Opposition Id.","Time x Opposition Id.","Time x Cue","Time x Cue x Opp. Id.")
fit4$vcov<-vcovHC(fit4,cluster="group") #gets cluster-robust SEs
summary(fit4)



###
# SECTION M: Placebo Test I: Perceptions of Unemployment
###



#calculate simple means for unemployment question conditional on PID (same procedure as Figure 2)
uiMean=matrix(NA,nc=2,nr=5)
uiSE=matrix(NA,nc=2,nr=5)
for(t in 1:5){
uiMean[t,]=c(mean(dat.balance$ui[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T),mean(dat.balance$ui[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T))
uiSE[t,]=c(sd(dat.balance$ui[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T)/sqrt(length(dat.balance$ui[dat.balance$time==t&dat.balance$partytimeinvar==0])),sd(dat.balance$ui[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T)/sqrt(length(dat.balance$ui[dat.balance$time==t&dat.balance$partytimeinvar==1])))
}
uiMean
uiSE

#calculate means for perceptions of budget deficit
biMean=matrix(NA,nc=2,nr=5)
biSE=matrix(NA,nc=2,nr=5)
for(t in 1:5){
biMean[t,]=c(mean(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T),mean(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T))
biSE[t,]=c(sd(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0])),sd(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1])))
}




par(mfrow=c(1,2),omd=c(0.1,1,0,1)) #set plotting region

#build plot
#PANEL A#
tt=.9 #controls text size
pp=1 #controls point size
jit=.1 #adds space to separate the points in the plot
plot(x,biMean[,1],ylim=c(.45,.7),xaxt='n',yaxt='n',ylab=NA,xlab=NA)
lines(x,biMean[,1])
points(x+jit,biMean[,2],pch=21,bg="black",cex=pp);lines(x+jit,biMean[,2])
segments(x,biMean[,1],x,biMean[,1]+2*biSE[,1]);segments(x,biMean[,1],x,biMean[,1]-2*biSE[,1])
segments(x+jit,biMean[,2],x+jit,biMean[,2]+2*biSE[,2]);segments(x+jit,biMean[,2],x+jit,biMean[,2]-2*biSE[,2])
points(x,biMean[,1],pch=21,bg="white",cex=pp)
abline(v=3.633,lty=2)
arrows(5.1,.675,3.7,.65,length=.1)
text(5,.68,"Restoration Act",pos=4,cex=.9)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=tt)
axis(2,las=2,cex.axis=tt)
axis(2,at=seq(0,1,.01),labels=NA,tck=-0.015)
mtext(2,text='Perception of Budget Deficit',padj=-5,cex=tt)
legend('bottomright',legend=c('Government identifiers','Opposition identifiers'),cex=tt,bty='n',pch=21,pt.bg=c('white','black'))
mtext(3,text="Treatment: Budget Deficit",font=1,padj=-1,cex=1.5)

#PANEL B#
par(omd=c(0,.9,0,1),new=T)
plot(x,uiMean[,1],ylim=c(.45,.7),xaxt='n',yaxt='n',ylab=NA,xlab=NA)
lines(x,uiMean[,1])
points(x+jit,uiMean[,2],pch=21,bg="black",cex=pp);lines(x+jit,uiMean[,2])
segments(x,uiMean[,1],x,uiMean[,1]+2*uiSE[,1]);segments(x,uiMean[,1],x,uiMean[,1]-2*uiSE[,1])
segments(x+jit,uiMean[,2],x+jit,uiMean[,2]+2*uiSE[,2]);segments(x+jit,uiMean[,2],x+jit,uiMean[,2]-2*uiSE[,2])
points(x,uiMean[,1],pch=21,bg="white",cex=pp)
abline(v=3.633,lty=2)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=tt)
axis(4,las=2,cex.axis=tt)
mtext(4,text='Perception of Unemployment',padj=5,cex=tt)
axis(4,at=seq(0,1,.01),labels=NA,tck=-0.015)
mtext(3,text="Placebo: Unemployment",font=1,padj=-1,cex=1.5)





###
# SECTION N: Placebo test II: Factual Beliefs About the Deficit
###


###
# Reproduce Figure S13

#Calculate group means over time.
bbMean=matrix(NA,nc=2,nr=5)
bbSE=matrix(NA,nc=2,nr=5)
for(t in 1:5){
bbMean[t,]=c(mean(dat.balance$bb[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T),mean(dat.balance$bb[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T))
bbSE[t,]=c(sd(dat.balance$bb[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T)/sqrt(length(dat.balance$bb[dat.balance$time==t&dat.balance$partytimeinvar==0])),sd(dat.balance$bb[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T)/sqrt(length(dat.balance$bb[dat.balance$time==t&dat.balance$partytimeinvar==1])))
}


#build plot
x<-c(22/28,2+5/30,4+15/31,11+11/31,16+25/30) #sets correct spacing on x-axis to match unequal number of days pr month
tt=.9 #controls text size
pp=1 #controls point size
jit=.1 #adds space to separate the points in the plot

par(mfrow=c(1,1),omd=c(0,1,0,1))
plot(x,bbMean[,1],ylim=c(.2,.8),xaxt='n',yaxt='n',ylab=NA,xlab=NA,yaxt='n')
lines(x,bbMean[,1])
points(x+jit,bbMean[,2],pch=21,bg="black",cex=pp);lines(x+jit,bbMean[,2])
segments(x,bbMean[,1],x,bbMean[,1]+2*bbSE[,1]);segments(x,bbMean[,1],x,bbMean[,1]-2*bbSE[,1])
segments(x+jit,bbMean[,2],x+jit,bbMean[,2]+2*bbSE[,2]);segments(x+jit,bbMean[,2],x+jit,bbMean[,2]-2*bbSE[,2])
points(x,bbMean[,1],pch=21,bg="white",cex=pp)
abline(v=3+19/31,lty=2)
arrows(5.1,.675,3.75,.65,length=.1)
text(5,.68,"Restoration Act",pos=4,cex=.9)
axis(2,at=sort(unique(dat.balance$bb)),label=c(">25","37","62","87","112","137",">150"),las=2)
mtext(2,text="Billion DKR",padj=-4)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=tt)
legend('bottomright',legend=c('Government Identifier','Opposition Identifier'),cex=tt,bty='n',pch=21,pt.bg=c('white','black'))


###
# Reproduce Table S22
# correlation between factual beliefs and interpretations, wave 1

#bb = factual beliefs question
#fit models appearing in each column in the upper part of Table S21

#column 1 (two first rows)
fit0<-lm(bi~bb,data=dat.balance[dat.balance$time==1 & !is.na(dat.balance$partytimeinvar),])
names(fit0$coefficients)<-c("Constant","Size of Deficit (0-1)")
summary(fit0)
nobs(fit0) #n

#column 2 (two first rows)
fit1<-lm(bi~bb,data=dat.balance[dat.balance$time==1 & dat.balance$partytimeinvar==0,])
names(fit1$coefficients)<-c("Constant","Size of Deficit (0-1)")
summary(fit1)
nobs(fit1) #n

#column 3 (two first rows)
fit2<-lm(bi~bb,data=dat.balance[dat.balance$time==1 & dat.balance$partytimeinvar==1,])
names(fit2$coefficients)<-c("Constant","Size of Deficit (0-1)")
summary(fit2)
nobs(fit2) #n

#indicator for Resp ID that are missing in the data (used to limit construction of effect sizes to effective sample)
us<-unique(dat.balance$id[is.na(dat.balance$bb)|is.na(dat.balance$bi)|is.na(dat.balance$partytimeinvar)])
us.out<-which(dat.balance$id%in%us)

#scaling effects
lower<-quantile(dat.balance$bb[-us.out],na.rm=T,probs=c(.25,.05,.01))
upper<-quantile(dat.balance$bb[-us.out],na.rm=T,probs=c(.75,0.95,.99))
scale=upper-lower

m1<-round(coef(fit0)['Size of Deficit (0-1)']*scale,digits=2)
m2<-round(coef(fit1)['Size of Deficit (0-1)']*scale,digits=2)
m3<-round(coef(fit2)['Size of Deficit (0-1)']*scale,digits=2)
nm<-c("25-75p","5-95p","1-99p")
out<-cbind(m1,m2,m3)
rownames(out)<-nm

#print effect size calculation in Table S22
out


###
# SECTION O: Covariates are Balanced Across Treatment Conditions (Study 2)
###

#load data from study 2
load("study2.RData")

#cleaning variable tapping device type (no cases for the two values "Skipped" and "Not Asked")
expdat$mobile_device<-recode(as.character(expdat$mobile_device),'"Skipped"=NA;"Not Asked"=NA') 


###
# Reproducing Figure S14
# Covariate balance for budget experiment

par(mfrow=c(2,3))
barplot(table(expdat$education,expdat$Utreat),xaxt="n",beside=T,main="Education")
axis(1,at=c(4,15),labels=c("Gov:Noproblem","Gov:Problem"),xpd=T,tick=F)
barplot(table(expdat$region,expdat$Utreat),xaxt="n",beside=T,main="Region")
axis(1,at=c(3,10),labels=c("Gov:Noproblem","Gov:Problem"),xpd=T,tick=F)
barplot(table(expdat$gender,expdat$Utreat),xaxt="n",beside=T,main="Gender")
axis(1,at=c(2,5),labels=c("Gov:Noproblem","Gov:Problem"),xpd=T,tick=F)

barplot(table(expdat$mobile_device,expdat$Utreat),xaxt="n",beside=T,main="Device Type (Mobile/PC)")
axis(1,at=c(2,5),labels=c("Gov:Noproblem","Gov:problem"),xpd=T,tick=F)

plot(density(expdat$tot_time[expdat$tot_time<600&expdat$Utreat=="noproblem"],na.rm=T,bw=13),main="Total Response Time (secs)",xlab="")
lines(density(expdat$tot_time[expdat$tot_time<600&expdat$Utreat=="problem"],na.rm=T,bw=13),lty=2)
legend('topright',bty='n',lty=1:2,legend=c("Gov:Noproblem","Gov:Problem"))

plot(density(expdat$age[expdat$Utreat=="noproblem"],na.rm=T,bw=4),main="Age",xlab="",ylim=c(0,0.03))
lines(density(expdat$age[expdat$Utreat=="problem"],na.rm=T,bw=4),lty=2)

###
# Reproducing Figure S15
# Covariate balance for unemployment experiment

par(mfrow=c(2,3))
barplot(table(expdat$education,expdat$Utreat),xaxt="n",beside=T,main="Education")
axis(1,at=c(4,15),labels=c("Gov:Noproblem","Gov:Problem"),xpd=T,tick=F)
barplot(table(expdat$region,expdat$Utreat),xaxt="n",beside=T,main="Region")
axis(1,at=c(3,10),labels=c("Gov:Noproblem","Gov:Problem"),xpd=T,tick=F)
barplot(table(expdat$gender,expdat$Utreat),xaxt="n",beside=T,main="Gender")
axis(1,at=c(2,5),labels=c("Gov:Noproblem","Gov:Problem"),xpd=T,tick=F)
barplot(table(expdat$mobile_device,expdat$Utreat),xaxt="n",beside=T,main="Device Type (Mobile/PC)")
axis(1,at=c(2,5),labels=c("Gov:Noproblem","Gov:problem"),xpd=T,tick=F)
plot(density(expdat$tot_time[expdat$tot_time<600&expdat$Utreat=="noproblem"],na.rm=T,bw=13),main="Total Response Time (secs)",xlab="")
lines(density(expdat$tot_time[expdat$tot_time<600&expdat$Utreat=="problem"],na.rm=T,bw=13),lty=2)
legend('topright',bty='n',lty=1:2,legend=c("Gov:Noproblem","Gov:Problem"))
plot(density(expdat$age[expdat$Utreat=="noproblem"],na.rm=T,bw=4),main="Age",xlab="",ylim=c(0,0.03))
lines(density(expdat$age[expdat$Utreat=="problem"],na.rm=T,bw=4),lty=2)


###
# Reproducing Table S23
# Model based test of covariate balance


#predicting assignment to unemployment treatment
balancefit0B<-glm(factor(expdat$Utreat)~1,data=expdat,family=binomial('logit'))
balancefitB<-glm(factor(expdat$Utreat)~education+age+region+gender+tot_time+mobile_device,data=expdat,family=binomial('logit'))
summary(balancefitB)
lrtest(balancefit0B,balancefitB)
compareB<-lrtest(balancefit0B,balancefitB)
compareB

#predicting assignment to budget treatment
balancefit0A<-glm(factor(expdat$Btreat)~1,data=expdat,family=binomial('logit')) #null model
balancefitA<-glm(factor(expdat$Btreat)~education+age+region+gender+tot_time+mobile_device,data=expdat,family=binomial('logit'))
summary(balancefitA)
compareU<-lrtest(balancefit0A,balancefitA)
compareU


###
# SECTION P: Dropout is not Related to Treatment Assignment (Study 2)
###

#construct variable tapping missingness
expdat$miss=NA
expdat$miss[is.na(expdat$budgetint)|is.na(expdat$unempint)]=1
expdat$miss[!is.na(expdat$budgetint)|!is.na(expdat$unempint)]=0

#dropout conditional on PID (result reported in text)
table(expdat$miss[expdat$VK%in%0:1])

###
# Reproducing results in Table S24

fit0=glm(miss~Btreat+Utreat,data=expdat,family=binomial(link="probit"))
names(fit0$coefficients)<-c("Constant","Budget Cue","Unemployment Cue")
fit1=glm(miss~Btreat+Utreat+VK,data=expdat,family=binomial(link="probit"))
names(fit1$coefficients)<-c("Constant","Budget Cue","Unemployment Cue","Opposition Identifier")
fit2=glm(miss~Btreat*VK+Utreat*VK,data=expdat,family=binomial(link="probit"))
names(fit2$coefficients)<-c("Constant","Budget Cue","Opposition Identifier","Unemployment Cue",
	"Budget Cue x Opposition Id.","Unemployment Cue x Opposition Id.")

#print models
summary(fit0) 
	nobs(fit0)
	logLik(fit0)
	AIC(fit0)
summary(fit1)
	nobs(fit1)
	logLik(fit1)
	AIC(fit1)
summary(fit2)
	nobs(fit2)
	logLik(fit2)
	AIC(fit2)


###
# Q: Cues about the Public Budget do not Influence Perceptions of Unemployment (Study 2)
###


fit0<-lm(unempint~Btreat,data=expdat)
names(fit0$coefficients)<-c("Constant","Budget Cue")
fit1<-lm(unempint~Btreat+VK,data=expdat)
names(fit1$coefficients)<-c("Constant","Budget Cue","Opposition Identifier")
fit2<-lm(unempint~Btreat*VK,data=expdat)
names(fit2$coefficients)<-c("Constant","Budget Cue","Opposition Identifier","Budget Cue x Opposition Id.")
summary(fit0)
summary(fit1)
summary(fit2)



###
# END
###





