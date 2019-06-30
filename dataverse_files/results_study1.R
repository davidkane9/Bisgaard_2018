##########################################
# This script reproduces Figure 2, Figure 3 and Table 1
# in the in article "Partisan Elites as Culprits?"
# AUTHORS: Martin Bisgaard & Rune Slothuus
# THIS VERSION: AUGUST 28, 2017

#set directory to specified folder containing the files
#setwd("/mydirectory/")

#load required packages (install if needed)
library(car)
library(plyr)
library(memisc)
library(plm)
library(lme4)
library(lmtest)
library(stargazer)
library(xtable)

#load the five wave panel data
load("study1.Rdata")
ls()				#check if data loaded correctly
str(dat.balance)




#########################
# REPRODUCE FIGURE 2 	#
# Note: the standard errors in the figure are calculated naively, i.e. without accounting for the clustering of observations within respondents.
# Doing so reduces the SE's slightly (see panel model results below), and the naive result is thus slightly more "conservative".

#calculate mean and SE's
biMean=matrix(NA,nc=2,nr=5)
biSE=matrix(NA,nc=2,nr=5)
for(t in 1:5){
biMean[t,]=c(mean(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T),mean(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T))
biSE[t,]=c(sd(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==0])),sd(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1],na.rm=T)/sqrt(length(dat.balance$bi[dat.balance$time==t&dat.balance$partytimeinvar==1])))
}


par(mfrow=c(1,1),omd=c(0,1,0,1)) 			#sets plotting region
x<-c(22/28,2+5/30,4+15/31,11+11/31,16+25/30) 	#sets correct spacing on x-axis to match unequal number of days pr month
tt=.9 							#controls text size
pp=1 								#controls point size
jit=.1 							#adds space to separate the points in the plot


# Build plot (Figure 2)
#png('naivecurves-budget.png',width=5,height=4,units="in",res=1800)

plot(x,biMean[,1],ylim=c(.5,.7),xlim=c(0,max(x)),xaxt='n',yaxt='n',ylab=NA,xlab=NA)
lines(x,biMean[,1])
points(x+jit,biMean[,2],pch=21,bg="black",cex=pp);lines(x+jit,biMean[,2])
segments(x,biMean[,1],x,biMean[,1]+2*biSE[,1]);segments(x,biMean[,1],x,biMean[,1]-2*biSE[,1]) 
segments(x+jit,biMean[,2],x+jit,biMean[,2]+2*biSE[,2]);segments(x+jit,biMean[,2],x+jit,biMean[,2]-2*biSE[,2])
points(x,biMean[,1],pch=21,bg="white",cex=pp)
abline(v=(3+19/31),lty=2)
arrows(5.1,.675,3.8,.65,length=.1)
text(5,.68,"Restoration Act",pos=4,cex=.9)
axis(1,at=0:16,label=c('February','March','April','May','June','','','September','','','','January','','','','','June'),las=2,cex.axis=tt)
axis(2,las=2,cex.axis=tt)
axis(2,at=seq(0,1,.01),labels=NA,tck=-0.015)
mtext(2,text='Perception of budget deficit',padj=-5,cex=tt)
legend('bottomright',legend=c('Government identifiers','Opposition identifiers'),cex=tt,bty='n',pch=21,pt.bg=c('white','black'))

#dev.off()

##################################
# REPRODUCE RESULTS IN TABLE 1	
# Note: Income is entered as a factor variable to keep the "do not want to report" category
dat.balance$income<-relevel(factor(dat.balance$income),ref='100.000 - 199.999 kr.') #same reference category as reported in the article

fit0<-plm(bi~income+educationClean+sex+age+
	partytimeinvar*partycue,data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time")
	,model="pooling")

#set correct variabel names
names(fit0$coefficients)<-c("Constant","Income: 200.000-299.999 DKR","Income: 300.000-399.999 DKR",
	"Income: 400.000-499.999 DKR","Income: 500.000-599.999 DKR","Income: 600.000-699.999 DKR","Income: 700.000 or above",
	"Income: Up to 99.999 DKR","Income: Refuse","Education","Sex","Age","Opposition Identifier","Cue","Cue x Opposition Id.")

#calculate and insert cluster-robust SEs in model object
fit0$vcov<-vcovHC(fit0,cluster="group") 

#Results for Table 1
print(summary(fit0),subset=c("Opposition Identifier","Cue","Cue x Opposition Id.","Constant"))

#Full output (printed in the SI)
summary(fit0)

####
#PRE/POST change for opposition identifiers (reported in text)
dat.balance$partytimeinvarR<-recode(dat.balance$partytimeinvar,'1=0;0=1')	#reversed PID indicator (easy way to get marginal effects for contrast)
fit.reverse<-plm(bi~income+educationClean+sex+age+partytimeinvarR*partycue,
data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="pooling")
coeftest(fit.reverse,vcov=vcovHC(fit.reverse,cluster="group"))["partycue",]

####
# Partisan gap after change in party cues (reported in text)

dat.balance$cueR<-recode(dat.balance$partycue,'1=0;0=1')
fit.reverse<-plm(bi~income+educationClean+sex+age+partytimeinvar*cueR,
data=dat.balance[dat.balance$time%in%c(2,3),],index=c("id","time"),model="pooling")
coeftest(fit.reverse,vcov=vcovHC(fit.reverse,cluster="group"))["partytimeinvar",]


####
#Placebo Diff-in-Diff (Wave 1 - Wave 2) (reported in a footnote)
fit.placeboDiD<-plm(bi~income+educationClean+sex+age+
	partytimeinvar*time,data=dat.balance[dat.balance$time%in%c(1,2),],	#set comparison to wave 1 and 2
	index=c("id","time"),model="pooling")
#get DiD and cluster-robust SEs
coeftest(fit.placeboDiD,vcov=vcovHC(fit.placeboDiD,cluster="group"))["partytimeinvar:time2",]



#### 
# END

