#######################
# This script reproduces Figure 4 and Table 2 in 
# the article "Partisan Elites as Culprits?"
#
#######################

#set directory to folder containing files
#setwd("/mydirectory/")

#load packages
library(stargazer)
library(xtable)

#load experimental data used for Study 2
load("study2.RData")
ls()
str(expdat)	#check data



##########
# Reproduce results in Table 2

#first colummn (Model 1)
fit1<-lm(budgetint~Btreat*VK,data=expdat)
names(fit1$coefficients)<-c("Constant","Cue","Opposition Identifier","Cue x Opposition Id.")
summary(fit1)

#second column (Model 2)
fit2<-lm(unempint~Utreat*VK,data=expdat)
names(fit2$coefficients)<-c("Constant","Cue","Opposition Identifier","Cue x Opposition Id.")
summary(fit2)


#producing results for Table 2
stargazer(fit1,fit2,no.space=T,star.cutoffs=c(0.05,0.01,0.001))


#treatment effect among opposition identifiers (VK=1), reported in text
fitB2<-lm(budgetint~Btreat,data=expdat[expdat$VK==1,])
names(fitB2$coefficients)<-c("Constant","Cue")
summary(fitB2)

fitU2<-lm(unempint~Utreat,data=expdat[expdat$VK==1,])
names(fitU2$coefficients)<-c("Constant","Cue")
summary(fitU2)



#####
# Reproduce Figure 4


#calculating expected means and associated SEs for each partisan group (note: models subsetting on "VK==1" are fitted above) 
fitB1<-lm(budgetint~Btreat,data=expdat[expdat$VK==0,]) 
fitU1<-lm(unempint~Utreat,data=expdat[expdat$VK==0,])

Bcoefs=matrix(c(fitB1$coef[1],fitB1$coef[1]+fitB1$coef[2],
	fitB2$coef[1],fitB2$coef[1]+fitB2$coef[2]),nr=2)
Bse=matrix(c(sqrt(vcov(fitB1)[1,1]),sqrt(vcov(fitB1)[1,1]+vcov(fitB1)[2,2]+2*vcov(fitB1)[1,2]),
sqrt(vcov(fitB2)[1,1]),sqrt(vcov(fitB2)[1,1]+vcov(fitB2)[2,2]+2*vcov(fitB2)[1,2])),nr=2)

#same for unemployment issue
Ucoefs=matrix(c(fitU1$coef[1],fitU1$coef[1]+fitU1$coef[2],
	fitU2$coef[1],fitU2$coef[1]+fitU2$coef[2]),nr=2)
Use=matrix(c(sqrt(vcov(fitU1)[1,1]),sqrt(vcov(fitU1)[1,1]+vcov(fitU1)[2,2]+2*vcov(fitU1)[1,2]),
sqrt(vcov(fitU2)[1,1]),sqrt(vcov(fitU2)[1,1]+vcov(fitU2)[2,2]+2*vcov(fitU2)[1,2])),nr=2)


#graphing results (produces Figure 4 in the paper)
#png('graph_study2.png',height=4,width=9,units="in",res=1800)

x=1:2+.035		 #set space so points do not overlap.
par(mfrow=c(1,1),omd=c(0.05,.525,0,1))

##PANEL A##
plot(Bcoefs[,1],ylim=c(.3,.8),xaxt='n',xlim=c(.75,2.25),xlab="",ylab="Perception of Budget Deficit",yaxt='n')
axis(2,las=2)
axis(2,seq(0,1,.025),tck=-0.025,labels=NA)
mtext(3,text="Budget Deficit",font=1,cex=1.5,padj=-1.5)
points(x,Bcoefs[,2],pch=21,bg="black")
lines(x,Bcoefs[,2],lty=1);lines(Bcoefs[,1],lty=1)
segments(1:2,Bcoefs[,1],1:2,Bcoefs[,1]+2*Bse[,1])
segments(1:2,Bcoefs[,1],1:2,Bcoefs[,1]-2*Bse[,1])
segments(x,Bcoefs[,2],x,Bcoefs[,2]+2*Bse[,2])
segments(x,Bcoefs[,2],x,Bcoefs[,2]-2*Bse[,2])
points(Bcoefs[,1],pch=21,bg="white",cex=1.25)
points(x,Bcoefs[,2],pch=21,bg="black",cex=1.25)
legend('bottomright',legend=c("Government identifiers","Opposition identifiers"),cex=.9,
	pch=21,pt.bg=c('white','black'),bty='n',pt.cex=1.25)
axis(1,at=1:2,labels=c('Gov: no problem','Gov: problem'),cex.axis=1,las=1)
mtext(1,text="Treatment Condition",line=3,hadj=-1)

##PANEL B##
par(omd=c(0.45,.925,0,1),new=T)
plot(Ucoefs[,1],ylim=c(.3,.8),xaxt='n',xlim=c(.75,2.25),xlab="",ylab="",yaxt='n')
mtext(3,text="Unemployment",font=1,cex=1.5,padj=-1.5)
points(x,Ucoefs[,2],pch=21,bg="black")
lines(x,Ucoefs[,2],lty=1);lines(Ucoefs[,1],lty=1)
segments(1:2,Ucoefs[,1],1:2,Ucoefs[,1]+2*Use[,1])
segments(1:2,Ucoefs[,1],1:2,Ucoefs[,1]-2*Use[,1])
segments(x,Ucoefs[,2],x,Ucoefs[,2]+2*Use[,2])
segments(x,Ucoefs[,2],x,Ucoefs[,2]-2*Use[,2])
points(Ucoefs[,1],pch=21,bg="white",cex=1.25)
points(x,Ucoefs[,2],pch=21,bg="black",cex=1.25)
axis(1,at=1:2,labels=c('Gov: no problem','Gov: problem'),cex.axis=1,las=1)
axis(4,las=2)
axis(4,seq(0,1,.025),tck=-0.025,labels=NA)
mtext(4,text="Perception of Unemployment",line=3)
mtext(1,text="Treatment Condition",line=3)

#dev.off()

######
# END




