#######################
# This script reproduces Figure 1 in 
# the article "Partisan Elites as Culprits?"
#
#######################

#load packages
library(car)
library(memisc)

#load data with the Primeministers statements
load("statements.RData")

#cleaning codes from coder 1, 2, 3, and 4.
statement$c1=recode(statement$c1,'2=1;3=1;4=0;5=0;-11=0;-99=NA')
statement$c2=recode(statement$c2,'2=1;3=1;4=0;5=0;-11=0;-99=NA')
statement$c3=recode(statement$c3,'2=1;3=1;4=0;5=0;-11=0;-99=NA')
statement$c4=recode(statement$c4,'2=1;3=1;4=0;5=0;-11=0;-99=NA')

#Note: The commands above recodes the classifications from the coders into binary outcomes where: 
#	 1=PM says deficit is a problem 
#	 0=PM does not talk about issue/says it is not a problem 
#	 In addition, -99 are coded as missing and denotes articles that where not relevant (e.g. comics, op-eds)	


#Construct vector of common agreement among coders
com_in<-cbind(statement$c1,statement$c2,statement$c3,statement$c4)	#set up in matrix form
statement$code_total<-apply(com_in,1,FUN=mean) 					#take the average of codes over rows	
out_total<-aggregate(code_total~month,data=statement,mean)			#aggregate result by month



###############
# Reproduce Figure 1

png("PMstatement.png",width=5,height=4,units="in",res=1600)

par(omd=c(.05,1,0,1),mfrow=c(1,1))	#set plotting region
x=out_total[,2]		#set vector to be plotted
wh=1:5			#placement on X-axis

#build plot
plot(wh,x,xlab="",ylab="",xaxt="n",yaxt="n",col="white",ylim=c(0,.75),xlim=c(.5,5.5))
lines(wh,x)
axis(2,at=seq(0,1,.25),las=2)
axis(2,at=seq(0,1,.05),las=2,tck=-0.015,labels=NA)
points(wh,x,pch=21,bg="grey",cex=1)
mtext(2,line=3.5,text="Proportion of Articles")
text(x=3,y=.6,label="Restoration Act",cex=.8)
arrows(4,.575,4.85,.425,length=0.1)
axis(1,at=1:5,labels=c("Jan\n(N=54)","Feb\n(34)","Mar\n(16)","Apr\n(44)","May\n(25)"),padj=.5)

dev.off()
####
# END




