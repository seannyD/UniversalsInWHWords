library(RColorBrewer)

setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis")

xlimx = c(0.4,0.82)
ylimx =c(0,1200)

colxA = brewer.pal(6,'Set2')
set.seed(10)
colx = paste(sample(colxA),"82",sep='')

breaks = seq(0.4,1,by=0.001)

files= c("AllLangs_firstSegments.csv","AllLangs_firstSegment_byFamily.csv","AllLangs_firstSegment_byArea.csv","AllLangs_firstSegment_byAreaAndFamily.csv","RandomConcepts/Comparison_WH_Random_firstSegments.csv")
nlangs = 1

trueEntropy = read.csv("../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments.csv")[1,]$SumEntropy


# Load random within domain results
random.within.domain = data.frame()
for(i in 1:20){
	dx = read.csv(paste("../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/Comparison_WH_Domain_firstSegments_concept_",i,".csv",sep=''))
	random.within.domain = rbind(random.within.domain,dx[-1,])
}
random.within.domain.hist = hist(random.within.domain[random.within.domain$Type=='Perm',]$SumEntropy,plot=F,breaks=breaks)
random.within.domain.hist$counts = random.within.domain.hist$counts/10


pdf(file="Graphs/MainSummaryGraph.pdf", width=8.81, height= 6.14)
par(yaxs='i',xpd=TRUE,mar=c(5, 4, 7, 2) + 0.1)

# plot other things
for(f in 1:length(files)){
	rx = read.csv(paste("../Results/SimplifiedPhonology/PermutationResults/",files[f],sep=''))
	to.plot =rx[rx$Type=='Perm',]$SumEntropy
	if(rx[1,]$SumEntropy>1){
		to.plot = to.plot
	}
	to.add = T
	if(f==1){ to.add=F}
	hist(to.plot,col=colx[f],border=NA,xlim=xlimx,main='',breaks=breaks,xlab='',ylab='',ylim=ylimx,add=to.add)
	
	if(f==1){
		plot(random.within.domain.hist,add=T,col=colx[6],border=NA,xlim=xlimx,main='',xlab='',ylab='',ylim=c(0,10000))
	}
}

adj = 200

lines(c(trueEntropy,trueEntropy),c(0,ylimx[2]-100),col=2,lwd=2)

actions = 0.5929543

lines(c(actions,actions),c(0,ylimx[2]-100),col=3,lwd=2)

body = 0.6109565

lines(c(body,body),c(0,ylimx[2]-100),col=4,lwd=2)

legend(0.38,1600,legend=c("Mean Wh-word Entropy","Permuted",'  within families',"  within areas","  within families and areas",'Mean entropy of',"  Random concepts",'  Random concepts from the same domain', "  Action term concetps", "  Body term concepts"),col = c(2,colxA[1:4],NA,colxA[5:6],3,4),pch=c(NA,rep(17,4),NA,17,17,NA,NA),lty=NA,lwd=2,ncol=2,cex=1)
box(bty='l')

title(xlab='Mean Entropy',ylab='Count')

lines(c(0.609,0.609),c(adj+1130,adj+1180),lwd=2,col=3)
lines(c(0.609,0.609),c(adj+1060,adj+1110),lwd=2,col=4)
lines(c(0.391,0.391),c(adj+1360,adj+1310),lwd=2,col=2)

dev.off()


############
# Plot comparisons between intial and non-initial

plotComparison = function(filename, div=NA,xlimx=c(-0.4,0.4)){
	
	ylimx= c(0,2000)
	idiff.first = read.csv(filename)
	if(!is.na(div)){
		idiff.first$SumEntropy = idiff.first$SumEntropy/div
	}
	idiff.first.perm = idiff.first[idiff.first$Type=='Perm',]$SumEntropy

#upper.lower = sort(idiff.first.perm)[length(idiff.first.perm)*c(0.025,0.975)]
upper.lower = sort(idiff.first.perm)[c(1,length(idiff.first.perm)*(0.95))]

x = hist(idiff.first.perm,col=NA,border=1,xlab='Difference in entropies',main='',ylim=ylimx,xlim=xlimx)
lines(rep(idiff.first[idiff.first$Type=='True',]$SumEntropy,2),c(0,1500),lwd=2,col=2)
rect(upper.lower[1],0,upper.lower[2],ylimx[2]+100,border=NA,col=rgb(1,0,0,0.2))
plot(x,col=NA,border=1,add=T)
box(bty='l')

}


wh.files = c(
#"InterrogativeOrderDifference_1_firstSegments.csv",
#"InterrogativeOrderDifference_2_firstSegments.csv",
"InterrogativeOrderDifference_3_firstSegments.csv",
"InterrogativeOrder_PermuteFamilies_firstSegments.csv",
"InterrogativeOrder_PermuteAreas_firstSegments.csv","InterrogativeOrder_PermuteFamiliesAndAreas_firstSegments.csv","InterrogativeOrderDifference_unanalyzable_3_firstSegments.csv")

titles = c("First segments", 'Permuting within families', "Permuting within areas", "Permuting within families and areas",'Only unanalyzable words')

lims = list(c(-0.22,0.22),c(-0.22,0.22),c(-0.22,0.22),c(-0.22,0.22),c(-0.42,0.42))

par(mfrow=c(3,2),xpd=F)
for(i in 1:length(wh.files)){
	plotComparison(paste("../Results/SimplifiedPhonology/PermutationResults/InterrogativeOrder/",wh.files[i],sep=''),xlimx = lims[[i]])
	title(main=titles[i])
}


files = list.files("../Results/SimplifiedPhonology/PermutationResults/RandomInterrogativeOrder/","_3_firstSegment")


randtrue = c()
randPerm = c()

for(f in 1:length(files)){
	dx = read.csv(paste("../Results/SimplifiedPhonology/PermutationResults/RandomInterrogativeOrder/",files[f],sep=''))
	randtrue = c(randtrue,dx[1,]$SumEntropy)
	randPerm = c(randPerm,dx[2:nrow(dx),]$SumEntropy)
}


xlimx=c(-0.15,0.15)
ylimx=c(0,15)
breaks = seq(-0.2,0.2,by=0.01)
hist(randtrue,col=2,border=F,xlim=xlimx,probability=T,breaks=breaks,main='',xlab='Difference in entropies')
hist(randPerm,add=T,xlim=xlimx,probability=T,breaks=breaks,main='')
upper.lower = sort(randPerm)[c(1,length(randPerm)*(0.95))]
rect(upper.lower[1],0,upper.lower[2],ylimx[2]+100,border=NA,col=rgb(1,0,0,0.3))
title(main="Random concepts")


