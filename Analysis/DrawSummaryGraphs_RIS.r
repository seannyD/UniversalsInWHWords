library(RColorBrewer)

setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis")

xlimx = c(0.4,0.82)
ylimx =c(0,1200)

colxA = brewer.pal(6,'Set2')
set.seed(10)
colxA = sample(colxA)
colx = paste(colxA,"82",sep='')

breaks = seq(-0.6,0.6,by=0.01)
ylimx=c(0,8.2)

######
plotDifHist = function(d, colx='#428C96',add=F){
#d = read.csv(filename)

hist(d$SumEntropy, breaks= breaks, col=colx, border=colx, xaxt='n', ylab='Density',xlab='Difference in Mean Entropies',main='',probability=T,add=add,ylim=ylimx)
if(!add){
	axis(1,pos=0)
	axis(1,at=c(-0.4,0.4),tick=F,labels=c("Initial < Non-Initial", "Non-Initial < Initial"),line=0.5)
	abline(v=0,lwd=2)
}
}

#plotDifHist("../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/ResultsSummary_RandomIndependentSamples.txt.csv")

#plotDifHist("../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/ResultsSummary_RandomIndependentSamples_RandomConcepts.txt.csv")


# WH WORDS
pdf(file='Graphs/RIS_Initial_NonInitial_wh.pdf', width=4.5,height=4.5)
par(bg=rgb(0,0,0,0))
plotDifHist(read.csv("../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/InterrogativeOrder_RandomIndependentSamples_firstSegments.csv"))
dev.off()


# RANDOM
folder = "../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/"
files = list.files(folder,"RIS_RandomConcepts_first*")
d = data.frame()
for(f in files){
	dx = read.csv(paste(folder,f,sep=''))
	d = rbind(d,dx)
}
pdf(file='Graphs/RIS_Initial_NonInitial_random.pdf', width=4.5,height=4.5)
par(bg=rgb(0,0,0,0))
	plotDifHist(d,col="#F39484")
dev.off()

plotDifHist(read.csv("../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/InterrogativeOrder_RandomIndependentSamples_firstSegments.csv"))
plotDifHist(d,col="#F3948485",add=T)