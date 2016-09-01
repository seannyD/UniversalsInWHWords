setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Writeup/examples/")

x = rnorm(1000,mean = 0.58, sd=0.05)

hist(x, xlim=c(0,1), breaks=20, ylim=c(0,160), xaxt='n', yaxt='n', main='', xlab=expression("E"[f]), ylab='')
abline(v=0.26, lwd=2, col=2)
axis(1,pos=0)

set.seed(1298)
pdf("IndependetSamples_dist.pdf", height=4, width=4)
x = rnorm(1000, mean = 0.2, sd=0.1)
hist(x, xlim=c(-1,1), xaxt='n', yaxt='n', main='', xlab="", ylab='')
axis(1,pos=0)
abline(v=0, lty=2)
dev.off()