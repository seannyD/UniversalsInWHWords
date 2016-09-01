setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Writeup/examples/")


pdf(file="Permutation_comparison.pdf", width = 7, height = 4.5)
par(mfrow=c(1,3), mar=c(4,2,2,1))
set.seed(15)
x = rnorm(8, 0.4, 0.2)
y = rnorm(8,0.8,0.12)

trueDiff = diff(tapply(c(x,y),c(rep(1,length(x)),rep(16,length(y))), mean))

plot(c(x,y)~c(rep(1,length(x)),rep(2,length(y))),
     pch = c(rep(1,length(x)),rep(16,length(y))),
     xlim=c(0.5,2.5), ylim=c(0,1),
     xlab='', ylab='', xaxt='n')

lines(c(0.75,1.5), c(mean(x),mean(x)), lwd=2, lty=2)
lines(c(1.5,2.25), c(mean(y),mean(y)), lwd=2)

set.seed(324)
perm.pch = sample(c(rep(1,length(x)),rep(16,length(y))))

plot(c(x,y)~c(rep(1,length(x)),rep(2,length(y))),
     pch = perm.pch,
     xlim=c(0.5,2.5),ylim=c(0,1),
     xlab='', ylab='', xaxt='n')

new.mean = tapply(c(x,y), perm.pch, mean)

lines(c(0.75,1.5), c(new.mean[1],new.mean[1]), lwd=2, lty=2)
lines(c(1.5,2.25), c(new.mean[2],new.mean[2]), lwd=2)

perm.pch = sample(c(rep(1,length(x)),rep(16,length(y))))

plot(c(x,y)~c(rep(1,length(x)),rep(2,length(y))),
     pch = perm.pch,
     xlim=c(0.5,2.5),ylim=c(0,1),
     xlab='', ylab='', xaxt='n')

new.mean = tapply(c(x,y), perm.pch, mean)

lines(c(0.75,1.5), c(new.mean[1],new.mean[1]), lwd=2, lty=2)
lines(c(1.5,2.25), c(new.mean[2],new.mean[2]), lwd=2)
dev.off()

getx = function(){
  perm.pch = sample(c(rep(1,length(x)),rep(16,length(y))))
  diff(tapply(c(x,y), perm.pch, mean))
}

p = replicate(1000,getx())

x = rnorm(1000,mean = 0, sd=0.05)

pdf("distComparison.pdf")
hist(x, xlim=c(-0.5,0.5), breaks=20, ylim=c(0,160), xaxt='n', yaxt='n', main='', xlab="", ylab='')
abline(v=trueDiff, lwd=2, col=2)
axis(1,pos=0)
dev.off()
