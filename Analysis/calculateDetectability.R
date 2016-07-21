library(gplots)
try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

#noData = which(apply(d.wh.m,2,function(X){sum(!is.na(X))})<2)

#d.wh.m = d.wh.m[,-noData]
#d.random.m = d.random.m[,-noData]

mhd = function(choice,pop){
  # Multivariate hypergeometric distribution
  # (probability of choosing choice out of pop)
  tx = cbind(table(choice), table(pop[pop %in% choice]))
  return(prod(apply(tx,1,function(X){choose(X[2],X[1])})) / choose(length(pop),length(choice)))
}

getfirstphoneme = function(X){	
  sx = unlist(strsplit(X,";"))
  sx = substr(sx,1,1)
  sx = sx[!is.na(sx)]
  return(as.vector(sx))
}

getallphonemes = function(X){
  unlist(strsplit(unlist(strsplit(X,";")),""))
}

getDetectability = function(sub.p,all.p){
  sapply(1:length(sub.p), function(X){
     mhd(sub.p[[X]],all.p[[X]])
  })
}

getRandomDetectability = function(sub.p,all.p, nperm=1000){
  nWords = sapply(sub.p,length)
  mapply(function(allx,k){
    replicate(nperm, mhd(sample(allx,k),allx))
  }, all.p,nWords)

}

wh.first.phonemes = apply(d.wh.m,2,getfirstphoneme)
all.first.phonemes = apply(rbind(d.random.m,d.wh.m),2,getfirstphoneme)

#wh.first.phonemes = apply(d.wh.m,2,getallphonemes)
#all.first.phonemes = apply(rbind(d.random.m,d.wh.m),2,getallphonemes)


trueDetectability = getDetectability(wh.first.phonemes,all.first.phonemes)

nperm = 10000

set.seed(102020)
permDetectability = t(getRandomDetectability(wh.first.phonemes,all.first.phonemes,nperm))

# perm should be > true, so predict negative
z.score = (trueDetectability - apply(permDetectability,1,mean)) / apply(permDetectability,1,sd)


# proportion of samples perm prob > wh prob
# (we predict that perm prob should be more than wh prob)
p.values = rowSums(permDetectability > trueDetectability) / ncol(permDetectability)
p.values[p.values==0] = 1/nperm 
sum(z.score < 0)/length(z.score)
sum(p.values>0.95 & z.score<0)/length(p.values)

detect.d = data.frame(lang=colnames(d.wh.m), detect=trueDetectability, p= p.values,z=z.score,stringsAsFactors=F,xnum = 1:length(trueDetectability))

lang2glotto = tapply(alldata$glotto,alldata$Language,sample,size=1)

detect.d$interrogative.position = l.details[match(lang2glotto[detect.d$lang], l.details$glotto),]$possiblegrammars

detect.d$initialInterrogative = detect.d$interrogative.position=="1 Initial interrogative phrase"
detect.d$initialInterrogative[detect.d$initialInterrogative==""] = NA

detect.d$langFam = l.details[match(lang2glotto[detect.d$lang], l.details$glotto),]$langFam

write.csv(detect.d,file="../Results/SimplifiedPhonology/Detectability/RandomConcepts/detectability_randomConcepts_firstSegments.csv")

par(mfrow=c(3,3))
for(i in 1:9){
hist(log(permDetectability[i,]),xlim=range(log(c(permDetectability[i,],trueDetectability[i]))), breaks=40,main=i)
abline(v=log(trueDetectability[i]),col=2)
}

xlims = range(c(log(permDetectability),log(trueDetectability)))
breaks = seq(xlims[1],xlims[2],length.out=30)
cols = c(rgb(0.8,0.8,0.8,1),rgb(1,0,0,0.2))

hist(log(permDetectability),xlim=xlims, freq=F, breaks = breaks, col=cols[1], border=NA, main='', xlab='Log probability')
hist(log(trueDetectability), add=T, col=cols[2], freq=F, breaks= breaks, border=NA)
legend(-45,0.12,legend=c("Wh-word detectability","Random words detectability"), col=cols, pch=15, cex=1)


mostDetectable = detect.d[which(detect.d$z==min(detect.d$z)),]$lang

xRand = table(substr(d.random.m[,which(colnames(d.random.m)==mostDetectable)],1,1))
xWh = table(substr(d.wh.m[,which(colnames(d.random.m)==mostDetectable)],1,1))

sort(xRand/sum(xRand))
xWh/sum(xWh)


t.test(detect.d$z~detect.d$initialInterrogative)

#Welch Two Sample t-test

# data:  detect.d$z by detect.d$initialInterrogative
# t = 1.4633, df = 52.209, p-value = 0.1494
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.06664641  0.42578787
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# -0.4559342          -0.6355049 


cutoff.h = mean(detect.d$z) + (2* sd(detect.d$z))
cutoff.l = mean(detect.d$z) - (2* sd(detect.d$z))

t.test(detect.d[detect.d$z < cutoff.h & detect.d$z>cutoff.l,]$z~detect.d[detect.d$z < cutoff.h & detect.d$z>cutoff.l,]$initialInterrogative)

# Welch Two Sample t-test
# 
# data:  detect.d[detect.d$z < cutoff.h & detect.d$z > cutoff.l, ]$z by detect.d[detect.d$z < cutoff.h & detect.d$z > cutoff.l, ]$initialInterrogative
# t = 2.1238, df = 73.088, p-value = 0.03707
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.003961409 0.124638734
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# -0.6134951          -0.6777951


dx = detect.d[!is.na(detect.d$initialInterrogative),]

trueDiff = mean(dx[dx$initialInterrogative,]$z,na.rm=T) - mean(dx[!dx$initialInterrogative,]$z,na.rm=T)


npermDiff = 10000
set.seed(8129)
permDiff = replicate(npermDiff, {
  iix = sample(dx$initialInterrogative)
  # initial langs should have a lower z score than non-initial
  
  mean(dx$z[iix]) - mean(dx$z[!iix])
})

# True difference should be very low and negative
# permuted difference should be higher
#  we predict that permuted difference should be less than actual difference
# P value:
1 - sum(trueDiff < permDiff) / npermDiff
# 0.0598

library(lme4)
m0 = lmer(z~ 1 + (1|langFam), data = dx[detect.d$z < cutoff.h & detect.d$z>cutoff.l,])
m1 = lmer(z~ initialInterrogative + (1|langFam), data = dx[detect.d$z < cutoff.h & detect.d$z>cutoff.l,])
anova(m0,m1)
# m0: z ~ 1 + (1 | langFam)
# m1: z ~ initialInterrogative + (1 | langFam)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# m0  3 176.37 184.33 -85.186   170.37                        
# m1  4 175.67 186.29 -83.835   167.67 2.702      1     0.1002
summary(m1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: z ~ initialInterrogative + (1 | langFam)
# Data: dx[detect.d$z < cutoff.h & detect.d$z > cutoff.l, ]
# 
# REML criterion at convergence: 173.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.4423 -0.3016 -0.0905  0.0951  5.2106 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# langFam  (Intercept) 0.3176   0.5636  
# Residual             0.1425   0.3775  
# Number of obs: 105, groups:  langFam, 51
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)               -0.4314     0.1101  -3.917
# initialInterrogativeTRUE  -0.1826     0.1115  -1.638
# 
# Correlation of Fixed Effects:
#   (Intr)
# intlIntTRUE -0.561
