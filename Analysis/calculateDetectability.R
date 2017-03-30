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

mean(z.score)
# -0.5382705
# -0.5300793

# proportion of samples perm prob > wh prob
# (we predict that perm prob should be more than wh prob)
p.values = rowSums(permDetectability > trueDetectability) / ncol(permDetectability)
p.values[p.values==0] = 1/nperm 
sum(z.score < 0)/length(z.score)
#0.9360465
#0.9517544   (217 out of 228)
sum(p.values>0.95 & z.score<0)/length(p.values)
#0.6802326
#0.6973684 (159 out of 228)

detect.d = data.frame(lang=colnames(d.wh.m), detect=trueDetectability, p= p.values,z=z.score,stringsAsFactors=F,xnum = 1:length(trueDetectability))

lang2glotto = tapply(alldata$glotto,alldata$Language,sample,size=1)

detect.d$interrogative.position = l.details[match(lang2glotto[detect.d$lang], l.details$glotto),]$qpos

detect.d$initialInterrogative = detect.d$interrogative.position=="1 Initial interrogative phrase"
detect.d$initialInterrogative[detect.d$initialInterrogative==""] = NA

detect.d$langFam = l.details[match(lang2glotto[detect.d$lang], l.details$glotto),]$langFam

detect.d$area = l.details[match(lang2glotto[detect.d$lang], l.details$glotto),]$area

write.csv(detect.d,file="../Results/SimplifiedPhonology/Detectability/RandomConcepts/detectability_randomConcepts_firstSegments.csv")

#detect.d = read.csv("../Results/SimplifiedPhonology/Detectability/RandomConcepts/detectability_randomConcepts_firstSegments.csv")


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
# Yaminahua

# Get profile of most detectable language
xRand = table(substr(d.random.m[,which(colnames(d.random.m)==mostDetectable)],1,1))
xWh = table(substr(d.wh.m[,which(colnames(d.random.m)==mostDetectable)],1,1))

sort(xRand/sum(xRand))
xWh/sum(xWh)


# Test interrogative order
dx = detect.d[!is.na(detect.d$initialInterrogative),]
dx = dx[!dx$interrogative.position=='3 Mixed',]
dx = dx[!duplicated(dx$lang),]

##########
# Random independent sample
selectRandomIndependentSampleB = function(values,strat,limit=length(values)){
  x = tapply(values,strat,sample,size=1)
  # out of this list, return 'limit' x languages
  return(x[sample(1:length(x),size=limit)])
}

getRandIndepSampleDiff = function(v1,v2, strat1, strat2,limit){
    rand.initial =     selectRandomIndependentSampleB(v1, strat1, limit)
    rand.non.initial = selectRandomIndependentSampleB(v2, strat2, limit)
    mean(rand.initial) - mean(rand.non.initial)
}

limit = min(length(unique(dx[dx$initialInterrogative,]$langFam)),
          length(unique(dx[!dx$initialInterrogative,]$langFam)))

set.seed(3267)
random.detectability.fam = replicate(10000, getRandIndepSampleDiff(
  dx[dx$initialInterrogative,]$z,
  dx[!dx$initialInterrogative,]$z,
  dx[dx$initialInterrogative,]$langFam,
  dx[!dx$initialInterrogative,]$langFam,
  limit
))

hist(random.detectability.fam)
sum(random.detectability.fam<=0) / length(random.detectability.fam)
# 0.7182


limit2 = min(length(unique(dx[dx$initialInterrogative,]$area)),
            length(unique(dx[!dx$initialInterrogative,]$area)))

set.seed(34367)
random.detectability.area = replicate(10000, getRandIndepSampleDiff(
  dx[dx$initialInterrogative,]$z,
  dx[!dx$initialInterrogative,]$z,
  dx[dx$initialInterrogative,]$area,
  dx[!dx$initialInterrogative,]$area,
  limit2
))

hist(random.detectability.area)
sum(random.detectability.area<=0) / length(random.detectability.area)
# 0.9145
# 0.7548



########
hist(log(dx$detect[!dx$initialInterrogative]), col = rgb(1,0,0,0.5), border=F)
hist(log(dx$detect[dx$initialInterrogative]), add=T,col=rgb(0,0,0,0.5), border = F)

brxx = breaks=seq(-1.5,4,length.out=20)
hist(dx$z[!dx$initialInterrogative], col = rgb(1,0,0,0.5), border=F, breaks=brxx)
hist(dx$z[dx$initialInterrogative], add=T,col=rgb(0,0,0,0.5), border = F, breaks= brxx)


plotmeans(detect.d$z~detect.d$initialInterrogative)
t.test(detect.d$z~detect.d$initialInterrogative)


# Welch Two Sample t-test
# 
# data:  detect.d$z by detect.d$initialInterrogative
# t = 1.6489, df = 51.535, p-value = 0.1052
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0435307  0.4443460
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# -0.4368377          -0.6372453 


# Welch Two Sample t-test
# 
# data:  detect.d$z by detect.d$initialInterrogative
# t = 1.2165, df = 109, p-value = 0.2264
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.03538043  0.14783455
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# -0.5691511          -0.6253782


cutoff.h = mean(detect.d$z) + (2* sd(detect.d$z))
cutoff.l = mean(detect.d$z) - (2* sd(detect.d$z))

plotmeans(detect.d[detect.d$z < cutoff.h & detect.d$z>cutoff.l,]$z~detect.d[detect.d$z < cutoff.h & detect.d$z>cutoff.l,]$initialInterrogative)

t.test(detect.d[detect.d$z < cutoff.h & detect.d$z>cutoff.l,]$z~detect.d[detect.d$z < cutoff.h & detect.d$z>cutoff.l,]$initialInterrogative)

# Welch Two Sample t-test
# 
# data:  detect.d[detect.d$z < cutoff.h & detect.d$z > cutoff.l, ]$z by detect.d[detect.d$z < cutoff.h & detect.d$z > cutoff.l, ]$initialInterrogative
# t = 2.5242, df = 65.956, p-value = 0.01401
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01859882 0.15937449
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# -0.5909012          -0.6798878 


# Welch Two Sample t-test
# 
# data:  detect.d[detect.d$z < cutoff.h & detect.d$z > cutoff.l, ]$z by detect.d[detect.d$z < cutoff.h & detect.d$z > cutoff.l, ]$initialInterrogative
# t = 2.6217, df = 90.705, p-value = 0.01026
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01691355 0.12269438
# sample estimates:
#   mean in group FALSE  mean in group TRUE 
# -0.5959750          -0.6657789 



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
# 0.0342
# 0.2144


hist(dx$z)

library(lme4)

m0 = lmer(log(dx$detect)~ 1 + (1 + initialInterrogative|langFam), data = dx)
m1 = lmer(log(dx$detect)~ initialInterrogative + (1 + initialInterrogative|langFam), data = dx)
anova(m0,m1)

# maximal model
m0 = lmer(z~ 1 + (1+initialInterrogative|langFam) + (1 +initialInterrogative| area), 
          data = dx[dx$z < cutoff.h & dx$z>cutoff.l,])
# Check the correlation of random intercept and slope:
cor(ranef(m0)$langFam[,1],ranef(m0)$langFam[,2])
cor(ranef(m0)$area[,1],ranef(m0)$area[,2])

# Slopes and intercepts are exactly correlated for langFam, so drop the random slope
# (in fact, including random slope for area lowers the probability, but either way not significant)

m0 = lmer(z~ 1 + (1|langFam) + (1 | area), 
          data = dx[dx$z < cutoff.h & dx$z>cutoff.l,])

m1 = lmer(z~ initialInterrogative + (1|langFam) + (1| area), 
          data = dx[dx$z < cutoff.h & dx$z>cutoff.l,])
anova(m0,m1)

# Data: dx[dx$z < cutoff.h & dx$z > cutoff.l, ]
# Models:
#   m0: z ~ 1 + (1 | langFam) + (1 | area)
# m1: z ~ initialInterrogative + (1 | langFam) + (1 | area)
#    Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# m0  4 -81.704 -71.089 44.852  -89.704                         
# m1  5 -81.024 -67.754 45.512  -91.024 1.3194      1     0.2507

# 
# m0: z ~ 1 + (1 | langFam) + (1 | area)
# m1: z ~ initialInterrogative + (1 | langFam) + (1 | area)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# m0  4 -133.56 -122.84 70.782  -141.56                         
# m1  5 -131.83 -118.42 70.916  -141.83 0.2684      1     0.6044

summary(m1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: z ~ initialInterrogative + (1 | langFam) + (1 | area)
# Data: dx[dx$z < cutoff.h & dx$z > cutoff.l, ]
# 
# REML criterion at convergence: -81
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.6492 -0.5518 -0.0287  0.3825  4.8822 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# langFam  (Intercept) 0.004055 0.06368 
# area     (Intercept) 0.006972 0.08350 
# Residual             0.018805 0.13713 
# Number of obs: 105, groups:  langFam, 49; area, 16
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)              -0.60321    0.03457 -17.450
# initialInterrogativeTRUE -0.04253    0.03721  -1.143
# 
# Correlation of Fixed Effects:
#   (Intr)
# intlIntTRUE -0.501


# Linear mixed model fit by REML ['lmerMod']
# Formula: z ~ initialInterrogative + (1 | langFam) + (1 | area)
# Data: dx[dx$z < cutoff.h & dx$z > cutoff.l, ]
# 
# REML criterion at convergence: -131.2
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -1.92530 -0.58954 -0.05192  0.55554  3.01118 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# langFam  (Intercept) 0.000153 0.01237 
# area     (Intercept) 0.007470 0.08643 
# Residual             0.013073 0.11434 
# Number of obs: 108, groups:  langFam, 45; area, 16
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)              -0.60482    0.03015  -20.06
# initialInterrogativeTRUE -0.01385    0.03009   -0.46
# 
# Correlation of Fixed Effects:
#   (Intr)
# intlIntTRUE -0.468