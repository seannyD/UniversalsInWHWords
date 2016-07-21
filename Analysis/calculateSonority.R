
try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

noData = which(apply(d.wh.m,2,function(X){sum(!is.na(X))})<2)

d.wh.m = d.wh.m[,-noData]
d.random.m = d.random.m[,-noData]

allsegments = unique(unlist(strsplit(unlist(rbind(d.wh.m,d.random.m)),'')))

#write.csv(allsegments,"../RAW_data/SegmentSonority.csv")
sonority = read.csv("../RAW_data/SegmentSonority.csv",stringsAsFactors = F)

sonority.scale = c(p=1,a=2,f=3,n=4,l=5,g=6,v=7)
sonority.score = sonority.scale[sonority$sonority]
names(sonority.score) = sonority$x
sonority.score = sonority.score[!is.na(sonority.score)]

dx = rbind(d.wh.m,d.random.m)

dx.son.all = apply(dx,2,function(X){
  sapply(X,function(Y){
    sx = strsplit(Y,"")[[1]]
    mean(sonority.score[sx],na.rm=T)
  })
})

dx.son.cons = apply(dx,2,function(X){
  sapply(X,function(Y){
    sx = strsplit(Y,"")[[1]]
    sx = sonority.score[sx]
    sx = sx[sx!=7]
    mean(sx,na.rm=T)
  })
})

dx.son.1st = apply(dx,2,function(X){
  sapply(X,function(Y){
    sx = strsplit(Y,";")[[1]]
    sx = unlist(sapply(sx,function(x){strsplit(x,"")[[1]][1]}))
    sx = sonority.score[sx]
    mean(sx,na.rm=T)
  })
})

dx.son.1st.cons = apply(dx,2,function(X){
  sapply(X,function(Y){
    sx = strsplit(Y,";")[[1]]
    sx = unlist(sapply(sx,function(x){
      segs = strsplit(x,"")[[1]]
      sxx = sonority.score[segs]
      sxx = sxx[!is.na(sxx)& sxx!=7]
      if(length(sxx)>0){
        return(sxx[1])
      } else{
        return(NA)
      }
      }))
    mean(sx,na.rm=T)
  })
})

# single column, one lang at a time
dx.son = data.frame(sonority=matrix(dx.son.all,ncol=1),sonority.cons=matrix(dx.son.cons,ncol=1), sonority.first = matrix(dx.son.1st,ncol=1), sonority.first.cons=matrix(dx.son.1st.cons, ncol=1))

dx.son$language = rep(colnames(d.wh.m),each=nrow(dx))
dx.son$wh = rep(c(rep(T,nrow(d.wh.m)),rep(F,nrow(d.random.m))),ncol(d.wh.m))

lang2glotto = tapply(alldata$glotto,alldata$Language,sample,size=1)

dx.son$glotto = lang2glotto[dx.son$language]
dx.son$family = l.details[match(dx.son$glotto,l.details$glotto),]$langFam
dx.son$area = l.details[match(dx.son$glotto,l.details$glotto),]$area
dx.son$area = l.details[match(dx.son$glotto,l.details$glotto),]$area
dx.son$initial = l.details[match(dx.son$glotto,l.details$glotto),]$possiblegrammars == "1 Initial interrogative phrase"

write.csv(dx.son, "../Results/SimplifiedPhonology/Sonority/Sonority.csv")

permX2 = function(x,y){
  r = sample(c(rep(1,length(x)),rep(2,length(y))))
  diff(tapply(c(x,y),r,mean,na.rm=T))
}
permX  = function(x,y,n=1000){
  replicate(n, permX2(x,y))
}


getDistDiff = function(dx,whx.r,rx.r){
  x = sample(c(whx.r,rx.r))
  whx.r = x[1:length(whx.r)]
  rx.r = x[(1:length(rx.r))+length(whx.r)]
  diff(c(mean(dx[rx.r,whx.r],na.rm=T),mean(dx[whx.r,whx.r],na.rm=T)))
}

editDistance = sapply(1:ncol(d.wh.m), function(i){
  whx = d.wh.m[,i]
  rx = d.random.m[,i]
  whx = sapply(whx,function(X){strsplit(X,";")[[1]][1]})
  rx = sapply(rx,function(X){strsplit(X,";")[[1]][1]})
  whx = whx[!is.na(whx)]
  rx = rx[!is.na(rx)]
  
  whx.r = 1:length(whx)
  rx.r = (1:length(rx))+length(whx)
  
  dx = adist(c(whx,rx),c(rx,rx))
  dx[diag(dx)] = NA # ignore self-self distances
  # if wh words are very different to other words, and other words are little different to other words, then trueDist is negative
  trueDist = diff(c(mean(dx[rx.r,whx.r],na.rm=T),mean(dx[whx.r,whx.r],na.rm=T)))
  permDist = replicate(1000,getDistDiff(dx,whx.r,rx.r))
  
  # we predict trueDist < permdist, 
  # if so, p is small and z is negative
  p = sum(trueDist>permDist)/length(permDist)
  if(p == 0){p=1/n}
  z = (trueDist - mean(permDist,na.rm=T))/sd(permDist,na.rm=T)
  return(c(p=p,z=z))  
  
#   edist = adist(c(whx,rx),c(whx,rx))
#   wdist = c(rep(0,length(whx)),rep(1,length(rx)))
#   wdist = abs(outer(wdist,wdist,"-"))
#   
#   # positive correlation means that the greater the difference in word type, the bigger the edit distance
#   
#   mt = mantel(as.dist(edist)~as.dist(wdist))
#   return(mt)          
#   dist = adist(whx,rx)
#   dist.all = adist(rx,rx)
#   dist.all[diag(dist.all)] = NA
#   whToOther = rowMeans(dist,na.rm=T)
#   otherToOther = rowMeans(dist.all,na.rm=T)
#   
#   n = 10
#   diff.mean = diff(c(mean(whToOther,na.rm=T),mean(otherToOther,na.rm=T)))
#   perm.diff = permX(whToOther,otherToOther,n)
#   
#   p = sum(perm.diff>diff.mean)/length(perm.diff)
#   if(p == 0){p=1/n}
#   z = (diff.mean - mean(perm.diff,na.rm=T))/sd(perm.diff,na.rm=T)
#   return(c(p,z))
})



edit.d = as.data.frame(t(editDistance))
#names(edit.d) = c("mantelr",'pval1','pval2','pval3','llim2.5%','ulim.97.5%')
edit.d$language = colnames(d.wh.m)[1:nrow(edit.d)]
edit.d$glotto = lang2glotto[edit.d$language]
edit.d$family = l.details[match(edit.d$glotto,l.details$glotto),]$langFam
edit.d$area = l.details[match(edit.d$glotto,l.details$glotto),]$area
edit.d$area = l.details[match(edit.d$glotto,l.details$glotto),]$area
edit.d$initial = l.details[match(edit.d$glotto,l.details$glotto),]$possiblegrammars == "1 Initial interrogative phrase"

write.csv(edit.d, "../Results/SimplifiedPhonology/Detectability/EditDistance.csv")
sum(edit.d$p < 0.05)
sum(edit.d$z<0)


###################
library(gplots)
plotmeans(sonority~ wh,data=dx.son)
plotmeans(sonority.cons~ wh,data=dx.son)
plotmeans(sonority.first~ wh,data=dx.son)
plotmeans(sonority.first.cons~ wh,data=dx.son)

plotmeans(sonority~ paste(initial,wh),data=dx.son[!is.na(dx.son$initial),])
plotmeans(sonority.cons~ paste(initial,wh),data=dx.son[!is.na(dx.son$initial),])

library(ggplot2)

wh.lang.mean = tapply(dx.son[dx.son$wh,]$sonority,dx.son[dx.son$wh,]$language,mean,na.rm=T)
non.wh.lang.mean = tapply(dx.son[!dx.son$wh,]$sonority,dx.son[!dx.son$wh,]$language,mean,na.rm=T)
lang.initial = tapply(dx.son[dx.son$wh,]$initial,dx.son[dx.son$wh,]$language,head,n=1)
lang.summaries = data.frame(sonority = c(wh.lang.mean,non.wh.lang.mean),wh=c(rep(T,nrow(wh.lang.mean)),rep(F,nrow(non.wh.lang.mean))), initial =  c(lang.initial,lang.initial))

p4 <- ggplot(lang.summaries[!is.na(lang.summaries$initial),], aes(factor(initial):factor(wh), sonority, fill=wh))
#pdf("Improvement_3conf.pdf", width = 12, height= 6)
p4 + geom_violin() + geom_boxplot(width=0.1, col=1) +
 # theme(text=element_text(size=20), legend.position="none") +
  scale_y_continuous(name="Sonority")+
  scale_x_discrete(name="")+
  scale_fill_grey(start = 0.55, end=0.8)

library(lme4)
m0 = lmer(sonority~ 1 + (1|language) + (1|family) + (1|area), data=dx.son)
m1 = lmer(sonority~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son)
anova(m0,m1)

m0 = lmer(sonority.cons~ 1 + (1|language) + (1|family) + (1|area), data=dx.son)
m1 = lmer(sonority.cons~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son)
anova(m0,m1)

m0 = lmer(sonority.first~ 1 + (1|language) + (1|family) + (1|area), data=dx.son)
m1 = lmer(sonority.first~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son)
anova(m0,m1)

m0 = lmer(sonority.first.cons~ 1 + (1|language) + (1|family) + (1|area), data=dx.son)
m1 = lmer(sonority.first.cons~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son)
anova(m0,m1)


# For initial languages only
m0 = lmer(sonority~ 1 + (1|language) + (1|family) + (1|area), data=dx.son[dx.son$initial,])
m1 = lmer(sonority~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son[dx.son$initial,])
anova(m0,m1)

m0 = lmer(sonority.cons~ 1 + (1|language) + (1|family) + (1|area), data=dx.son[dx.son$initial,])
m1 = lmer(sonority.cons~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son[dx.son$initial,])
anova(m0,m1)

m0 = lmer(sonority.first.cons~ 1 + (1|language) + (1|family) + (1|area), data=dx.son[dx.son$initial,])
m1 = lmer(sonority.first.cons~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son[dx.son$initial,])
anova(m0,m1)


# interactions with initial language
m0 = lmer(sonority~ 1 + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m1 = lmer(sonority~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m2 = lmer(sonority~ 1 + wh + initial + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m3 = lmer(sonority~ 1 + wh * initial + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
anova(m0,m1,m2,m3)


m0 = lmer(sonority.cons~ 1 + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m1 = lmer(sonority.cons~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m2 = lmer(sonority.cons~ 1 + wh + initial + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m3 = lmer(sonority.cons~ 1 + wh * initial + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
anova(m0,m1,m2,m3)


m0 = lmer(sonority.first.cons~ 1 + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m1 = lmer(sonority.first.cons~ 1 + wh + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m2 = lmer(sonority.first.cons~ 1 + wh + initial + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
m3 = lmer(sonority.first.cons~ 1 + wh * initial + (1|language) + (1|family) + (1|area), data=dx.son[!is.na(dx.son$initial),])
anova(m0,m1,m2,m3)