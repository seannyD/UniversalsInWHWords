try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")


wh.ent = getWordListEntropy(d.wh.m, T)


l.details$lang.verbPos = as.numeric(sapply(strsplit(as.character(l.details$basicWordOrder)," "),head,n=1))
l.details$lang.verbPos = l.details$lang.verbPos -1

l.details$wh.ent = wh.ent[l.details$Language]
l.details$wh.ent.norm = l.details$wh.ent - mean(l.details$wh.ent)

library(gplots)
sel = !is.na(l.details$qpos) & l.details$qpos!='3 Mixed'
plotmeans(l.details[sel,]$wh.ent ~ l.details[sel,]$qpos)

m0 = lmer(wh.ent.norm~ 1 + (1 +qpos|langFam) + (1 +qpos| area), data = l.details[sel,])
m1 = lmer(wh.ent.norm~ 1 + qpos + (1+qpos |langFam) + (1 +qpos| area), data = l.details[sel,])
anova(m0,m1)
summary(m1)
dotplot(ranef(m1))

########
plotmeans(l.details$wh.ent~l.details$basicWordOrder)

l.details2 = l.details[complete.cases(l.details$basicWordOrder),]
l.details2 = l.details2[!l.details2$basicWordOrder %in% c("5 OVS",'4 VOS','7 No dominant order'),]

library(lme4)
m0 = lmer(wh.ent.norm ~ 1+  (1 | langFam) + (1 | area),data=l.details2)
m1 = lmer(wh.ent.norm ~ basicWordOrder + (1 | langFam) + (1 | area), data=l.details2)
anova(m0,m1)
summary(m1)


m0 = lmer(wh.ent.norm ~ 1+  (1 +lang.verbPos| langFam) + (1 +lang.verbPos| area),data=l.details2)
m1 = lmer(wh.ent.norm ~ lang.verbPos + (1 +lang.verbPos| langFam) + (1+lang.verbPos | area), data=l.details2)
anova(m0,m1)
summary(m1)
library(sjPlot)

sjp.lmer(m1,'eff')

l.details3 = l.details2[!is.na(l.details2$qpos) & l.details2$qpos!='3 Mixed',]

m0 = lmer(wh.ent.norm ~ 1+  (1 | langFam) + (1 | area),data=l.details3)
m1 = lmer(wh.ent.norm ~ lang.verbPos + (1 | langFam) + (1 | area), data=l.details3)
m2 = lmer(wh.ent.norm ~ lang.verbPos + qpos + (1 | langFam) + (1 | area), data=l.details3)
m3 = lmer(wh.ent.norm ~ lang.verbPos * qpos + (1 | langFam) + (1 | area), data=l.details3)
anova(m0,m1,m2,m3)
summary(m3)

plotmeans(wh.ent~paste(basicWordOrder,qpos),data=l.details3)


