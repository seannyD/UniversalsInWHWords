library(lme4)

try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")



e = data.frame(wh.E = getWordListEntropy(d.wh.m,T))
e$lang = names(getWordListEntropy(d.wh.m,T))
e$glotto = names2glotto[colnames(d.wh.m)]
e$all.E = getWordListEntropy(d.random.m,T)
e$fam = families.d.wh
e$area = areas.d.wh
e$wh.pos = l.details[match(e$glotto,l.details$glotto),]$possiblegrammars
e$wh.init = e$wh.pos=="1 Initial interrogative phrase"

e = e[!is.na(e$wh.pos),]
e = e[e$wh.E > 0.2,]

m0 = lmer(wh.E ~ 1 + (1 | fam) + (1   | area),data=e)
m1 = lmer(wh.E ~ wh.init + (1 | fam) + (1  | area),data=e)
anova(m0,m1)

m2 = lmer(wh.E ~ 1 + all.E + (1 | fam) + (1 | area),data=e)
m3 = lmer(wh.E ~ 1 + wh.init + all.E + (1 | fam) + (1 | area),data=e)
m4 = lmer(wh.E ~ 1 + wh.init*all.E + (1 | fam) + (1 | area),data=e)
anova(m2,m3,m4)

interaction.plot(e$wh.init,cut(e$all.E,quantile(e$all.E,seq(0,1,length.out=5))),e$wh.E)
library(lattice)
xyplot(e$wh.E~e$all.E,groups=e$wh.init, type=c('p','r'), auto.key=T)
