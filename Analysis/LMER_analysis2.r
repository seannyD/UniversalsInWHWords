library(lme4)
try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")


e.initial = getWordListEntropy(d.wh.possible.initial.m,firstSegment=T)
e.non.initial = getWordListEntropy(d.wh.possible.non.initial.m,firstSegment=T)

dx = data.frame(lang=c(names(e.initial),names(e.non.initial)),
    e=c(e.initial,e.non.initial),
    family=c(families.d.wh.possible.initial, families.d.wh.possible.non.initial),
    area = c(areas.d.wh.possible.initial,areas.d.wh.possible.non.initial),
    initial = c(rep(T,length(e.initial)),rep(F,length(e.non.initial))))

plotmeans(e~initial,data=dx)

m0 = lmer(e~1 + (1|area) , data =dx)
m1 = lmer(e~1 + initial + (1|area) , data =dx)
anova(m0,m1)


######


e.initial = getWordListEntropy(d.unanalyzable.wh.initial.m,firstSegment=T)
e.non.initial = getWordListEntropy(d.unanalyzable.wh.non.initial.m,firstSegment=T)

dx = data.frame(lang=c(names(e.initial),names(e.non.initial)),
                e=c(e.initial,e.non.initial),
                family=c(families.d.unanalyzable.wh.initial, families.d.unanalyzable.wh.non.initial),
                area = c(areas.d.unanalyzable.wh.initial,areas.d.unanalyzable.wh.non.initial),
                initial = c(rep(T,length(e.initial)),rep(F,length(e.non.initial))))

plotmeans(e~initial,data=dx)

m0 = lmer(e~1 + (1|area) , data =dx)
m1 = lmer(e~1 + initial + (1|area) , data =dx)
anova(m0,m1)
