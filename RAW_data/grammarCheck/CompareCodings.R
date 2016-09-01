try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

x1=names2glotto[colnames(d.wh.m)]

gcheck = grammar[grammar$Glotto %in% c(x1),]

jp = read.csv("../RAW_data/grammarCheck/LanguagesToCheck_withReferenceAndPageNumbers_CodedByJP.csv", stringsAsFactors = F)


table(jp$Value..1...obligatory.initial..2...not.obligatory.initial..3...Mixed..some.initial.others.not.)

jp$jp.initial = NA
jp$jp.initial[jp$Value..1...obligatory.initial..2...not.obligatory.initial..3...Mixed..some.initial.others.not.==1] = "JP initial"
jp$jp.initial[jp$Value..1...obligatory.initial..2...not.obligatory.initial..3...Mixed..some.initial.others.not.==2] = "JP non-initial"
jp$jp.initial[jp$Value..1...obligatory.initial..2...not.obligatory.initial..3...Mixed..some.initial.others.not.==3] = "JP non-initial"


jp$jp.initial2 = "initial"
jp$jp.initial2[jp$jp.initial=="JP non-initial"] = "non-initial"
jp$jp.initial2[is.na(jp$jp.initial)] = NA

jp$as.initial = NA
jp$as.initial[jp$Glotto %in% initial.possible.glotto] = "AS initial"
jp$as.initial[jp$Glotto %in% non.initial.possible.glotto] = "AS non-initial"

jp$as.initial2 = "initial"
jp$as.initial2[jp$as.initial=="AS non-initial"] = "non-initial"
##############

table(jp$jp.initial, jp$as.initial)

disagree = jp[!is.na(jp$jp.initial) & !is.na(jp$as.initial) & jp$jp.initial2 != jp$as.initial2,]$Glotto

disagree[!is.na(disagree)]



jp[!is.na(jp$jp.initial) & !is.na(jp$as.initial) & jp$jp.initial2 != jp$as.initial2,c("Glotto","jp.initial2",'as.initial2')]

set.seed(10020)
rsamp = sample(jp$Glotto[!jp$Glotto %in% disagree], 6)

jp[jp$Glotto %in% rsamp,c("Glotto",'jp.initial2','as.initial2')]
