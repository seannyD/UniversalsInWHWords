try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

alldata = alldata[alldata$glotto %in% names2glotto[colnames(d.wh.m)],]

write.csv(alldata,"../Writeup/SupportingInformation/S2_LexicalData.csv", row.names = F)

write.csv(alldata,"../RAW_data/S2_LexicalData.csv", row.names = F)
