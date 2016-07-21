try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

d.wh.glotto = names2glotto[colnames(d.wh.m)]


l.details = l.details[l.details$glotto %in% d.wh.glotto,]
library(maps)

map()
points(l.details$longitude,l.details$latitude,col=rgb(1,0,0),  pch= 16)