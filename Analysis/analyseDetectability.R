try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))

detect.d = read.csv("../Results/SimplifiedPhonology/Detectability/RandomConcepts/detectability_randomConcepts_firstSegments.csv", stringsAsFactors=F)

mean(detect.d$z)

sum(detect.d$z<0)
sum(detect.d$z<0)/nrow(detect.d)
mean(detect.d$z)
sum(detect.d$p>0.95 & detect.d$z<0)
sum(detect.d$p>0.95 & detect.d$z<0)/nrow(detect.d)


