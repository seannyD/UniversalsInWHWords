try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

#############

allLangN  = 500
domainLangN = 100

number.of.random.samples = allLangN

set.seed(3478)
compareWordsWithinLanguage(d.wh.m, d.random.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_AllSegments.csv" , F)

compareWordsWithinLanguage(d.wh.m, d.random.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_FirstSegments.csv" , T)


number.of.random.samples = domainLangN
set.seed(322)
compareWordsWithinLanguage_Domain(d.wh.m, d.random.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_AllSegments_Domain" , F)

compareWordsWithinLanguage_Domain(d.wh.m, d.random.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_FirstSegments_Domain" , T)


#############
number.of.random.samples = allLangN
set.seed(563)
compareWordsWithinLanguage(d.unanalyzable.wh.m, d.random.unanalyzable.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_unanalyzable_AllSegments.csv" , F)
set.seed(38)
compareWordsWithinLanguage(d.unanalyzable.wh.m, d.random.unanalyzable.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_unanalyzable_FirstSegments.csv" , T)

number.of.random.samples = domainLangN
set.seed(939)
compareWordsWithinLanguage_Domain(d.unanalyzable.wh.m, d.random.unanalyzable.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_unanalyzable_AllSegments_Domain" , F)
set.seed(444)
compareWordsWithinLanguage_Domain(d.unanalyzable.wh.m, d.random.unanalyzable.m,"../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_unanalyzable_FirstSegments_Domain" , T)
