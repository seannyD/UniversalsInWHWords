try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

####################################
# Run the permutations#
####################################

#Specifes where Text file with results goes
resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_RandomIndependentSamples.txt"
#Removes old info
cat("permName\tDiffMoreThanZero\tMeanDiff\tNumberOfSamples\tz",file=resultsFile) # clear results file

number.of.perms = 20000
number.of.random.samples = 100# number of sets of random concept sets chosen in the comparison permutation for non-wh concepts.


set.seed(45678)
# Random independent samples
runComparison.randomSample(d.wh.possible.initial.m,d.wh.possible.non.initial.m,families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_allSegments",F)

runComparison.randomSample(d.wh.possible.initial.m,d.wh.possible.non.initial.m,families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_firstSegments",T)

runComparison.randomSample(d.wh.possible.initial.m,d.wh.possible.non.initial.m,areas.d.wh.possible.initial,areas.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_area_allSegments",F)
runComparison.randomSample(d.wh.possible.initial.m,d.wh.possible.non.initial.m,areas.d.wh.possible.initial,areas.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_area_firstSegments",T)

hist(getWordListEntropy(d.wh.possible.initial.m,T), col=rgb(1,0,0,0.3),border=F,xlim=c(0,1),breaks=20)
hist(getWordListEntropy(d.wh.possible.non.initial.m,T),add=T, col=rgb(0,1,0,0.3),border=F,xlim=c(0,1),breaks=20)




##########
# Random independent samples with random concepts

resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_RandomIndependentSamples_RandomConcepts.txt"
cat("permName\tDiffMoreThanZero\tMeanDiff\tNumberOfSamples\tz",file=resultsFile) # clear results file

# Body concepts
set.seed(732)
runComparison.randomSample(d.BodyConcepts.initial.m, d.BodyConcepts.non.initial.m, families.d.BodyConcepts.initial, families.d.BodyConcepts.non.initial, "RIS_BodyConcepts_allSegments_Family", F)
runComparison.randomSample(d.BodyConcepts.initial.m, d.BodyConcepts.non.initial.m, families.d.BodyConcepts.initial, families.d.BodyConcepts.non.initial, "RIS_BodyConcepts_firstSegments_Family", T)

runComparison.randomSample(d.BodyConcepts.initial.m, d.BodyConcepts.non.initial.m, areas.d.BodyConcepts.initial, areas.d.BodyConcepts.non.initial, "RIS_BodyConcepts_allSegments_Area", F)
runComparison.randomSample(d.BodyConcepts.initial.m, d.BodyConcepts.non.initial.m, areas.d.BodyConcepts.initial, areas.d.BodyConcepts.non.initial, "RIS_BodyConcepts_firstSegments_Area", T)

# Action concepts
set.seed(1121)
runComparison.randomSample(d.BasicActionsConcepts.initial.m, d.BasicActionsConcepts.non.initial.m, families.d.BasicActions.initial, families.d.BasicActions.non.initial, "RIS_BasicActionsConcepts_allSegments_Family", F)
runComparison.randomSample(d.BasicActionsConcepts.initial.m, d.BasicActionsConcepts.non.initial.m, families.d.BasicActions.initial, families.d.BasicActions.non.initial, "RIS_BasicActionsConcepts_firstSegments_Family", T)

runComparison.randomSample(d.BasicActionsConcepts.initial.m, d.BasicActionsConcepts.non.initial.m, areas.d.BasicActions.initial, areas.d.BasicActions.non.initial, "RIS_BasicActionsConcepts_allSegments_Area", F)
runComparison.randomSample(d.BasicActionsConcepts.initial.m, d.BasicActionsConcepts.non.initial.m, areas.d.BasicActions.initial, areas.d.BasicActions.non.initial, "RIS_BasicActionsConcepts_firstSegments_Area", T)

# Pronouns
set.seed(7382)
runComparison.randomSample(d.PronounConcepts.initial.m, d.PronounConcepts.non.initial.m, families.d.PronounConcepts.initial, families.d.PronounConcepts.non.initial, "RIS_PronounConcepts_allSegments_Family", F)
runComparison.randomSample(d.PronounConcepts.initial.m, d.PronounConcepts.non.initial.m, families.d.PronounConcepts.initial, families.d.PronounConcepts.non.initial, "RIS_PronounConcepts_firstSegments_Family", T)

runComparison.randomSample(d.PronounConcepts.initial.m, d.PronounConcepts.non.initial.m, areas.d.PronounConcepts.initial, areas.d.PronounConcepts.non.initial, "RIS_PronounConcepts_allSegments_Area", F)
runComparison.randomSample(d.PronounConcepts.initial.m, d.PronounConcepts.non.initial.m, areas.d.PronounConcepts.initial, areas.d.PronounConcepts.non.initial, "RIS_PronounConcepts_firstSegments_Area", T)




# Random concepts
number.of.perms = 1000
number.of.random.samples = 50


set.seed(87)
runComparison.RandomIndependentSample.RandomConcepts(d.random.possible.initial.m, d.random.possible.non.initial.m, families.d.wh.possible.initial, families.d.wh.possible.non.initial, "RandomIndependentSamples/RIS_RandomConcepts_allSegments", F)
runComparison.RandomIndependentSample.RandomConcepts(d.random.possible.initial.m, d.random.possible.non.initial.m, families.d.wh.possible.initial, families.d.wh.possible.non.initial, "RandomIndependentSamples/RIS_RandomConcepts_firstSegments", T)

set.seed(2378)
runComparison.RandomIndependentSample.RandomConcepts_Domain(d.random.possible.initial.m, d.random.possible.non.initial.m, families.d.wh.possible.initial, families.d.wh.possible.non.initial, "RandomIndependentSamples/RIS_RandomConcepts_Domain_allSegments", F)
runComparison.RandomIndependentSample.RandomConcepts_Domain(d.random.possible.initial.m, d.random.possible.non.initial.m, families.d.wh.possible.initial, families.d.wh.possible.non.initial, "RandomIndependentSamples/RIS_RandomConcepts_Domain_firstSegments", T)



number.of.perms = 10000
number.of.random.samples = 100# number of sets of random concept sets chosen in the comparison permutation for non-wh concepts.


# Unanalysable

runComparison.randomSample(d.unanalyzable.wh.initial.m,d.unanalyzable.wh.non.initial.m, families.d.unanalyzable.wh.initial, families.d.unanalyzable.wh.non.initial,"RandomIndependentSamples/RIS_WH_Unanalyzable_Family_allSegments",F)
runComparison.randomSample(d.unanalyzable.wh.initial.m,d.unanalyzable.wh.non.initial.m, families.d.unanalyzable.wh.initial, families.d.unanalyzable.wh.non.initial,"RandomIndependentSamples/RIS_WH_Unanalyzable_Family_firstSegments",T)

runComparison.randomSample(d.unanalyzable.wh.initial.m,d.unanalyzable.wh.non.initial.m, areas.d.unanalyzable.wh.initial, areas.d.unanalyzable.wh.non.initial,"RandomIndependentSamples/RIS_WH_Unanalyzable_Area_allSegments",F)
runComparison.randomSample(d.unanalyzable.wh.initial.m,d.unanalyzable.wh.non.initial.m, areas.d.unanalyzable.wh.initial, areas.d.unanalyzable.wh.non.initial,"RandomIndependentSamples/RIS_WH_Unanalyzable_Area_firstSegments",T)


# Consonants and vowels
runComparison.randomSample(d.wh.possible.initial.consonantsOnly.m,d.wh.possible.non.initial.consonantsOnly.m, families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/ConsonantsInitial_3_allSegments_RandomIndependentSample",F)

runComparison.randomSample(d.wh.possible.initial.consonantsOnly.m,d.wh.possible.non.initial.consonantsOnly.m, families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/ConsonantsInitial_3_firstSegments_RandomIndependentSample",T)


runComparison.randomSample(d.wh.possible.initial.vowelsOnly.m,d.wh.possible.non.initial.vowelsOnly.m, families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/VowelsInitial_3_allSegments_RandomIndependentSample",F)

#rx
runComparison.randomSample(d.wh.possible.initial.vowelsOnly.m,d.wh.possible.non.initial.vowelsOnly.m, families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/VowelsInitial_3_firstSegments_RandomIndependentSample",T)




######
# LMER analysis of initial vs non-initial languages

initial.e = getWordListEntropy(d.wh.possible.initial.m, T)
non.initial.e = getWordListEntropy(d.wh.possible.non.initial.m, T)

dx = data.frame(e = c(initial.e,non.initial.e),
                qpos = c(rep('initial',length(initial.e)),rep('non.initial',length(non.initial.e))),
                family = c(families.d.wh.possible.initial,families.d.wh.possible.non.initial),
                area = c(areas.d.wh.possible.initial, areas.d.wh.possible.non.initial)
)

dx$e.norm = dx$e - mean(dx$e)


library(lme4)

m0 = lmer(e.norm ~ 1 + (1|family) + (1|area), data=dx)
m1 = lmer(e.norm ~ 1 + qpos + (1|family) + (1|area), data=dx)
anova(m0,m1)
