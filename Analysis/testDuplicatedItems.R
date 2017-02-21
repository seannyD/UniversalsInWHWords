try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")


alldata.wh = alldata[alldata$meaning.id.fixed %in% whwords,]

# Remove duplicate wh words within languages
d.wh.noDuplicates = by(alldata.wh[,c("word.clean",'word.simple')], alldata.wh$Language, function(X){
  clean = unlist(strsplit(X[,1],";"))
  simple = unlist(strsplit(X[,2],";"))
  paste(simple[which(!duplicated(clean))],collapse=";")
})

d.wh.noDuplicates = rbind(
  t(as.matrix(d.wh.noDuplicates)),
  rep(NA, length(d.wh.noDuplicates)) # fake row to stop the matrix collapsing
)

d.wh.noDuplicates.initial = d.wh.noDuplicates[,colnames(d.wh.possible.initial.m)]
d.wh.noDuplicates.non.initial = d.wh.noDuplicates[,colnames(d.wh.possible.non.initial.m)]

#########
number.of.perms = 10000
number.of.random.samples = 100# number of sets of random concept sets chosen in the comparison permutation for non-wh concepts.

#Specifes where Text file with results goes
resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_noDuplicates.txt"
#Removes old info
cat("",file=resultsFile) # clear results file


mean(getWordListEntropy(d.wh.m, firstSegment=F))
mean(getWordListEntropy(d.wh.m, firstSegment=T))

mean(getWordListEntropy(d.wh.noDuplicates, firstSegment=F))
mean(getWordListEntropy(d.wh.noDuplicates, firstSegment=T))

set.seed(9999)
# XXX TODO These don't make sense because there's only one row
#runPermutation(d.wh.noDuplicates,"AllLangs_allSegments_noDuplicates",F)
#runPermutation(d.wh.noDuplicates,"AllLangs_firstSegments_noDuplicates",T)


runComparison.wh.random.permutation(d.wh.noDuplicates,d.random.m,"RandomConcepts/Comparison_WH_Random_allSegments_noDuplicates",F)
runComparison.wh.random.permutation(d.wh.noDuplicates,d.random.m,"RandomConcepts/Comparison_WH_Random_firstSegments_noDuplicates",T)


resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_RIS_noDuplicates.txt"
#Removes old info
cat("",file=resultsFile) # clear results file

number.of.perms = 20000
number.of.random.samples = 100

set.seed(327823)

runComparison.randomSample(d.wh.noDuplicates.initial,d.wh.noDuplicates.non.initial,families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_allSegments_noDuplicates",F)

runComparison.randomSample(d.wh.noDuplicates.initial,d.wh.noDuplicates.non.initial,families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_firstSegments_noDuplicates",T)


set.seed(32)
# Areas
runComparison.randomSample(
  d.wh.noDuplicates.initial,
  d.wh.noDuplicates.non.initial,
  areas.d.wh.possible.initial,
  areas.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_allSegments_Areas_noDuplicates",F)

runComparison.randomSample(
  d.wh.noDuplicates.initial,
  d.wh.noDuplicates.non.initial,
  areas.d.wh.possible.initial,
  areas.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_firstSegments_Areas_noDuplicates",T)
