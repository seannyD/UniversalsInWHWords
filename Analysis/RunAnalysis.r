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
resultsFile = "../Results/SimplifiedPhonology/ResultsSummary.txt"
#Removes old info
cat("",file=resultsFile) # clear results file

number.of.perms = 10000
number.of.random.samples = 100# number of sets of random concept sets chosen in the comparison permutation for non-wh concepts.

set.seed(9999)
# perumatation test, specify variable and add a title, T-looks only at first character)
runPermutation(d.wh.m,"AllLangs_allSegments",F)
runPermutation(d.wh.m,"AllLangs_firstSegments",T)

set.seed(4785)
#Restricting by family
runPermutation(d.wh.m,"AllLangs_allSegments_byFamily",F,stratified=T,families.d.wh)
runPermutation(d.wh.m,"AllLangs_firstSegment_byFamily",T,T,families.d.wh)

set.seed(65)
#Restricting by area
runPermutation(d.wh.m,"AllLangs_allSegments_byArea",F,stratified=T,areas.d.wh)
runPermutation(d.wh.m,"AllLangs_firstSegment_byArea",T,T,areas.d.wh)

set.seed(6435)
#Restricting by area and family
runPermutation(d.wh.m,"AllLangs_allSegments_byAreaAndFamily",F,stratified=T,paste(families.d.wh,areas.d.wh))
runPermutation(d.wh.m,"AllLangs_firstSegment_byAreaAndFamily",T,T,paste(families.d.wh,areas.d.wh))


###########
# Compare differences between interrogative first and non-first

#WALS (1)
set.seed(7438)
runComparisonPermutation(d.wh.initial.m,d.wh.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_1_allSegments",F)
runComparisonPermutation(d.wh.initial.m,d.wh.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_1_firstSegments",T)

#Grammars (2)
set.seed(8000)
runComparisonPermutation(d.wh.grammar.initial.m,d.wh.grammar.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_2_allSegments",F)
runComparisonPermutation(d.wh.grammar.initial.m,d.wh.grammar.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_2_firstSegments",T)

#Possible (3)
# xxx
set.seed(657)
runComparisonPermutation(d.wh.possible.initial.m,d.wh.possible.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_3_allSegments",F)
runComparisonPermutation(d.wh.possible.initial.m,d.wh.possible.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_3_firstSegments",T)



runComparisonPermutationStratified(d.wh.possible.initial.m,d.wh.possible.non.initial.m,families.d.wh.possible.initial,families.d.wh.possible.non.initial, "InterrogativeOrder/InterrogativeOrder_PermuteFamilies_allSegments",F)
runComparisonPermutationStratified(d.wh.possible.initial.m,d.wh.possible.non.initial.m,families.d.wh.possible.initial,families.d.wh.possible.non.initial, "InterrogativeOrder/InterrogativeOrder_PermuteFamilies_firstSegments",T)


runComparisonPermutationStratified(d.wh.possible.initial.m,d.wh.possible.non.initial.m,areas.d.wh.possible.initial,areas.d.wh.possible.non.initial, "InterrogativeOrder/InterrogativeOrder_PermuteAreas_allSegments",F)
runComparisonPermutationStratified(d.wh.possible.initial.m,d.wh.possible.non.initial.m,areas.d.wh.possible.initial,areas.d.wh.possible.non.initial, "InterrogativeOrder/InterrogativeOrder_PermuteAreas_firstSegments",T)

runComparisonPermutationStratified(d.wh.possible.initial.m,d.wh.possible.non.initial.m,paste(families.d.wh.possible.initial,areas.d.wh.possible.initial),paste(families.d.wh.possible.non.initial,areas.d.wh.possible.non.initial), "InterrogativeOrder/InterrogativeOrder_PermuteFamiliesAndAreas_allSegments",F)
runComparisonPermutationStratified(d.wh.possible.initial.m,d.wh.possible.non.initial.m,paste(families.d.wh.possible.initial,areas.d.wh.possible.initial),paste(families.d.wh.possible.non.initial,areas.d.wh.possible.non.initial), "InterrogativeOrder/InterrogativeOrder_PermuteFamiliesAndAreas_firstSegments",T)

###############
# Random concepts

resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_RandomConcepts.txt"
#Removes old info
cat("",file=resultsFile) # clear results file


set.seed(4485)
# example: run comparison with random concepts
runComparison.wh.random.permutation(d.wh.m,d.random.m,"RandomConcepts/Comparison_WH_Random_allSegments",F)
runComparison.wh.random.permutation(d.wh.m,d.random.m,"RandomConcepts/Comparison_WH_Random_firstSegments",T)


# example: run within domains 
runComparison.wh.domain.permutation(d.wh.m, d.random.m, "RandomConcepts/Comparison_WH_Domain_allSegments",F)
runComparison.wh.domain.permutation(d.wh.m, d.random.m, "RandomConcepts/Comparison_WH_Domain_firstSegments",T)



# run within specific domains (pre-selected) - body parts, basic actions
set.seed(4731)
runPermutation(d.BodyConcepts.m,"AllLangs_allSegments_BodyDomain",F)
runPermutation(d.BodyConcepts.m,"AllLangs_firstSegments_BodyDomain",T)

runPermutation(d.BasicActionsConcepts.m,"AllLangs_allSegments_ActionDomain",F)
runPermutation(d.BasicActionsConcepts.m,"AllLangs_firstSegments_ActionDomain",T)


set.seed(13897439)
runPermutation(d.BodyConcepts.m,"AllLangs_allSegments_BodyDomain_byFamilyAndArea",F,stratified=T,paste(families.d.wh,areas.d.wh))
runPermutation(d.BodyConcepts.m,"AllLangs_firstSegments_BodyDomain_byFamilyAndArea",T,stratified=T,paste(families.d.wh,areas.d.wh))
runPermutation(d.BasicActionsConcepts.m,"AllLangs_allSegments_BasicActionsDomain_byFamilyAndArea",F,stratified=T,paste(families.d.wh,areas.d.wh))
runPermutation(d.BasicActionsConcepts.m,"AllLangs_firstSegments_BasicActionsDomain_byFamilyAndArea",T,stratified=T,paste(families.d.wh,areas.d.wh))


set.seed(478445)
runComparisonPermutation(d.wh.m,d.BodyConcepts.m,"Comparison_WH_BodyDomain_allSegments",F)
runComparisonPermutation(d.wh.m,d.BodyConcepts.m,"Comparison_WH_BodyDomain_firstSegments",T)
    runComparisonPermutation(d.wh.m,d.BasicActionsConcepts.m,"Comparison_WH_BasicActionsDomain_allSegments",F)
runComparisonPermutation(d.wh.m,d.BasicActionsConcepts.m,"Comparison_WH_BasicActionsDomain_firstSegments",T)

# Run comparison between initial and non-initial languages, for random concepts
# (runs both first segments and all segments)
set.seed(454785)
runRandomComparison(d.random.initial.m,d.random.non.initial.m, number.of.random.samples=number.of.random.samples, "RandomInterrogativeOrder/RandomInitial_1")
#Grammars
runRandomComparison(d.random.grammar.initial.m,d.random.grammar.non.initial.m, number.of.random.samples=number.of.random.samples, "RandomInterrogativeOrder/RandomInitial_2")
#Possible
runRandomComparison(d.random.possible.initial.m,d.random.possible.non.initial.m, number.of.random.samples=number.of.random.samples, "RandomInterrogativeOrder/RandomInitial_3")


# compare actual wh words with random words in initial languages

set.seed(322378)

runComparison.wh.random.permutation(d.wh.possible.initial.m, d.random.possible.initial.m, "RandomConcepts/Comparison_Initial_WH_Random_All_allSegments",F)
runComparison.wh.random.permutation(d.wh.possible.initial.m, d.random.possible.initial.m,"RandomConcepts/Comparison_Initial_WH_Random_All_firstSegments",T)

runComparison.wh.random.permutation(d.wh.possible.non.initial.m, d.random.possible.non.initial.m, "RandomConcepts/Comparison_NonInitial_WH_Random_All_allSegments",F)
runComparison.wh.random.permutation(d.wh.possible.non.initial.m, d.random.possible.non.initial.m,"RandomConcepts/Comparison_NonInitial_WH_Random_All_firstSegments",T)

set.seed(3278)

runComparison.wh.domain.permutation(d.wh.possible.initial.m, d.random.possible.initial.m, "RandomConcepts/Comparison_Initial_WH_Random_allSegments",F)
runComparison.wh.domain.permutation(d.wh.possible.initial.m, d.random.possible.initial.m,"RandomConcepts/Comparison_Initial_WH_Random_firstSegments",T)

runComparison.wh.domain.permutation(d.wh.possible.non.initial.m, d.random.possible.non.initial.m, "RandomConcepts/Comparison_NonInitial_WH_Random_allSegments",F)
runComparison.wh.domain.permutation(d.wh.possible.non.initial.m, d.random.possible.non.initial.m,"RandomConcepts/Comparison_NonInitial_WH_Random_firstSegments",T)





################
# Compare vowels/consonants seperately

#Specifes where Text file with results goes
resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_Consonants_Vowels.txt"
#Removes old info
cat("",file=resultsFile) # clear results file

# Vowels
set.seed(1657)
runPermutation(d.wh.vowelsOnly.m ,"AllLangs_Vowels_allSegments",F)
runPermutation(d.wh.vowelsOnly.m ,"AllLangs_Vowels_firstSegments",T)

# Consonants
set.seed(98765)
runPermutation(d.wh.consonantsOnly.m ,"AllLangs_Consonants_allSegments",F)
runPermutation(d.wh.consonantsOnly.m ,"AllLangs_Consonants_firstSegments",T)

set.seed(71138)
runComparisonPermutation(d.wh.vowelsOnly.m,d.wh.consonantsOnly.m,"VowelsVersusConsonantsDifference_allSegments",F)
runComparisonPermutation(d.wh.vowelsOnly.m,d.wh.consonantsOnly.m,"VowelsVersusConsonantsDifference_firstSegments",T)

# Compare initial /non initial looking at only consonants / vowels

#WALS consonants
set.seed(4789)
runComparisonPermutation(d.wh.initial.consonantsOnly.m,d.wh.non.initial.consonantsOnly.m,"InterrogativeOrder/ConsonantsInitial_1_allSegments",F)
runComparisonPermutation(d.wh.initial.consonantsOnly.m,d.wh.non.initial.consonantsOnly.m,"InterrogativeOrder/ConsonantsInitial_1_firstSegments",T)

#Grammar consonants

runComparisonPermutation(d.wh.grammar.initial.consonantsOnly.m,d.wh.grammar.non.initial.consonantsOnly.m,"InterrogativeOrder/ConsonantsInitial_2_allSegments",F)
runComparisonPermutation(d.wh.grammar.initial.consonantsOnly.m,d.wh.grammar.non.initial.consonantsOnly.m,"InterrogativeOrder/ConsonantsInitial_2_firstSegments",T)

#Possible consonants

runComparisonPermutation(d.wh.possible.initial.consonantsOnly.m,d.wh.possible.non.initial.consonantsOnly.m,"InterrogativeOrder/ConsonantsInitial_3_allSegments",F)
runComparisonPermutation(d.wh.possible.initial.consonantsOnly.m,d.wh.possible.non.initial.consonantsOnly.m,"InterrogativeOrder/ConsonantsInitial_3_firstSegments",T)


set.seed(1789)
#WALS vowels

runComparisonPermutation(d.wh.initial.vowelsOnly.m,d.wh.non.initial.vowelsOnly.m,"InterrogativeOrder/VowelsInitial_1_allSegments",F)
runComparisonPermutation(d.wh.initial.vowelsOnly.m,d.wh.non.initial.vowelsOnly.m,"InterrogativeOrder/VowelsInitial_1_firstSegments",T)


#Grammar vowels

runComparisonPermutation(d.wh.grammar.initial.vowelsOnly.m,d.wh.grammar.non.initial.vowelsOnly.m,"InterrogativeOrder/VowelsInitial_2_allSegments",F)
runComparisonPermutation(d.wh.grammar.initial.vowelsOnly.m,d.wh.grammar.non.initial.vowelsOnly.m,"InterrogativeOrder/VowelsInitial_2_firstSegments",T)

#Possible vowels

runComparisonPermutation(d.wh.possible.initial.vowelsOnly.m,d.wh.possible.non.initial.vowelsOnly.m,"InterrogativeOrder/VowelsInitial_3_allSegments",F)
runComparisonPermutation(d.wh.possible.initial.vowelsOnly.m,d.wh.possible.non.initial.vowelsOnly.m,"InterrogativeOrder/VowelsInitial_3_firstSegments",T)



############################
# Using only unanalyzable words

resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_UnanalyzableWords.txt"
#Removes old info
cat("",file=resultsFile) # clear results file


set.seed(99199)
# perumatation test, specify variable and add a title, T-looks only at first character)
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_allSegments",F)
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_firstSegments",T)

set.seed(47185)
#Restricting by family
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_allSegments_byFamily",F,stratified=T,families.d.unanalyzable.wh)
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_firstSegment_byFamily",T,T,families.d.unanalyzable.wh)

set.seed(615)
#Restricting by area
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_allSegments_byArea",F,stratified=T,areas.d.unanalyzable.wh)
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_firstSegment_byArea",T,T,areas.d.unanalyzable.wh)

set.seed(64135)
#Restricting by area and family
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_allSegments_byAreaAndFamily",F,stratified=T,paste(families.d.unanalyzable.wh,areas.d.unanalyzable.wh))
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_firstSegment_byAreaAndFamily",T,T,paste(families.d.unanalyzable.wh,areas.d.unanalyzable.wh))

# compare initial and non-initial
runComparisonPermutation(d.unanalyzable.wh.initial.m,d.unanalyzable.wh.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_unanalyzable_3_allSegments",F)
runComparisonPermutation(d.unanalyzable.wh.initial.m,d.unanalyzable.wh.non.initial.m,"InterrogativeOrder/InterrogativeOrderDifference_unanalyzable_3_firstSegments",T)


# Compare with random concepts
set.seed(4485)
runComparison.wh.random.permutation(d.unanalyzable.wh.m,d.random.unanalyzable.m,"RandomConcepts/Comparison_WH_Random_unanalyzable_allSegments",F)
runComparison.wh.random.permutation(d.unanalyzable.wh.m,d.random.unanalyzable.m,"RandomConcepts/Comparison_WH_Random_unanalyzable_firstSegments",T)


# run comparison with random concepts within domains 
runComparison.wh.domain.permutation(d.unanalyzable.wh.m, d.random.unanalyzable.m, "RandomConcepts/Comparison_WH_Domain_unanalyzable_allSegments",F)
runComparison.wh.domain.permutation(d.unanalyzable.wh.m, d.random.unanalyzable.m, "RandomConcepts/Comparison_WH_Domain_unanalyzable_firstSegments",T)


##########################
# Do permutation tests with random concepts

number.of.perms = 500
number.of.random.samples = 100

resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_RandomPermutations.txt"

runRandomPermutation(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_allSegments",firstSegment=F)

runRandomPermutation(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_firstSegments",firstSegment=T)



number.of.perms = 100
number.of.random.samples = 20


runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_firstSegments",T,stratified=T,families=families.d.wh)

runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_allSegments",F,stratified=T,families=families.d.wh)

# families and areas
setseed(923)
runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_and_Area_firstSegments",T,stratified=T,families=paste(families.d.wh,areas.d.wh))

runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_and_Area_allSegments",F,stratified=T,families=paste(families.d.wh,areas.d.wh))


