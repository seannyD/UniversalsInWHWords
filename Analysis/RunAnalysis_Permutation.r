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

# #Specifes where Text file with results goes
resultsFile = "../Results/SimplifiedPhonology/ResultsSummary.txt"
# #Removes old info
cat("",file=resultsFile) # clear results file
# 
# 
number.of.perms = 10000
number.of.random.samples = 100# number of sets of random concept sets chosen in the comparison permutation for non-wh concepts.
# 
#check
sort(apply(d.wh.m,2,function(X){sum(is.na(X)| nchar(X)==0)}))

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

runPermutation(d.PronounConcepts.m,"AllLangs_allSegments_Pronouns",F)
runPermutation(d.PronounConcepts.m,"AllLangs_firstSegments_Pronouns",T)


set.seed(13897439)
runPermutation(d.BodyConcepts.m,"AllLangs_allSegments_BodyDomain_byFamilyAndArea",F,stratified=T,paste(families.d.wh,areas.d.wh))
runPermutation(d.BodyConcepts.m,"AllLangs_firstSegments_BodyDomain_byFamilyAndArea",T,stratified=T,paste(families.d.wh,areas.d.wh))
runPermutation(d.BasicActionsConcepts.m,"AllLangs_allSegments_BasicActionsDomain_byFamilyAndArea",F,stratified=T,paste(families.d.wh,areas.d.wh))
runPermutation(d.BasicActionsConcepts.m,"AllLangs_firstSegments_BasicActionsDomain_byFamilyAndArea",T,stratified=T,paste(families.d.wh,areas.d.wh))

set.seed(13897439)
runPermutation(d.PronounConcepts.m,"AllLangs_allSegments_PronounDomain_byFamilyAndArea",F,stratified=T,
               paste(families.d.wh[names2glotto[colnames(d.PronounConcepts.m)]],
                     areas.d.wh[names2glotto[colnames(d.PronounConcepts.m)]]))
runPermutation(d.PronounConcepts.m,"AllLangs_firstSegments_PronounDomain_byFamilyAndArea",T,stratified=T,
               paste(families.d.wh[names2glotto[colnames(d.PronounConcepts.m)]],
                     areas.d.wh[names2glotto[colnames(d.PronounConcepts.m)]]))

###########



#runPermutation(d.PronounConcepts.m,"AllLangs_allSegments_PronounDomain_byFamilyAndArea",F,stratified=T,paste(families.d.wh,areas.d.wh))
#runPermutation(d.PronounConcepts.m,"AllLangs_firstSegments_PronounDomain_byFamilyAndArea",T,stratified=T,paste(families.d.wh,areas.d.wh))

set.seed(478445)
runComparisonPermutation(d.wh.m,d.BodyConcepts.m,"Comparison_WH_BodyDomain_allSegments",F)
runComparisonPermutation(d.wh.m,d.BodyConcepts.m,"Comparison_WH_BodyDomain_firstSegments",T)
runComparisonPermutation(d.wh.m,d.BasicActionsConcepts.m,"Comparison_WH_BasicActionsDomain_allSegments",F)
runComparisonPermutation(d.wh.m,d.BasicActionsConcepts.m,"Comparison_WH_BasicActionsDomain_firstSegments",T)

runComparisonPermutation(d.wh.m,d.PronounConcepts.m,"Comparison_WH_PronounDomain_allSegments",F)
runComparisonPermutation(d.wh.m,d.PronounConcepts.m,"Comparison_WH_PronounDomain_firstSegments",T)



############

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

##XX

set.seed(615)
#Restricting by area
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_allSegments_byArea",F,stratified=T,areas.d.unanalyzable.wh)
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_firstSegment_byArea",T,T,areas.d.unanalyzable.wh)

set.seed(64135)
#Restricting by area and family
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_allSegments_byAreaAndFamily",F,stratified=T,paste(families.d.unanalyzable.wh,areas.d.unanalyzable.wh))
runPermutation(d.unanalyzable.wh.m,"AllLangs_unanalyzable_firstSegment_byAreaAndFamily",T,T,paste(families.d.unanalyzable.wh,areas.d.unanalyzable.wh))

# Compare with random concepts
set.seed(4485)
runComparison.wh.random.permutation(d.unanalyzable.wh.m,d.random.unanalyzable.m,"RandomConcepts/Comparison_WH_Random_unanalyzable_allSegments",F)
runComparison.wh.random.permutation(d.unanalyzable.wh.m,d.random.unanalyzable.m,"RandomConcepts/Comparison_WH_Random_unanalyzable_firstSegments",T)


# run comparison with random concepts within domains 
runComparison.wh.domain.permutation(d.unanalyzable.wh.m, d.random.unanalyzable.m, "RandomConcepts/Comparison_WH_Domain_unanalyzable_allSegments",F)
runComparison.wh.domain.permutation(d.unanalyzable.wh.m, d.random.unanalyzable.m, "RandomConcepts/Comparison_WH_Domain_unanalyzable_firstSegments",T)


# Run within specific domains (pre-selected) - body parts, basic actions

resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_UnanalyzableWords_SpecificDomains.txt"
#Removes old info
cat("",file=resultsFile) # clear results file

set.seed(3278)
runComparisonPermutation(d.unanalyzable.wh.m,d.unanalyzable.BodyConcepts.m,"Comparison_WH_BodyDomain_unanalyzable_allSegments",F)
runComparisonPermutation(d.unanalyzable.wh.m,d.unanalyzable.BodyConcepts.m,"Comparison_WH_BodyDomain_unanalyzable_firstSegments",T)
runComparisonPermutation(d.unanalyzable.wh.m,d.unanalyzable.BasicActionConcepts.m,"Comparison_WH_BasicActionsDomain_unanalyzable_allSegments",F)
runComparisonPermutation(d.unanalyzable.wh.m,d.unanalyzable.BasicActionConcepts.m,"Comparison_WH_BasicActionsDomain_unanalyzable_firstSegments",T)

runComparisonPermutation(d.unanalyzable.wh.m,d.unanalyzable.PronounConcepts.m,"Comparison_WH_PronounDomain_unanalyzable_allSegments",F)
runComparisonPermutation(d.unanalyzable.wh.m,d.unanalyzable.PronounConcepts.m,"Comparison_WH_PronounDomain_unanalyzable_firstSegments",T)


# Run to here
##########################
# Do permutation tests with random concepts

number.of.perms = 500
number.of.random.samples = 100

set.seed(9233)
resultsFile = "../Results/SimplifiedPhonology/ResultsSummary_RandomPermutations.txt"

runRandomPermutation(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_allSegments",firstSegment=F)

runRandomPermutation(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_firstSegments",firstSegment=T)



number.of.perms = 100
number.of.random.samples = 20


runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_firstSegments",T,stratified=T,families=families.d.wh)

runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_allSegments",F,stratified=T,families=families.d.wh)

# families and areas
set.seed(923)
runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_and_Area_firstSegments",T,stratified=T,families=paste(families.d.wh,areas.d.wh))

runRandomPermutation_Domain(d.random.m,"RandomConcepts/RandomConceptPermutationTest/Permutation_Domain_byFamily_and_Area_allSegments",F,stratified=T,families=paste(families.d.wh,areas.d.wh))

