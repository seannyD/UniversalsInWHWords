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


numWordsPerLang = apply(d.wh.m,2, function(X){
 x = unlist(strsplit(X,";"))
 x = x[nchar(x)>0 & !is.na(x)]
 length(x)
})

getE = function(sx){
  segs = unlist(strsplit(sx,""))
  uSegs = unique(segs)
  uSegs = uSegs[uSegs!=""]
  charmap = 1:length(uSegs)
  names(charmap) = uSegs
  return(myEntropy(table(charmap[segs])))
}

sampleSameNumberOfLangs = function(dx, n){
  # put rows into random order
  ranOrder = sample(1:nrow(dx))
  
  matchConcept.E.all = mean(
    getWordListEntropy(
      dx[ranOrder[1:9],],
      firstSegment = F))
  matchConcept.E.first = mean(
    getWordListEntropy(
      dx[ranOrder[1:9],],
      firstSegment = T))
  
  matchN.E = 
      sapply(1:length(n), function(i){
        sx = unlist(strsplit(dx[ranOrder,i],";"))
        sx = sx[!is.na(sx)]
        sx = sx[1:n[i]]
        e.all = getE(sx)
        # first segs
        sx = substr(sx,1,1)
        e.first = getE(sx)
        return(c(e.all,e.first))
      })
  apply(matchN.E,1,mean)
  
  #concept all, concept first, N all, N first
  return(c(matchConcept.E.all,
           matchConcept.E.first,
           apply(matchN.E,1,mean)))
}

set.seed(2387)
matchedNPerm.means = replicate(
  n = 1000, 
  sampleSameNumberOfLangs(d.random.m,numWordsPerLang))

write.csv(matchedNPerm.means,"../Results/SimplifiedPhonology/PermutationResults/MatchNPermutation.csv", row.names = F)

#matchedNPerm.means = read.csv("../Results/SimplifiedPhonology/PermutationResults/MatchNPermutation.csv", stringsAsFactors = F)

wh.Mean.All = mean(getWordListEntropy(d.wh.m,F))
wh.Mean.First = mean(getWordListEntropy(d.wh.m,T))

getStats = function(permE,realE){
  p =sum(permE <= realE, na.rm = T)
  if(p==0){
    p = paste("< ",1/length(permE),collapse='')
  } else{
    p = p / sum(!is.na(p))
  }
  z = (realE - mean(permE,na.rm=T)) / sd(permE,na.rm=T)
  return(paste("p = ",p,", z = ",z,collapse=""))
}

# wh vs matched concept, all
getStats(unlist(matchedNPerm.means[1,]),wh.Mean.All)
# wh vs matched concept, first (in table 4 z = 27.47)
getStats(unlist(matchedNPerm.means[2,]),wh.Mean.First)
# wh vs matched N, all
getStats(unlist(matchedNPerm.means[3,]),wh.Mean.All)
# wh vs matched N, first
getStats(unlist(matchedNPerm.means[4,]),wh.Mean.First)

#####

# Compare controlling concepts to controlling N

sum(matchedNPerm.means[1,] < matchedNPerm.means[3,])
sum(matchedNPerm.means[2,] < matchedNPerm.means[4,],na.rm=T)

mean(matchedNPerm.means[1,] - matchedNPerm.means[3,],na.rm=T)
mean(matchedNPerm.means[2,] - matchedNPerm.means[4,],na.rm=T)


######
# Test the comparison of pronouns to wh words

numOfPronounPerLang = apply(d.PronounConcepts.m,2, function(X){
  x = unlist(strsplit(X,";"))
  x = x[nchar(x)>0 & !is.na(x)]
  length(x)
})

numOfPronounPerLang["Tsimshian"] = 0
numOfPronounPerLang = numOfPronounPerLang[names(numWordsPerLang)]

numPWords = apply(cbind(numOfPronounPerLang, numWordsPerLang), 1, min)


d.PronounConcepts.m2 = d.PronounConcepts.m
d.PronounConcepts.m2 = cbind(d.PronounConcepts.m, Tsimshian=rep("",6))
d.PronounConcepts.m2 = d.PronounConcepts.m2[,colnames(d.wh.m)]

sampleMin = function(X,Y){
  ret= sapply(1:ncol(X), function(i){
   # for(i in 1:ncol(X)){
    X2 = unlist(strsplit(X[,i],";"))
    X2= X2[!is.na(X2) & nchar(X2)>0]
    if(length(X2)==0){
      return(NA)
    } else{
    Y2 = unlist(strsplit(Y[,i],";"))
    Y2= Y2[!is.na(Y2) & nchar(Y2)>0]
    X2 = sample(X2,min(length(X2), length(Y2)))
    Y2 = sample(Y2,min(length(X2), length(Y2)))
    
    xE = getWordListEntropy(cbind(X2),T)
    yE = getWordListEntropy(cbind(Y2),T)
    
    return(c(xE,yE))
    }
  })
  xE = mean(sapply(ret,head,n=1),na.rm=T)
  yE = mean(sapply(ret,tail,n=1),na.rm=T)
  # negative number = yE smaller than xE
  return(yE - xE)
}  

# negative numebr = wh smaller than pronouns
res = replicate(1000,sampleMin(d.PronounConcepts.m2, d.wh.m))

sum(res >= 0) / sum(!is.na(res))

(0 - mean(res,na.rm=T)) / sd(res,na.rm=T)
