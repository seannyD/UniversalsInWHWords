
setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis/")

options("scipen"=100, "digits"=7)

getStats = function(filename,base=''){
  d = read.csv(paste(base,filename,sep=''))
  trueV = d[d$Type=='True',2]
  permV = d[d$Type!='True',2]
  permV = permV[!is.na(permV)]
  
  meanPerm = mean(permV)
  
  p = sum(trueV > permV) / length(permV)
  if(sum(grepl("Comparison_",filename)>0)){
    p = sum(trueV < permV) / length(permV)  
  }
  
  if(p==0){
    p = paste("<",1/length(permV))
  }
  z.score = (trueV - mean(permV)) / sd(permV)
  
  return(c(filename,meanPerm,p,z.score))
}


getStats2 = function(filenames,base=''){
  d = data.frame()
  for(filename in filenames){
    d = rbind(d,read.csv(paste(base,filename,sep='')))
  }
  trueV = mean(d[d$Type=='True',2])
  permV = d[d$Type!='True',2]
  permV = permV[!is.na(permV)]
  
  meanPerm = mean(permV)
  
  p = sum(trueV > permV) / length(permV)
  #if(sum(grepl("Comparison_",filename)>0)){
  #  p = sum(trueV < permV) / length(permV)  
 # }
  
  if(p==0){
    p = paste("<",1/length(permV))
  }
  z.score = (trueV - mean(permV)) / sd(permV)
  
  return(c(filename,meanPerm,p,z.score))
}

baseF = "../Results/SimplifiedPhonology/PermutationResults/"
files = list.files(baseF)
files = files[grepl("\\.csv",files)]

res = as.data.frame(t(sapply(files,getStats,base=baseF)))
names(res) = c("run",'meanPerm','p','z')
rownames(res) = res$run
res$meanPerm = as.numeric(as.character(res$meanPerm))
res$z = as.numeric(as.character(res$z))
res$meanPerm = signif(res$meanPerm,2)
res$z = round(res$z,2)

study1.res= c("AllLangs_firstSegments.csv",
"AllLangs_firstSegment_byFamily.csv",
"AllLangs_firstSegment_byArea.csv",
"AllLangs_firstSegment_byAreaAndFamily.csv",
"AllLangs_unanalyzable_allSegments.csv",
"AllLangs_unanalyzable_allSegments_byFamily.csv",
"AllLangs_unanalyzable_allSegments_byArea.csv",
"AllLangs_unanalyzable_allSegments_byAreaAndFamily.csv")

study1.table = res[study1.res,]
write.csv(study1.table,file= "../Results/SimplifiedPhonology/tables/study1.csv", row.names = F)


#############
baseF = "../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/"
files = list.files(baseF)
files = files[grepl("\\.csv",files)]

res2 = as.data.frame(t(sapply(files,getStats,base=baseF)))
names(res2) = c("run",'meanPerm','p','z')
rownames(res2) = res2$run
res2$meanPerm = as.numeric(as.character(res2$meanPerm))
res2$z = as.numeric(as.character(res2$z))
res2$meanPerm = round(res2$meanPerm,2)
res2$z = round(res2$z,2)


res3 = getStats2(files[grepl("Comparison_WH_Domain_firstSegments_concept_",files)],base="../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/")
res3 = as.data.frame(t(res3))
names(res3) = c("run",'meanPerm','p','z')
res3$meanPerm = as.numeric(as.character(res3$meanPerm))
res3$z = as.numeric(as.character(res3$z))
res3$meanPerm = round(res3$meanPerm,2)
res3$z = round(res3$z,2)

res4 = getStats2(files[grepl("Comparison_WH_Domain_unanalyzable_allSegments_concept_",files)],base="../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/")
res4 = as.data.frame(t(res4))
names(res4) = c("run",'meanPerm','p','z')
res4$meanPerm = as.numeric(as.character(res4$meanPerm))
res4$z = as.numeric(as.character(res4$z))
res4$meanPerm = round(res4$meanPerm,2)
res4$z = round(res4$z,2)



study2.table = rbind(res2["Comparison_WH_Random_firstSegments.csv",],
res3[1,],
res["Comparison_WH_BodyDomain_firstSegments.csv",],
res["Comparison_WH_BasicActionsDomain_firstSegments.csv",],
res["Comparison_WH_PronounDomain_firstSegments.csv",],
res2["Comparison_WH_Random_unanalyzable_firstSegments.csv",],
res4[1,]
)

write.csv(study2.table,file= "../Results/SimplifiedPhonology/tables/study2.csv", row.names = F)


############




getStats3 = function(filenames,base=''){
  # random independent sample
  # test whether differences are greater than zero
  d = data.frame()
  for(filename in filenames){
    d = rbind(d,read.csv(paste(base,filename,sep='')))
  }
  trueV = 0
  permV = d[,1]
  permV = permV[!is.na(permV)]
  
  meanPerm = mean(permV)
  
  p = sum(trueV < permV) / length(permV)
  #if(sum(grepl("Comparison_",filename)>0)){
  #  p = sum(trueV < permV) / length(permV)  
  # }
  
  if(p==0){
    p = paste("<",1/length(permV))
  }
  z.score = (trueV - mean(permV)) / sd(permV)
  
  return(c(filename,meanPerm,p,z.score))
}

baseF = "../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/"
files = list.files(baseF)
files = files[grepl("\\.csv",files)]

res5 = rbind(
  t(getStats3("InterrogativeOrder_RandomIndependentSamples_firstSegments.csv",baseF)),
    t(getStats3("InterrogativeOrder_RandomIndependentSamples_area_firstSegments.csv",baseF)),
  t(getStats3("RIS_WH_Unanalyzable_Family_firstSegments.csv",baseF)),
  t(getStats3("RIS_WH_Unanalyzable_Area_firstSegments.csv",baseF)),
  t(getStats3(files[grepl("RIS_RandomConcepts_firstSegments_",files)],baseF)),
  c(NA,NA,NA,NA),
  t(getStats3("RIS_BodyConcepts_firstSegments_Family.csv",baseF)),
  t(getStats3("RIS_BodyConcepts_firstSegments_Area.csv",baseF)),
  t(getStats3("RIS_BasicActionsConcepts_firstSegments_Family.csv",baseF)),
  t(getStats3("RIS_BasicActionsConcepts_firstSegments_Area.csv",baseF))
    )
res5 = as.data.frame((res5))
names(res5) = c("run",'meanPerm','p','z')
res5$meanPerm = as.numeric(as.character(res5$meanPerm))
res5$z = as.numeric(as.character(res5$z))
res5$meanPerm = round(res5$meanPerm,2)
res5$z = round(res5$z,2)

write.csv(res5,file= "../Results/SimplifiedPhonology/tables/study3.csv", row.names = F)
