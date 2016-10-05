library(xtable)
setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis/")

options("scipen"=100, "digits"=7)

getStats3 = function(filenames,base='',label=filenames[1],lessThan=F){
  # random independent sample
  # test whether differences are greater than zero
#  d = data.frame()
#  for(filename in filenames){
#    d = rbind(d,read.csv(paste(base,filename,sep='')))
#  }
  
  d = do.call(rbind, lapply(paste(base,filenames,sep=''), function(x) read.csv(x, stringsAsFactors = FALSE)))
  if(ncol(d)==2){
    trueV = mean(d[d$Type=="True",2])
    permV = d[d$Type=="Perm",2]
  } else{
    trueV = 0
    permV = d[,1]
  }
  permV = permV[!is.na(permV)]
  meanPerm = mean(permV)
  
  p = sum(trueV >= permV) / length(permV)
  if(lessThan){
    p = sum(trueV <= permV) / length(permV)  
   }

  
  
  if(p==0){
    if(length(permV) > 10000 ){
      p = "< 0.0001"
    } else{
      p = paste("<",1/length(permV))
    }
  } else{
    if(p < 0.0001){
      p = "< 0.0001"
    } else{
      p = signif(p,2)      
    }
    
  }
  
  z.score = (trueV - mean(permV)) / sd(permV)
  z.score = round(z.score,2)
  meanPerm = signif(meanPerm,2)
  
  filenameLabel = filenames
  if(length(filenameLabel)>1){
    x = unlist(strsplit(filenameLabel, "_"))
    tx = table(x)
    tx = tx[tx==max(tx)]
    filenameLabel = paste(paste(unique(x[x %in% unique(names(tx))]),collapse='_'),"*",sep='')
  }
  
  dx = data.frame(Test=label,filename=filenameLabel,meanPerm=meanPerm,p=p,z=z.score, stringsAsFactors = F)
  return(dx)
}

addLine = function(label){
  return(data.frame(Test=label,filename="Filename",meanPerm="Mean",p="p",z="z", stringsAsFactors = F))
}

makeTitle = function(label,folder){
  return(paste(label," From results folder",folder))
}

makeTable = function(dx, title, baseF, n=172){
  return(print(xtable(dx, caption=makeTitle(title,baseF)),include.rownames=FALSE, type='latex'))
}

baseF = "../Results/SimplifiedPhonology/PermutationResults/"

whAll = rbind(
  #addLine("Wh words, all segments"),
  getStats3(c("AllLangs_allSegments.csv"),baseF , "All segments"),
  getStats3(c("AllLangs_Consonants_allSegments.csv"),baseF,"Consonants"),
  getStats3(c("AllLangs_Vowels_allSegments.csv"),baseF,"Vowels"),
  getStats3("AllLangs_allSegments_byFamily.csv",baseF,"Permute within families"),
  getStats3("AllLangs_allSegments_byArea.csv",baseF,"Permute within areas"),
  getStats3("AllLangs_allSegments_byAreaAndFamily.csv",baseF,"Permute within families andareas"),
  
  getStats3("AllLangs_unanalyzable_allSegments.csv",baseF,"Unanalysable words"),
  getStats3("AllLangs_unanalyzable_allSegments_byFamily.csv",baseF,"Unanalysable words, permute within families"),
  getStats3("AllLangs_unanalyzable_allSegments_byArea.csv",baseF,"Unanalysable words, permute within areas"),
  getStats3("AllLangs_unanalyzable_allSegments_byAreaAndFamily.csv",baseF,"Unanalysable words, permute within families and areas")
)

t1 = makeTable(whAll, "Results for wh words, all segments",baseF,172)

wh1st = rbind(
 # addLine("Wh words, first segments"),
  getStats3(c("AllLangs_firstSegments.csv"),baseF , "All segments"),
  getStats3(c("AllLangs_Consonants_firstSegments.csv"),baseF,"Consonants"),
  getStats3(c("AllLangs_Vowels_firstSegments.csv"),baseF,"Vowels"),
  getStats3("AllLangs_firstSegment_byFamily.csv",baseF,"Permute within families"),
  getStats3("AllLangs_firstSegment_byArea.csv",baseF,"Permute within areas"),
  getStats3("AllLangs_firstSegment_byAreaAndFamily.csv",baseF,"Permute within families andareas"),
  
  getStats3("AllLangs_unanalyzable_firstSegments.csv",baseF,"Unanalysable words"),
  getStats3("AllLangs_unanalyzable_firstSegment_byFamily.csv",baseF,"Unanalysable words, permute within families"),
  getStats3("AllLangs_unanalyzable_firstSegment_byArea.csv",baseF,"Unanalysable words, permute within areas"),
  getStats3("AllLangs_unanalyzable_firstSegment_byAreaAndFamily.csv",baseF,"Unanalysable words, permute within families and areas")
)

t2 = makeTable(wh1st, "Results for wh words, first segments",baseF,172)


action = rbind(
 # addLine("Basic action words"),
  getStats3("AllLangs_allSegments_ActionDomain.csv",baseF , "All segments"),
  getStats3("AllLangs_firstSegments_ActionDomain.csv",baseF , "First segments"),
  getStats3("AllLangs_allSegments_BasicActionsDomain_byFamilyAndArea.csv", baseF, "All segments, permute within families and areas"),
  getStats3("AllLangs_firstSegments_BasicActionsDomain_byFamilyAndArea.csv", baseF, "First segments, permute within families and areas")
)
t3 = makeTable(action, "Results for basic action words",baseF,172)

body = rbind(  
  #addLine("Body words"),
  getStats3("AllLangs_allSegments_BodyDomain.csv",baseF , "All segments"),
  getStats3("AllLangs_firstSegments_BodyDomain.csv",baseF , "First segments"),
  getStats3("AllLangs_allSegments_BodyDomain_byFamilyAndArea.csv", baseF, "All segments, permute within families and areas"),
  getStats3("AllLangs_firstSegments_BodyDomain_byFamilyAndArea.csv", baseF, "First segments, permute within families and areas")
)
t4 = makeTable(body, "Results for body words",baseF,172)

pronoun = rbind(  
 # addLine("Pronouns"),
  getStats3("AllLangs_allSegments_Pronouns.csv",baseF , "All segments"),
  getStats3("AllLangs_firstSegments_Pronouns.csv",baseF , "First segments"),
  getStats3("AllLangs_allSegments_PronounDomain_byFamilyAndArea.csv", baseF, "All segments, permute within families and areas"),
  getStats3("AllLangs_allSegments_PronounDomain_byFamilyAndArea.csv", baseF, "First segments, permute within families and areas")
)
t5 = makeTable(pronoun, "Results for pronouns",baseF,172)


baseF = "../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/RandomConceptPermutationTest/"
files = list.files(baseF)
files =files[grepl("\\.csv",files)]


random2random = rbind(
  getStats3(files[grepl("Permutation_allSegments_",files)],baseF,"All segments"),
  getStats3(files[grepl("Permutation_firstSegments_",files)],baseF,"First segments"),
  getStats3(files[grepl("Permutation_Domain_byFamily_allSegments_",files)],baseF,"Same domain, permute within family, all segments"),
  getStats3(files[grepl("Permutation_Domain_byFamily_firstSegments_",files)],baseF,"Same domain, permute within family, first segments"),
  getStats3(files[grepl("Permutation_Domain_byFamily_and_Area_allSegments_",files)],baseF,"Same domain, permute within family and area, all segments"),
  getStats3(files[grepl("Permutation_Domain_byFamily_and_Area_firstSegments_",files)],baseF,"Same domain, permute within family and area, first segments")
)

t6 = makeTable(random2random, "Similarity of randomly selected concepts within a language, compared to between languages.", baseF)

outFile = "../Results/SimplifiedPhonology/tables/Summary/SummaryTables.tex"
cat( paste(t1,t2,t3,t4,t5,t6,sep="\n"), file=outFile)

#######################

baseF = "../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/"
files = list.files(baseF)
files =files[grepl("\\.csv",files)]

wh2random = rbind(
  getStats3("Comparison_WH_Random_allSegments.csv",baseF,"All segments"),
  getStats3("Comparison_WH_Random_firstSegments.csv",baseF,"First segments"),
  getStats3(files[grepl("Comparison_WH_Domain_allSegments_concept_1",files)], baseF, "From same semantic domain, all segments"),
  getStats3(files[grepl("Comparison_WH_Domain_firstSegments_concept_1",files)], baseF, "From same semantic domain, first segments"),
  
  getStats3("Comparison_WH_Random_unanalyzable_allSegments.csv",baseF,"Unanalysable words, all segments"),
  getStats3("Comparison_WH_Random_unanalyzable_firstSegments.csv",baseF,"Unanalysable words, first segments"),
  
  getStats3("Comparison_Initial_WH_Random_All_allSegments.csv",baseF,"Initial languages only, all segments"),
  getStats3("Comparison_Initial_WH_Random_All_firstSegments.csv",baseF,"Initial languages only, first segments"),
  getStats3("Comparison_NonInitial_WH_Random_All_allSegments.csv",baseF,"Non-Initial languages only, all segments"),
  getStats3("Comparison_NonInitial_WH_Random_All_firstSegments.csv",baseF,"Non-Initial languages only, first segments")
)

tb1 = makeTable(wh2random, "Comparing the mean entropy of wh words to a randomly selected set of words.\\\\", baseF)

outFile2 = "../Results/SimplifiedPhonology/tables/Summary/SummaryTables2.tex"
cat(tb1, file=outFile2)

#########

baseF = "../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/RandomConceptPermutationTest/"
files = list.files(baseF)
files =files[grepl("\\.csv",files)]



######################################################

baseF = "../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/"
files = list.files(baseF)
files =files[grepl("\\.csv",files)]

RISx = rbind(
  
  getStats3(files[grepl("RIS_WH_Unanalyzable_Family_allSegments",files)],baseF,"Unanalyzable wh words, permuting within families, all segments", T),
  getStats3(files[grepl("RIS_WH_Unanalyzable_Family_firstSegments",files)],baseF,"Unanalyzable wh words, permuting within families, first segments", T),  
  
  getStats3(files[grepl("RIS_WH_Unanalyzable_Area_allSegments",files)],baseF,"Unanalyzable wh words, permuting within areas, all segments", T),
  getStats3(files[grepl("RIS_WH_Unanalyzable_Area_firstSegments",files)],baseF,"Unanalyzable wh words, permuting within areas, first segments", T),
  
  getStats3(files[grepl("RIS_BodyConcepts_allSegments_Family",files)],baseF,"Body concepts, permuting within family, all segments", T),
  getStats3(files[grepl("RIS_BodyConcepts_firstSegments_Family",files)],baseF,"Body concepts, permuting within family, first segments", T),
  getStats3(files[grepl("RIS_BodyConcepts_allSegments_Area",files)],baseF,"Body concepts, permuting within area, all segments", T),
  getStats3(files[grepl("RIS_BodyConcepts_firstSegments_Area",files)],baseF,"Body concepts, permuting within area, first segments", T),
  
  getStats3(files[grepl("RIS_BasicActionsConcepts_allSegments_Family",files)],baseF,"Action concepts, permuting within family, all segments", T),
  getStats3(files[grepl("RIS_BasicActionsConcepts_firstSegments_Family",files)],baseF,"Action concepts, permuting within family, first segments", T),
  getStats3(files[grepl("RIS_BasicActionsConcepts_allSegments_Area",files)],baseF,"Action concepts, permuting within area, all segments", T),
  getStats3(files[grepl("RIS_BasicActionsConcepts_firstSegments_Area",files)],baseF,"Action concepts, permuting within area, first segments", T),
  
  getStats3(files[grepl("RIS_PronounConcepts_allSegments_Family",files)],baseF,"Pronouns, permuting within family, all segments", T),
  getStats3(files[grepl("RIS_PronounConcepts_firstSegments_Family",files)],baseF,"Pronouns concepts, permuting within family, first segments", T),
  getStats3(files[grepl("RIS_PronounConcepts_allSegments_Area",files)],baseF,"Pronouns concepts, permuting within area, all segments", T),
  getStats3(files[grepl("RIS_PronounConcepts_firstSegments_Area",files)],baseF,"Pronouns concepts, permuting within area, first segments", T),
  
  
  getStats3(files[grepl("RIS_RandomConcepts_allSegments_",files)],baseF,"Random concepts, all segments", T),
  getStats3(files[grepl("RIS_RandomConcepts_firstSegments_",files)],baseF,"Random concepts, first segments", T),
 
   getStats3(files[grepl("RIS_RandomConcepts_Domain_allSegments",files)],baseF,"Random concepts within the same domain, all segments", T),
  getStats3(files[grepl("RIS_RandomConcepts_Domain_firstSegments",files)],baseF,"Random concepts within the same domain, first segments", T)
  

)

tb2 = makeTable(RISx, "Random independent samples tests, comparing initial interrogative languages and non-initial interrogative languages.", baseF)


outFile3 = "../Results/SimplifiedPhonology/tables/Summary/SummaryTables3.tex"
cat(tb2, file=outFile3)

#######
baseF = "../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/"
files = list.files(baseF)
files =files[grepl("\\.csv",files)]

RISx2 = rbind(
  getStats3(files[grepl("ConsonantsInitial_3_allSegments_RandomIndependentSample",files)],baseF,"Wh words, all consonants", T),
  getStats3(files[grepl("ConsonantsInitial_3_firstSegments_RandomIndependentSample",files)],baseF,"Wh words, first consonant", T),
  getStats3(files[grepl("VowelsInitial_3_allSegments_RandomIndependentSample",files)],baseF,"Wh words, all vowels", T),
  getStats3(files[grepl("VowelsInitial_3_firstSegments_RandomIndependentSample",files)],baseF,"Wh words, first vowel", T)
)


tb3 = makeTable(RISx2, "Random independent samples tests, comparing wh words by consonants or vowels separately.",baseF)


outFile4 = "../Results/SimplifiedPhonology/tables/Summary/SummaryTables4.tex"
cat(tb3, file=outFile4)

