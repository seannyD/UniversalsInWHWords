try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# Add grammar positioning to the clean-up file in new column whith wals info and grammar info on positioning together.
  #Import clean-up file.
l.details = read.csv("../Analysis/LangsInAnalysis_withGeoData.csv", fileEncoding = "utf-8", stringsAsFactors = F)




#Import grammars file to make reference
grammar = read.csv("../RAW_data/Grammars.csv",stringsAsFactors=F)

c1 = paste("\\cite[",grammar$Page1,"]{",grammar$Reference1,"}", sep='')
c2 = paste("\\cite[",grammar$Page2,"]{",grammar$Reference.2,"}", sep='')
c2[nchar(c2)<11] = ""

grammar$ref = paste(c1, c2)

glx = unique(alldata$glotto)
glx[!glx %in% l.details$glotto]



