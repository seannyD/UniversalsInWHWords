try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# Add grammar positioning to the clean-up file in new column whith wals info and grammar info on positioning together.
  #Import clean-up file.
l.details = read.csv("../RAW_data/Data_clean_up2.csv",stringsAsFactors=F)
  #Import grammars file
grammar = read.csv("../RAW_data/Grammars.csv",stringsAsFactors=F)

  #specify that blank spaces or strings NA are replaced by actual NA in both file positionign columns.
grammar$Positioning[nchar(grammar$Positioning)<3]<-NA
l.details$InterrogativePosition[nchar(l.details$InterrogativePosition)<3]<-NA
#duplicate positioning column InterrogativePosition, call it walsandgrammars
l.details$walsandgrammars=l.details$InterrogativePosition
  #add info about positioning from grammar to walsandgrammars by matching glotto codes.
  #Specify in which column new info goes (l.details$walsandgrammars), then take a subset only of those strings that are NA; equate it to the column from which it takes info (grammar$Positioning);match l.details$glotto (but only the subset where walsandgrammars rows are NA) to grammar$Glotto
l.details$walsandgrammars[is.na(l.details$walsandgrammars)]=grammar$Positioning[match(l.details$glotto[is.na(l.details$walsandgrammars)],grammar$Glotto)]

#Add POSSIBLe grammar positioning in a new column the same as with definite grammar positioning.
grammar$Possible.Positioning[nchar(grammar$Possible.Positioning)<3]<-NA
l.details$possiblegrammars=l.details$walsandgrammars
l.details$possiblegrammars[is.na(l.details$possiblegrammars)]=grammar$Possible.Positioning[match(l.details$glotto[is.na(l.details$possiblegrammars)],grammar$Glotto)]

c1 = paste("\\cite[",grammar$Page1,"]{",grammar$Reference1,"}", sep='')
c2 = paste("\\cite[",grammar$Page2,"]{",grammar$Reference.2,"}", sep='')
c2[nchar(c2)<11] = ""

grammar$ref = paste(c1, c2)

glx = unique(alldata$glotto)
glx[!glx %in% l.details$glotto]



