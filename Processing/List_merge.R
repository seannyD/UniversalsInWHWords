try(setwd("U:/Pragmatics/Slonimska/Data/Processing/Matched_word_lists"))

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Processing/Matched_word_lists"))

countWhWords <- function(X){
  y <- sum(c("when?",'which?','who?','where?','why?','what?','how many?','how much?','how?') %in% X, na.rm=T)
  if(is.na(y)){ return(0)}
  return(y)
}

whwords<-c("when?",'which?','who?','where?','why?','what?','how many?','how much?','how?')
IDS<-read.csv("IDSlist.csv", stringsAsFactors=F, fileEncoding='utf-8')
WOLD<-read.csv("WOLDlist.csv", stringsAsFactors=F, fileEncoding='utf-8')
SB<-read.csv("SBlist.csv", stringsAsFactors=F, fileEncoding='utf-8')

IDS$analyzability = NA
SB$analyzability = NA


Alldata<-rbind(IDS,WOLD,SB)


Alldata$domain <- floor(Alldata$language_pk)



write.csv(Alldata,file="Alldata.csv",fileEncoding='utf-8')
