try(setwd("U:/Pragmatics/Slonimska/Data/Processing/Matched_word_lists"))

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Processing/Matched_word_lists"))


IDS<-read.csv("IDSlist_new.csv", stringsAsFactors=F, fileEncoding='utf-8')
IDS = cbind(X=rep(NA,nrow(IDS)),IDS)
IDS$borrowed_score = NA
IDS$analyzability = NA

WOLD<-read.csv("WOLDlist.csv", stringsAsFactors=F, fileEncoding='utf-8')
WOLD = WOLD[!is.na(WOLD$glotto),]
WOLD = WOLD[!is.na(WOLD$language_pk),]
WOLD = WOLD[,names(WOLD)!="originalWord"]

SB<-read.csv("SBlist.csv", stringsAsFactors=F, fileEncoding='utf-8')
SB$analyzability = NA


Alldata<-rbind(IDS,WOLD,SB)

numSourcesPerLang = tapply(Alldata$Source, Alldata$glotto, function(X){length(unique(X))})
oneSource = names(numSourcesPerLang[numSourcesPerLang==1])
moreThanOneSource = names(numSourcesPerLang[numSourcesPerLang>1])

for(i in moreThanOneSource){
  print(i)
  print(table(Alldata[Alldata$glotto==i,]$Source))
}

keep.spakbanken = ("nepa1252")
keep.WOLD = c("wich1264",'thai1261','mapu1245','haus1257', 'arch1244','bezh1248' )
keep.WOLD.but.add.IDS = c('hawa1245')
presentMeanings = unique(Alldata[Alldata$glotto=='hawa1245' & Alldata$Source=='WOLD',]$language_pk)

Alldata = 
  Alldata[
    # Just one source
  (Alldata$glotto %in% oneSource) |
    # Or where the dominant source is IDS
    (!Alldata$glotto %in% oneSource & Alldata$Source=="IDS" & !Alldata$glotto %in% c(keep.WOLD,keep.spakbanken,keep.WOLD.but.add.IDS)) |
    # Or specifically keep some Sprakbanken languages
    (Alldata$Source=='Sprakbanken' & Alldata$glotto %in% keep.spakbanken) |
    # Or some wold languages
    (Alldata$Source=='WOLD' & Alldata$glotto %in% keep.WOLD) |
    # Or this WOLD language with additional meanings from IDS
    (Alldata$Source=="WOLD" & Alldata$glotto=='hawa1245')|
    ((Alldata$Source=='IDS') & (Alldata$glotto=='hawa1245') & (!Alldata$language_pk %in% presentMeanings)),]


Alldata$domain <- floor(Alldata$language_pk)

Alldata[Alldata$Language=="Carib",]$Language="Kali'na"


write.csv(Alldata,file="Alldata.csv",fileEncoding='utf-8')
