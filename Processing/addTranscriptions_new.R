rm(list=ls())

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/RAW_data/WOLD_new/"))


covertCelex = function(celex){
	celex$PhonCLX = gsub("\\.",'',celex$PhonCLX)

	celex = celex[!duplicated(celex$Word),]
	
	celex.conv = read.delim("../../Processing/CharacterSubstitutions/CELEX_converstion.tab")
	for(i in 1:nrow(celex.conv)){
		celex$PhonCLX = gsub(celex.conv[i,1],celex.conv[i,2],celex$PhonCLX)
	}
	celex$PhonCLX = gsub("\\{","a",celex$PhonCLX)
	celex$PhonCLX = gsub("\\'","",celex$PhonCLX)
	celex$PhonCLX = gsub('\\"',"",celex$PhonCLX)
	celex$PhonCLX = gsub("\\$","o",celex$PhonCLX)
	celex$PhonCLX = gsub("\\+","pf", celex$PhonCLX)
	celex$PhonCLX = gsub("\\|","o", celex$PhonCLX)
	celex$PhonCLX = gsub("\\)","a", celex$PhonCLX)
	celex$PhonCLX = gsub("\\}","ǝ",celex$PhonCLX)	
	celex$Word = tolower(celex$Word)
	return(celex)
}


AllWOLDdata<-read.csv("WOLD_words.csv",stringsAsFactors=F,encoding='utf-8')

AllWOLDdata$originalWord = AllWOLDdata$word


### ENGLISH

celex = read.delim("~/Documents/MPI/CausalGraphs/CELEX_EngWordforms.txt",sep='\\',quote='',stringsAsFactors=F)
celex = celex[,c("Word","PhonCLX")]
celex.extra = read.delim("../WOLD/ExtraEnglishWords.tab",sep='\t',quote='',stringsAsFactors=F)
celex=rbind(celex,celex.extra)
celex = covertCelex(celex)

Lang = "English"
EnglishWords = AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word
EnglishWords = tolower(EnglishWords)


EnglishWords = gsub("^be ","",EnglishWords)
EnglishWords = gsub("^to ","",EnglishWords)
EnglishWords = gsub("^to ","",EnglishWords)
EnglishWords = gsub("\\?","",EnglishWords)
EnglishWords = gsub(" \\(1\\)","",EnglishWords)
EnglishWords = gsub(" \\(2\\)","",EnglishWords)
EnglishWords = gsub(" \\(3\\)","",EnglishWords)
EnglishWords = gsub("\\)"," ",EnglishWords)
EnglishWords = gsub("\\("," ",EnglishWords)
EnglishWords = gsub(" +"," ",EnglishWords)
EnglishWords = gsub('\\"',"",EnglishWords)
EnglishWords = gsub("\\'","",EnglishWords)
EnglishWords = gsub(" $","",EnglishWords)

AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word = EnglishWords


EnglishWords = celex$PhonCLX[match(EnglishWords,celex$Word)]

AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[!is.na(EnglishWords)]  = EnglishWords[!is.na(EnglishWords)]

AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[is.na(EnglishWords)] = sapply(AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[is.na(EnglishWords)], function(X){
	wx = strsplit(X,",")[[1]]
	bits = sapply(wx,function(Y){
		x = celex[match(strsplit(Y," ")[[1]],celex$Word),]$PhonCLX
		return(paste(x[!is.na(x)],collapse=''))
		}
		)
	
	if(length(bits[!is.na(bits)])==0){return(NA)}
	return(paste(bits[!is.na(bits)],collapse=';'))
})





#### Dutch

dcelex = read.delim("../WOLD/Dutch_Celex.txt",sep='\\',quote='',stringsAsFactors=F)
dcelex = covertCelex(dcelex)
dcelex = dcelex[nchar(dcelex$PhonCLX)>0,]


Lang = "Dutch"
DutchWords = AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word
DutchWords = tolower(DutchWords)


DutchWords = gsub("\\?","",DutchWords)
DutchWords = gsub(" \\(1\\)","",DutchWords)
DutchWords = gsub(" \\(2\\)","",DutchWords)
DutchWords = gsub(" \\(3\\)","",DutchWords)
DutchWords = gsub("\\)"," ",DutchWords)
DutchWords = gsub("\\("," ",DutchWords)
DutchWords = gsub(" +"," ",DutchWords)
DutchWords = gsub('\\"',"",DutchWords)
DutchWords = gsub("\\'","",DutchWords)
DutchWords = gsub(" $","",DutchWords)

AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word = DutchWords


DutchWords = dcelex$PhonCLX[match(DutchWords,dcelex$Word)]

AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[!is.na(DutchWords)]  = DutchWords[!is.na(DutchWords)]

AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[is.na(DutchWords)] = sapply(AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[is.na(DutchWords)], function(X){
	wx = strsplit(X,",")[[1]]
	bits = sapply(wx,function(Y){
		x = dcelex[match(strsplit(Y," ")[[1]],dcelex$Word),]$PhonCLX
		return(paste(x[!is.na(x)],collapse=''))
		}
		)
	
	if(length(bits[!is.na(bits)])==0){return(NA)}
	return(paste(bits[!is.na(bits)],collapse=';'))
})



####
# German (not in WOLD)

# 
# gcelex = read.delim("../WOLD/German_Celex.txt",sep='\\',quote='',stringsAsFactors=F)
# gcelex = covertCelex(gcelex)
# gcelex = gcelex[nchar(gcelex$PhonCLX)>0,]
# 
# 
# Lang = "German"
# GermanWords = AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word
# GermanWords = tolower(GermanWords)
# 
# 
# GermanWords = gsub("\\?","",GermanWords)
# GermanWords = gsub(" \\(1\\)","",GermanWords)
# GermanWords = gsub(" \\(2\\)","",GermanWords)
# GermanWords = gsub(" \\(3\\)","",GermanWords)
# GermanWords = gsub("\\)"," ",GermanWords)
# GermanWords = gsub("\\("," ",GermanWords)
# GermanWords = gsub(" +"," ",GermanWords)
# GermanWords = gsub('\\"',"",GermanWords)
# GermanWords = gsub("\\'","",GermanWords)
# GermanWords = gsub(" $","",GermanWords)
# 
# 
# AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word = GermanWords
# 
# 
# GermanWords = gcelex$PhonCLX[match(GermanWords,gcelex$Word)]
# 
# AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[!is.na(GermanWords)]  = GermanWords[!is.na(GermanWords)]
# 
# AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[is.na(GermanWords)] = sapply(AllWOLDdata[AllWOLDdata$Language==Lang & !is.na(AllWOLDdata$Language),]$word[is.na(GermanWords)], function(X){
# 	wx = strsplit(X,",")[[1]]
# 	bits = sapply(wx,function(Y){
# 		x = gcelex[match(strsplit(Y," ")[[1]],gcelex$Word),]$PhonCLX
# 		return(paste(x[!is.na(x)],collapse=''))
# 		}
# 		)
# 	
# 	if(length(bits[!is.na(bits)])==0){return(NA)}
# 	return(paste(bits[!is.na(bits)],collapse=';'))
# })




write.csv(AllWOLDdata,file="../../Processing/Matched_word_lists/WOLDlist.csv",fileEncoding='utf-8')
