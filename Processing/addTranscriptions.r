rm(list=ls())

try(setwd("U:/Pragmatics/Slonimska/Data/RAW_data/WOLD"))

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/RAW_data/WOLD"))


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


AllWOLDdata<-read.csv("word_processed.csv",stringsAsFactors=F,encoding='utf-8')

AllWOLDdata$originalWord = AllWOLDdata$word


### ENGLISH

celex = read.delim("~/Documents/MPI/CausalGraphs/CELEX_EngWordforms.txt",sep='\\',quote='',stringsAsFactors=F)
celex = celex[,c("Word","PhonCLX")]
celex.extra = read.delim("ExtraEnglishWords.tab",sep='\t',quote='',stringsAsFactors=F)
celex=rbind(celex,celex.extra)
celex = covertCelex(celex)

Lang = "English"
EnglishWords = AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word
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

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word = EnglishWords


EnglishWords = celex$PhonCLX[match(EnglishWords,celex$Word)]

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[!is.na(EnglishWords)]  = EnglishWords[!is.na(EnglishWords)]

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[is.na(EnglishWords)] = sapply(AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[is.na(EnglishWords)], function(X){
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

dcelex = read.delim("Dutch_Celex.txt",sep='\\',quote='',stringsAsFactors=F)
dcelex = covertCelex(dcelex)
dcelex = dcelex[nchar(dcelex$PhonCLX)>0,]


Lang = "Dutch"
DutchWords = AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word
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

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word = DutchWords


DutchWords = dcelex$PhonCLX[match(DutchWords,dcelex$Word)]

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[!is.na(DutchWords)]  = DutchWords[!is.na(DutchWords)]

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[is.na(DutchWords)] = sapply(AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[is.na(DutchWords)], function(X){
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
# German


gcelex = read.delim("German_Celex.txt",sep='\\',quote='',stringsAsFactors=F)
gcelex = covertCelex(gcelex)
gcelex = gcelex[nchar(gcelex$PhonCLX)>0,]


Lang = "German"
GermanWords = AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word
GermanWords = tolower(GermanWords)


GermanWords = gsub("\\?","",GermanWords)
GermanWords = gsub(" \\(1\\)","",GermanWords)
GermanWords = gsub(" \\(2\\)","",GermanWords)
GermanWords = gsub(" \\(3\\)","",GermanWords)
GermanWords = gsub("\\)"," ",GermanWords)
GermanWords = gsub("\\("," ",GermanWords)
GermanWords = gsub(" +"," ",GermanWords)
GermanWords = gsub('\\"',"",GermanWords)
GermanWords = gsub("\\'","",GermanWords)
GermanWords = gsub(" $","",GermanWords)


AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word = GermanWords


GermanWords = gcelex$PhonCLX[match(GermanWords,gcelex$Word)]

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[!is.na(GermanWords)]  = GermanWords[!is.na(GermanWords)]

AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[is.na(GermanWords)] = sapply(AllWOLDdata[AllWOLDdata$target_language==Lang & !is.na(AllWOLDdata$target_language),]$word[is.na(GermanWords)], function(X){
	wx = strsplit(X,",")[[1]]
	bits = sapply(wx,function(Y){
		x = gcelex[match(strsplit(Y," ")[[1]],gcelex$Word),]$PhonCLX
		return(paste(x[!is.na(x)],collapse=''))
		}
		)
	
	if(length(bits[!is.na(bits)])==0){return(NA)}
	return(paste(bits[!is.na(bits)],collapse=';'))
})




write.csv(AllWOLDdata,file="word_processed_transcribed.csv",fileEncoding='utf-8')