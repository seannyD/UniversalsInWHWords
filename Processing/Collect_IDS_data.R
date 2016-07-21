
rm(list=ls())

#set directory from which files are used
try(setwd("U:/Pragmatics/Slonimska/Data/RAW_data/IDS"))

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/RAW_data/IDS"))

#Import Data_clean_up.csv file to get glotto and iso codes.
codes.file<-read.delim("../Data_clean_up.csv",sep='\t',stringsAsFactors=F)

#create function that returns language data frame with adjusted columns

getAdjustedIDSdata<-function(Filename){
  #Adjust file - quotation marks can be used in strings, don't treat separate strings as factors
  Data<-read.delim(Filename, quote="",stringsAsFactors=F,encoding="UTF-8")
  #Add new column Source that shows the source of the language -IDS
  Data$Source<-"IDS"
  #Add new column Language that is named after the 3rd header in file (happens to be the title of Language)
  Data$Language<- names(Data)[3]
  #Re-name the first three columns
  names(Data)[1]<-"language_pk"
  names(Data)[2]<-"meaning"
  names(Data)[3]<-"word"
  #Add iso codes. Specify in which column new info goes (Data$iso); equate it to the column from which it takes info (codes.file$iso);match Data$Language to codes.file$IDS2
  
  Data$iso = NA
  Data$glotto = NA
  
  mx = codes.file[codes.file$IDS2==Data$Language[1],]
  
  Data$iso=mx$iso
  #Do the same for glotto codes.
  Data$glotto=mx$glotto
  #Add column for borrowed_score
  Data$borrowed_score<-NA
  #Finish function with requesting data frame (?)
  return(Data)
}

#create and name main data file (data.frame - rows and columns)- empty, in which all data about languages will be added
AllIDSdata=data.frame()

#create variable that is a list of all language file; specified that has to end with .tab
language.titles<-list.files(pattern="*.tab")

#create loop (delimited by {}) to apply following functions for each item (filename) from the list of all languages (langauge.titles)
for(Filename in language.titles){
  DataIDS<-getAdjustedIDSdata(Filename)
  #Add rows from the Data to the AllIDSdata data frame
  AllIDSdata<-rbind(AllIDSdata,DataIDS)
}

#Treating language_pk numbers as numeric and not as strings (In order to match with meaning filds from IDS and WOLD)
AllIDSdata$language_pk = as.character(as.numeric(as.character(AllIDSdata$language_pk)))

write.csv(AllIDSdata,file="../../Processing/Matched_word_lists/IDSlist.csv",fileEncoding='utf-8')

#Make function that takes only unique characters and shows the number of items (length)
get_unique_length<-function(firstCH){
  unique_firstCH<-unique(firstCH)
  length(unique_firstCH)  
}

#Get the first character of each word
firstCH<-substr(AllIDSdata$word,1,1)

#Split characters by language
splitfirstCH<-tapply(firstCH,AllIDSdata$Language,get_unique_length)

