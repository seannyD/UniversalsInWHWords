rm(list=ls())

try(setwd("U:/Pragmatics/Slonimska/Data/RAW_data/SPRAKBANKEN"))

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/RAW_data/SPRAKBANKEN"))

#Create an empty data frame for SB languages
AllSBdata=data.frame()
#create variable that is a list of all language files; specified that has to end with .csv
language.titles.SB<-list.files(pattern="*_IDS.csv")

#Import Data_clean_up.csv file to get the glotto codes.
codes.file<-read.delim("../Data_clean_up.csv",sep='\t',stringsAsFactors=F)
codes.file = codes.file[codes.file$Source=='Spraakbanken',]

#create function that returns language data frame with adjusted columns
getAdjustedSBdata<-function(FilenameSB){
  #Adjust file - quotation marks can be used in strings, don't treat separate strings as factors, skip first 9 lines, there are no headers
  DataSB<-read.delim(FilenameSB, quote="",stringsAsFactors=F,encoding="UTF-8", skip=9, header=F)
  #Add new column Source that shows the source of the language -Sprakbanken
  #add new column Language, use FilenameSB to get particular language.
 # DataSB$Language<- FilenameSB
 # DataSB$iso<-""
 # DataSB$glotto<-""
#  DataSB$borrowed_score<-"NA"
  #Name the 1st header of the file "language_pk", 2nd header "meaning", V4 column "word"
 # names(DataSB)[1]<-"language_pk"
 # names(DataSB)[2]<-"meaning"
  
  DataSB2 <- data.frame(Language=FilenameSB,language_pk=DataSB[,1],meaning=DataSB[,2], stringsAsFactors=F)
  
  if(sum(nchar(DataSB[1,])>2)==5){
    DataSB2$word <- DataSB[,4]
  } else{
    DataSB2$word <- DataSB[,3]
  }
  
  DataSB2$borrowed_score <- NA
  DataSB2$Source<-"Sprakbanken"
  
  #Delete columns 3, 5, 6, 7
  #DataSB$V3<-NULL
 # DataSB$V5<-NULL
 # DataSB$V6<-NULL
 # DataSB$V7<-NULL
  #Get iso codes by matching language file titles with titles from codes.file column IDS2 (??)
   #Delete _IDS.csv part for Language column 
    DataSB2$Language <- substr(DataSB2$Language, 1, nchar(DataSB2$Language)-8)
  
  DataSB2$iso = NA
  DataSB2$glotto = NA
  
  mx = codes.file[codes.file$N1==DataSB2$Language[1],]
  
  DataSB2$iso=mx$iso
  #Get glotto codes by matching iso codes from codes.file
  DataSB2$glotto=mx$glotto
  
  DataSB2 <- DataSB2[,c("language_pk", "meaning" , "word", "Source","Language","iso","glotto", "borrowed_score" )]
  
  return(DataSB2)
}


for(FilenameSB in language.titles.SB){
  DataSB<-getAdjustedSBdata(FilenameSB)
  #Add rows of the file to the main file AllSBdata
  AllSBdata<-rbind(AllSBdata,DataSB)
}

AllSBdata$language_pk = gsub("S",'',AllSBdata$language_pk)
AllSBdata$language_pk  = as.character(as.numeric(AllSBdata$language_pk))

write.csv(AllSBdata,file="../../Processing/Matched_word_lists/SBlist.csv",fileEncoding='utf-8')
