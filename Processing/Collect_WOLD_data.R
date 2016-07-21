rm(list=ls())

try(setwd("U:/Pragmatics/Slonimska/Data/RAW_data/WOLD"))

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/RAW_data/WOLD"))


#Get rid of unnecessary columns
AllWOLDdata<-read.csv("word_processed_transcribed.csv",stringsAsFactors=F,encoding='utf-8')
AllWOLDdata$age_score<-NULL
AllWOLDdata$simplicity_score<-NULL
AllWOLDdata$borrowed<-NULL
#AllWOLDdata$analyzability<-NULL
AllWOLDdata$valueset_pk<-NULL
AllWOLDdata$counterpart<-NULL
AllWOLDdata$POS<-NULL
AllWOLDdata$semantic_field_pk<-NULL
AllWOLDdata$semantic_field<-NULL
AllWOLDdata$pk<-NULL
AllWOLDdata$parameter_pk<-NULL

AllWOLDdata = AllWOLDdata[!is.na(AllWOLDdata$language_pk),]

AllWOLDdata = AllWOLDdata[AllWOLDdata$word!='Unidentifiable',]


#Add new column Source that shows the source of the language -WOLD
AllWOLDdata$Source<-"WOLD"
#Name column target_language into Language
AllWOLDdata$Language = AllWOLDdata$target_language


#Add iso codes to the file. Import file Wold_to_iso.tab
iso.codes.wold<-read.delim("Wold_to_iso.tab",stringsAsFactors=F,encoding='utf-8')

#Specify in which column new info goes (AllWOLDdata$iso); equate it to the column from which it takes info (iso.codes.wold$iso);match AllWOLDdata$language_pk to iso.codes.wold$wold.number
AllWOLDdata$iso=iso.codes.wold$iso[match(AllWOLDdata$language_pk,iso.codes.wold$wold.number)]

AllWOLDdata$language_pk=AllWOLDdata$meaning.id

#Import Data_clean_up.csv file to get glotto codes.
#codes.file<-read.delim("../Data_clean_up.csv",sep='\t',stringsAsFactors=F)
codes.file <- read.csv("../Data_clean_up.csv",stringsAsFactors=F)

AllWOLDdata$iso[is.na(AllWOLDdata$iso)]=codes.file$iso[match(AllWOLDdata$Language[is.na(AllWOLDdata$iso)],codes.file$N1)]

#Add glotto codes to the Wold languages in new column by using codes.file. Match via iso codes.
AllWOLDdata$glotto=codes.file$glotto[match(AllWOLDdata$iso,codes.file$iso)]

AllWOLDdata$glotto[is.na(AllWOLDdata$glotto)]=codes.file$glotto[match(AllWOLDdata[is.na(AllWOLDdata$glotto),]$Language,codes.file$N1)]




#Reorder AllWOLDdata column in the right order (borrowed score last)
AllWOLDdata<-AllWOLDdata[,c("language_pk", "meaning" , "word", "Source","Language","iso","glotto", "borrowed_score" ,"analyzability")]


write.csv(AllWOLDdata,file="../../Processing/Matched_word_lists/WOLDlist.csv",fileEncoding='utf-8')
