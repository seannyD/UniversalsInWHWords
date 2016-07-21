setwd("U:/Pragmatics/Slonimska/Data/Processing/Matched_word_lists")

countWhWords <- function(X){
  y <- sum(c("when?",'which?','who?','where?','why?','what?','how many?','how much?','how?') %in% X, na.rm=T)
  if(is.na(y)){ return(0)}
  return(y)
}

whwords<-c("when?",'which?','who?','where?','why?','what?','how many?','how much?','how?')


unique(Alldata[is.na(Alldata$iso),]$Language)

#list of words, split by language, count how many words on a list

wordcount_lang<-tapply(Alldata$word,Alldata$Language, length)

#list of languages that has <4 items.
lowCount<-names(wordcount_lang)[wordcount_lang<4]

Alldata <- Alldata[! Alldata$Language %in% lowCount,]
list.whwords<-tapply(Alldata$meaning, Alldata$Language, countWhWords)
list.whwords[is.na(list.whwords)]<-0

#Remove all languages that have <4 whwords
#list of langauges that have <4 wh words
nowhwords<-names(list.whwords)[list.whwords<4]
#exclude nowhwords from alldata
Alldata <- Alldata[! Alldata$Language %in% nowhwords,]
list.whwords2<-tapply(Alldata$meaning, Alldata$Language, countWhWords)

#Disregard those lines that have no entry in word column
Alldata <- Alldata[nchar(Alldata$word)>0,]

unique(Alldata[is.na(Alldata$glotto),]$Language)


#language divided by concept, see how many words are covered

#Make function that takes only unique meanings and shows the number of languages (length)
get_unique_length_meaning<-function(x){
  unique_x<-unique(x)
  length(unique_x)  
}

#create variable that splits data by how many languages have the meanings
meaningsbylang<-tapply(Alldata$Language, Alldata$language_pk,get_unique_length_meaning)
hist(meaningsbylang)

#create variable that splits data by how many languages have each whword
whwordsbylang<-tapply(Alldata[Alldata$meaning %in% whwords,]$Language, Alldata[Alldata$meaning %in% whwords,]$language_pk,get_unique_length_meaning)

whwordsbylang/(length(unique(Alldata$Language)))
min_crit<-mean(whwordsbylang/(length(unique(Alldata$Language))))
prop_meaningsbylang<-meaningsbylang/(length(unique(Alldata$Language)))

#How many meaning there are that are covered by mean % of covered whwords
sum(prop_meaningsbylang>=min_crit)

randomwords<-names(prop_meaningsbylang)[prop_meaningsbylang>=min_crit]