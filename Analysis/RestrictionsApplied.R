
try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))

try(source("PermutationTools.r"))

#make a list of wh-words by taking meaning.id.fixed reference numbers
whwords<-c(17.61,17.62,17.63,17.64,17.65,17.66,17.67,17.68,17.69)


countWhWords <- function(X){
  y <- sum(c(17.61,17.62,17.63,17.64,17.65,17.66,17.67,17.68,17.69) %in% X, na.rm=T)
  if(is.na(y)){ return(0)}
  return(y)
}



#open data file, load all the data
try(alldata<-read.csv("../Processing/CleanedAndSimplifiedData/Alldata_simple.csv", stringsAsFactors=F))

alldata$domain = floor(alldata$meaning.id.fixed)

#Pre-filtering: Disregard those lines that have no entry in word column
alldata <- alldata[nchar(alldata$word)>0,]

# take out lines with no glotto code
alldata <- alldata[!is.na(alldata$glotto),]

#Take out following langauges:
#Take out Old chinese
#Take out classical greek
#Take out "Unidentified" language (WOLD)
#Remove classical arabic, Slavic, Mongolic and Suriname Portuguese
#Remove Elamite_Phonemic?  Ancient language
# Take out Hindi (non-phonetic script)

deleteLang<-c("Old Chinese","Classical Greek","Unidentified", "Classical Arabic","Slavic","Mongolic","Suriname Portuguese","Hindi")

alldata <- alldata[!alldata$Language %in% deleteLang,]


#######################################################
# Pronouns - in the raw data, there are two types of coding:
#  he/she/it in one entry, and each separately
#  so collapse all he/she/it into one meaning
pronouns = as.numeric(c("2.91","2.92","2.93","2.931","2.932","2.933","2.94","2.941","2.942","2.95","2.96"))
he.she.it = as.numeric(c("2.931","2.932","2.933"))
dx = alldata[alldata$meaning.id.fixed %in% he.she.it & !is.na(alldata$meaning.id.fixed),]
he.she.it.words = tapply(dx$word.simple,dx$Language, paste,collapse=';')
sel = (is.na(alldata$word.simple) | nchar(alldata$word.simple)==0) & !is.na(alldata$meaning.id.fixed) & alldata$meaning.id.fixed==2.93
alldata[sel,]$word.simple = he.she.it.words[alldata[sel,]$Language]

langs.with.hesheit = unique(alldata[!is.na(alldata$meaning.id.fixed) & !is.na(alldata$word.simple) & alldata$meaning.id.fixed==2.93,]$Language)
langs.without.hesheit = setdiff(unique(alldata$Language), langs.with.hesheit)
he.she.it.words = he.she.it.words[langs.without.hesheit]
langs.without.hesheit = langs.without.hesheit[!is.na(he.she.it.words)]
he.she.it.words = he.she.it.words[!is.na(he.she.it.words)]

extraPronounRows = data.frame(
    meaning.id=2.93,
    meaning='he/she/it',
    word = he.she.it.words,
    Source = "XX",
    Language = langs.without.hesheit,
    iso = tapply(alldata$iso,alldata$Language,head,n=1)[langs.without.hesheit],
    glotto = tapply(alldata$glotto,alldata$Language,head,n=1)[langs.without.hesheit],
    borrowed_score = NA,
    analyzability = NA,
    domain = 2,
    word.clean = he.she.it.words,
    word.simple = he.she.it.words,
    meaning.id.fixed = 2.93
    )
alldata = rbind(alldata,extraPronounRows)


########################################
# Restrict data by certain criteria


#Restriction 1. Wh-word count benchmark < 4

    #split wh-words (their IDs) by language and specify that those that are NA should be treated as O

list.whwords<-tapply(alldata$meaning.id.fixed, alldata$Language, countWhWords)
list.whwords[is.na(list.whwords)]<-0

    #Remove all languages that have <4 whwords
           #make a list of langauges that have <4 whwords
nowhwords<-names(list.whwords)[list.whwords<4]
            #exclude nowhwords from alldata
alldata <- alldata[! alldata$Language %in% nowhwords,]




#Restriction 2. Word count benchmark >= as wh-words mean count. 

  #Make function that takes only unique meaning.id and shows the number of languages (length)
get_unique_length_meaning<-function(x){
  unique_x<-unique(x)
  length(unique_x)  
}

  #create variable that splits data by how many languages have the meaning.id
meaningsbylang<-tapply(alldata$Language, alldata$meaning.id.fixed,get_unique_length_meaning)

  #create variable that splits data by how many languages have each wh-word
whwordsbylang<-tapply(alldata[alldata$meaning.id.fixed %in% whwords,]$Language, alldata[alldata$meaning.id.fixed %in% whwords,]$meaning.id.fixed,get_unique_length_meaning)

  # define threshold (mean proportion of wh-words available) for word count (id meanings) for langauge to be included.
min_crit<-min(whwordsbylang/(length(unique(alldata$Language))))
prop_meaningsbylang<-meaningsbylang/(length(unique(alldata$Language)))

randommeanings<-as.numeric(names(prop_meaningsbylang)[prop_meaningsbylang>=min_crit])

  #keep only those lines in alldata that are above min-crit (randommeanings)

alldata <- alldata [alldata$meaning.id.fixed %in% randommeanings | alldata$meaning.id.fixed %in% whwords,]

#Restriction 3. Keep 1 of overlapping IDS/WOLD languages
#hau  haus1257
#haw	hawa1245 
#arn  mapu1245 
#tha  thai1261 (thai problem)
#mzh  wich1264
#wca  yano1262

# Check that all languages have wh words


countWhWordsInLanguage <- function(lang.name){
	ax <- alldata[alldata$Language==lang.name & alldata$meaning.id.fixed %in% whwords,]
	# make list of words (function from PermutationTools.r)
	wx <- data.frame.to.matrix(ax)
	getWordListEntropy(wx)
}

overlap.lang<-tapply(alldata$Language,alldata$glotto,get_unique_length_meaning)
overlap.glotto <-names(overlap.lang)[overlap.lang>1]

for(glotto.code in overlap.glotto){
	lang.names.associated.with.glotto = unique(alldata[alldata$glotto==glotto.code,]$Language)
	# select only data with this glotto code
	overlapdata <- alldata[alldata$glotto==glotto.code,]
	# and only wh words
	overlapdata.wh <- overlapdata[overlapdata$meaning.id.fixed %in% whwords,]
	
	# get number of concetps (total)
	num.of.concepts <- tapply(overlapdata$meaning.id.fixed,overlapdata$Language,get_unique_length_meaning)
	
	# get number of wh words
	num.of.Wh.words <- tapply(overlapdata.wh$meaning.id.fixed,overlapdata.wh$Language,get_unique_length_meaning)
	
	# get entropy of wh words
	# convert to special data frame
	overlapdata.matrix <- data.frame.to.matrix(overlapdata.wh)
	entropy.wh.words <- getWordListEntropy(overlapdata.matrix)
	
	#rank langs by max number of wh words, then max entropy, then max number of concepts
	rank_langs = order(num.of.Wh.words,entropy.wh.words,num.of.concepts,decreasing=T)
	# choose top ranking language
	language_to_keep = names(num.of.Wh.words)[rank_langs[1]]
		
	# keep only the chosen language
	alldata = alldata[alldata$glotto!=glotto.code | alldata$Language==language_to_keep,]
	

}

#Restriction 4. Delete languages with the same iso/glotto code. Keep the one with MORE ENTRIES for wh-words or LARGEST entropy score
#hye,nucl1235
#ese, esee1248
#nut,nung1283
#spn,nucl1655
#shn,shan1277
# thai1261 problem



# remove objects we don't need
loaded.functions <- as.vector(lsf.str())
do.not.delete <- c("alldata",'whwords')
rm(list=ls()[! ls() %in% c(loaded.functions,do.not.delete)])

