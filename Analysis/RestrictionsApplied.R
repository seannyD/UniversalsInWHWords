
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


# Restriction: No creoles or reconstructed langauges
# Languages must have their own glottocode (removes dialects)

#Take out following langauges:
deleteLang<-c(
          # Reconstructed
              "Old Chinese",
              "Classical Greek",
              "Unidentified", 
              "Classical Arabic",
              "Slavic",
              "Mongolic",
              "Proto Polynesian",
              "Proto Austronesian",
              "Ancient Aramaic",
          # Creoles
              'Jamaican Creole (Limonese Creole dialect)',
              "Seychelles Creole",
              "Suriname Portuguese",
              "Negerhollands",
          # Duplicated dialects
              'Shan', # we also have another dialect of the same glottocode
              "Nung-Fengshan", # We have another version of the dialect
              "Kumyk (Dorgeli dialect)", # Using plain "Kumyk" instead of these
              "Kumyk (Kajtak dialect)",
              "Kumyk (Kajtak Tumenler dialect)",
              "Kumyk (Karabudakhkent dialect)",
              "Kumyk (Ter Bragun dialect)",
              "Akhvakh (Northern dialect)", # Using southern dialect
              "Avar (Kusur dialect)",
          "Avar (Andalal dialect)",
          "Avar (Antsukh dialect)",
          "Avar (Batlukh dialect)",
          "Avar (Hid dialect)",
          "Avar (Karakh dialect)",
          "Avar (Zakataly dialect)",
          "Bezhta (Khasharkhota dialect)",
          "Bezhta (Tlyadal dialect, Karauzek subdialect)",
          "Bulang",
          "Bulang-3",
              "Botlikh (Miarso dialect)",
              "Chechen (Akkin dialect)",
          "Chamalal (Gigatli dialect)",
              "Dargwa (Tsudakhar dialect, Tanty subdialect)",
              "Dargwa (Gapshima dialect)",
              "Dargwa (Gapshima Shukti dialect)",
              "Dargwa (Gubden dialect)",
              "Dargwa (Kadar dialect)",
              "Dargwa (Megeb dialect)",
              "Dargwa (Mekegi dialect)",
              "Dargwa (Mugi dialect)",
              "Dargwa (Muiri dialect)",
              "Dargwa (Sirkhi dialect)",
              "Dargwa (Usisha dialect)",
          "Dargwa (Chirag dialect)",
          "Dargwa (Itsari dialect)",
          "Dargwa (Khajdak dialect)",
          "Dargwa (Kubachi dialect)",
          "Dargwa (Tsudakhar dialect)",
          "Dargwa (Urakhi dialect)",
              "Rutul (Mukhrek) dialect",
              "Lak (Arakul dialect)",
              "Lak (Balkhar dialect)",
              "Lak (Shali dialect)",
              "Lezgi (Mikrakh dialect)",
          "Karata (Tokitin dialect)",
          "Khwarshi (Khwarshi dialect)",
          "Lezgian (Quba dialect)",
          "Mang VN",
          "Rutul (Ikhrek dialect)",
          "Rutul (Shinaz dialect)",
          "Sanapaná (Angaité dialect)",
          "Tabasaran (Northern dialect Khanag subdialect)",
          "Tsez (Sagada dialect)",
              "Azerbaijani (Terekeme dialect)",
              "Rutul (Borchino Khnow dialect)",
              "Tsakhur (Gelmets dialect)",
              "Aghul (Koshan dialect)",
          "Andi (Muni dialect)",
          "Armenian (Western variety)",
          "KNB (a Pearic variety)",
          'LiHa',
          'Tum',
          'Kme-2 (Kemie variety)'
              )


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
    Source = tapply(alldata$Source,alldata$Language,head,n=1)[langs.without.hesheit],
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

list.whwords<-tapply(alldata$meaning.id.fixed, alldata$glotto, countWhWords)
#list.whwords[is.na(list.whwords)]<-0

    #Remove all languages that have <4 whwords
           #make a list of langauges that have <4 whwords
nowhwords<-names(list.whwords)[list.whwords<4]
            #exclude nowhwords from alldata
alldata <- alldata[! alldata$glotto %in% nowhwords,]




#Restriction 2. Word count benchmark >= as wh-words mean count. 

  #Make function that takes only unique meaning.id and shows the number of languages (length)
get_unique_length_meaning<-function(x){
  unique_x<-unique(x)
  length(unique_x)  
}

  #create variable that splits data by how many languages have the meaning.id
meaningsbylang<-tapply(alldata$glotto, alldata$meaning.id.fixed,get_unique_length_meaning)

  #create variable that splits data by how many languages have each wh-word
whwordsbylang<-tapply(alldata[alldata$meaning.id.fixed %in% whwords,]$glotto, alldata[alldata$meaning.id.fixed %in% whwords,]$meaning.id.fixed,get_unique_length_meaning)

  # define threshold (mean proportion of wh-words available) for word count (id meanings) for langauge to be included.
min_crit<-min(whwordsbylang/(length(unique(alldata$glotto))))

 # Override with fixed proportion
min_crit = 0.75

prop_meaningsbylang<-meaningsbylang/(length(unique(alldata$glotto)))

randommeanings<-as.numeric(names(prop_meaningsbylang)[prop_meaningsbylang>=min_crit])

  #keep only those lines in alldata that are above min-crit (randommeanings)

alldata <- alldata [alldata$meaning.id.fixed %in% randommeanings | alldata$meaning.id.fixed %in% whwords,]


overlap.lang<-tapply(alldata$Language,alldata$glotto,function(X){length(unique(X))})
overlap.glotto <-names(overlap.lang)[overlap.lang>1]
# 
# for(glotto.code in overlap.glotto){
# 	lang.names.associated.with.glotto = unique(alldata[alldata$glotto==glotto.code,]$Language)
# 	# select only data with this glotto code
# 	overlapdata <- alldata[alldata$glotto==glotto.code,]
# 	# and only wh words
# 	overlapdata.wh <- overlapdata[overlapdata$meaning.id.fixed %in% whwords,]
# 	
# 	# get number of concetps (total)
# 	num.of.concepts <- tapply(overlapdata$meaning.id.fixed,overlapdata$Language,get_unique_length_meaning)
# 	
# 	# get number of wh words
# 	num.of.Wh.words <- tapply(overlapdata.wh$meaning.id.fixed,overlapdata.wh$Language,get_unique_length_meaning)
# 	
# 	# get entropy of wh words
# 	# convert to special data frame
# 	overlapdata.matrix <- data.frame.to.matrix(overlapdata.wh)
# 	entropy.wh.words <- getWordListEntropy(overlapdata.matrix)
# 	
# 	#rank langs by max number of wh words, then max entropy, then max number of concepts
# 	rank_langs = order(num.of.Wh.words,entropy.wh.words,num.of.concepts,decreasing=T)
# 	# choose top ranking language
# 	language_to_keep = names(num.of.Wh.words)[rank_langs[1]]
# 		
# 	# keep only the chosen language
# 	alldata = alldata[alldata$glotto!=glotto.code | alldata$Language==language_to_keep,]
# 	
# 
# }

# Final restriction: Must have more than 400 meanings

numMeaningsPerLang = tapply(alldata$meaning.id.fixed, alldata$glotto, function(X){length(unique(X))})



alldata = alldata[!alldata$glotto %in% names(numMeaningsPerLang[numMeaningsPerLang<400]),]

allx =alldata[,c("Language",'glotto','Source')]
allx = allx[!duplicated(allx),]
write.csv(allx,file = 'LangsInAnalysis.csv', row.names = F, fileEncoding = 'utf-8')

# remove objects we don't need
loaded.functions <- as.vector(lsf.str())
do.not.delete <- c("alldata",'whwords')
rm(list=ls()[! ls() %in% c(loaded.functions,do.not.delete)])

