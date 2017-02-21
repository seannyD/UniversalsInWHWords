#data that has question words, all columns
d.wh = alldata[alldata$meaning.id.fixed %in% whwords,]
#variable for analyses - only wh-words words
d.wh.m = data.frame.to.matrix(d.wh)

fam = tapply(l.details$langFam,l.details$glotto,head,n=1)
area = tapply(l.details$area,l.details$glotto,head,n=1)
names2glotto = tapply(d.wh$glotto,d.wh$Language,head,n=1)
d.wh.glotto = names2glotto[colnames(d.wh.m)]

families.d.wh = fam[d.wh.glotto]
areas.d.wh = area[d.wh.glotto]

#library(maps)
#map()
#points(l.details[match(d.wh.glotto ,l.details$glotto),]$longitude,l.details[match(d.wh.glotto ,l.details$glotto),]$latitude,col=2,pch=16)

#Defining variables

#initial.glotto = l.details[l.details$InterrogativePosition=="1 Initial interrogative phrase" & !is.na(l.details$InterrogativePosition) & l.details$InterrogativePosition!="",]$glotto

#non.initial.glotto = l.details[l.details$InterrogativePosition!="1 Initial interrogative phrase" & !is.na(l.details$InterrogativePosition) & l.details$InterrogativePosition!="",]$glotto

#d.wh.initial = alldata[alldata$meaning.id.fixed %in% whwords & alldata$glotto %in% initial.glotto,]
#d.wh.initial.m = data.frame.to.matrix(d.wh.initial)

#d.wh.non.initial = alldata[alldata$meaning.id.fixed %in% whwords & alldata$glotto %in% non.initial.glotto,]
#d.wh.non.initial.m = data.frame.to.matrix(d.wh.non.initial)

#Define variables with grammars positioning info
#initial.grammar.glotto = l.details[l.details$walsandgrammars=="1 Initial interrogative phrase" & !is.na(l.details$walsandgrammars) & l.details$walsandgrammars!="",]$glotto

#non.initial.grammar.glotto = l.details[l.details$walsandgrammars=="2 Not initial interrogative phrase" & !is.na(l.details$walsandgrammars) & l.details$walsandgrammars!="",]$glotto

#d.wh.grammar.initial = alldata[alldata$meaning.id.fixed %in% whwords & alldata$glotto %in% initial.grammar.glotto,]
#d.wh.grammar.initial.m = data.frame.to.matrix(d.wh.grammar.initial)

#d.wh.grammar.non.initial = alldata[alldata$meaning.id.fixed %in% whwords & alldata$glotto %in% non.initial.grammar.glotto,]
#d.wh.grammar.non.initial.m = data.frame.to.matrix(d.wh.grammar.non.initial)


#Define variables with possible positioning info
initial.possible.glotto = l.details[l.details$qpos=="1 Initial interrogative phrase" & !is.na(l.details$qpos) & l.details$qpos!="",]$glotto

non.initial.possible.glotto = l.details[l.details$qpos=="2 Not initial interrogative phrase" & !is.na(l.details$qpos) & l.details$qpos!="",]$glotto

d.wh.possible.initial = alldata[alldata$meaning.id.fixed %in% whwords & alldata$glotto %in% initial.possible.glotto,]
d.wh.possible.initial.m = data.frame.to.matrix(d.wh.possible.initial)

d.wh.possible.non.initial = alldata[alldata$meaning.id.fixed %in% whwords & alldata$glotto %in% non.initial.possible.glotto,]
d.wh.possible.non.initial.m = data.frame.to.matrix(d.wh.possible.non.initial)

d.wh.possible.initial.glotto = names2glotto[colnames(d.wh.possible.initial.m)]
d.wh.possible.non.initial.glotto = names2glotto[colnames(d.wh.possible.non.initial.m)]

families.d.wh.possible.initial = fam[d.wh.possible.initial.glotto]
areas.d.wh.possible.initial = area[d.wh.possible.initial.glotto]

families.d.wh.possible.non.initial = fam[d.wh.possible.non.initial.glotto]
areas.d.wh.possible.non.initial = area[d.wh.possible.non.initial.glotto]

# create data matrix for random words

d.random=alldata[!alldata$meaning.id.fixed %in% whwords,]
d.random.m=data.frame.to.matrix(d.random)

# Random - positioning WALS
#d.random.initial = alldata[(!alldata$meaning.id.fixed %in% whwords) & alldata$glotto %in% initial.glotto,]
#d.random.initial.m = data.frame.to.matrix(d.random.initial)

#d.random.non.initial = alldata[(!alldata$meaning.id.fixed %in% whwords) & alldata$glotto %in% non.initial.glotto,]
#d.random.non.initial.m = data.frame.to.matrix(d.random.non.initial)

# Random- positioning Grammars
#d.random.grammar.initial = alldata[(!alldata$meaning.id.fixed %in% whwords) & alldata$glotto %in% initial.grammar.glotto,]
#d.random.grammar.initial.m = data.frame.to.matrix(d.random.grammar.initial)

#d.random.grammar.non.initial = alldata[(!alldata$meaning.id.fixed %in% whwords) & alldata$glotto %in% non.initial.grammar.glotto,]
#d.random.grammar.non.initial.m = data.frame.to.matrix(d.random.grammar.non.initial)

# Random- positioning Possible
d.random.possible.initial = alldata[(!alldata$meaning.id.fixed %in% whwords) & alldata$glotto %in% initial.possible.glotto,]
d.random.possible.initial.m = data.frame.to.matrix(d.random.possible.initial)

d.random.possible.non.initial = alldata[(!alldata$meaning.id.fixed %in% whwords) & alldata$glotto %in% non.initial.possible.glotto,]
d.random.possible.non.initial.m = data.frame.to.matrix(d.random.possible.non.initial)


################
# Versions with only vowels or only consonants

# load character conversion file
charList = read.csv("../Processing/CharacterSubstitutions/extraChars3.csv",quote="",stringsAsFactors=F)
charList = charList[charList$Consonant!="",]

# define list of vowels
vowels = unique(charList[charList$Consonant=="V",]$Simple)
vowels = vowels[nchar(vowels)>0]
vowels = unique(c('a','e','i','o','u','y',vowels))

# define list of consonants
consonants = unique(charList[charList$Consonant=="C",]$Simple)
consonants = consonants[nchar(consonants)>0]
consonants = unique(c(letters[! letters %in% vowels], "ʒ",consonants))

# remove consonants with regular expressions
consRegExpr = paste("[^",paste(consonants,collapse=''),"]",sep='')
d.wh.consonantsOnly.m = t(apply(d.wh.m,1,function(X){
	gsub(consRegExpr,"",X)
	}))
# remove vowels with regular expressions
vowRegExpr = paste("[^",paste(vowels,collapse=''),"]",sep='')
d.wh.vowelsOnly.m = t(apply(d.wh.m,1,function(X){
	gsub(vowRegExpr,"",X)
	}))	

#WALS
  #consonants
#d.wh.initial.consonantsOnly.m = t(apply(d.wh.initial.m,1,function(X){
#  gsub(consRegExpr,"",X)
#}))
#d.wh.non.initial.consonantsOnly.m = t(apply(d.wh.non.initial.m,1,function(X){
#  gsub(consRegExpr,"",X)
#}))
  #vowels
#d.wh.initial.vowelsOnly.m = t(apply(d.wh.initial.m,1,function(X){
#  gsub(vowRegExpr,"",X)
#}))
#d.wh.non.initial.vowelsOnly.m = t(apply(d.wh.non.initial.m,1,function(X){
#  gsub(vowRegExpr,"",X)
#}))

#Grammar
  #consonants
#d.wh.grammar.initial.consonantsOnly.m = t(apply(d.wh.grammar.initial.m,1,function(X){
#  gsub(consRegExpr,"",X)
#}))
#d.wh.grammar.non.initial.consonantsOnly.m = t(apply(d.wh.grammar.non.initial.m,1,function(X){
#  gsub(consRegExpr,"",X)
#}))

  #vowels
# d.wh.grammar.initial.vowelsOnly.m = t(apply(d.wh.grammar.initial.m,1,function(X){
#   gsub(vowRegExpr,"",X)
# }))
# d.wh.grammar.non.initial.vowelsOnly.m = t(apply(d.wh.grammar.non.initial.m,1,function(X){
#   gsub(vowRegExpr,"",X)
# }))

#Possible
  #Consonants
d.wh.possible.initial.consonantsOnly.m = t(apply(d.wh.possible.initial.m,1,function(X){
  gsub(consRegExpr,"",X)
}))
d.wh.possible.non.initial.consonantsOnly.m = t(apply(d.wh.possible.non.initial.m,1,function(X){
  gsub(consRegExpr,"",X)
}))

  #Vowels
d.wh.possible.initial.vowelsOnly.m = t(apply(d.wh.possible.initial.m,1,function(X){
  gsub(vowRegExpr,"",X)
}))
d.wh.possible.non.initial.vowelsOnly.m = t(apply(d.wh.possible.non.initial.m,1,function(X){
  gsub(vowRegExpr,"",X)
}))

########
# Make specific lists of concepts
  #face= head(4.200), face(4.204), forehead (4.205), cheek(4.208), chin(4.209), eye(4.210), ear (4.220), nose(4.230), mouth(4.240)
  body.concepts = as.numeric(c("4.200","4.204","4.205","4.208", "4.209", "4.210", "4.220", "4.230", "4.240"))
  d.BodyConcepts = alldata[alldata$meaning.id.fixed %in% body.concepts,]
  d.BodyConcepts.m = data.frame.to.matrix(d.BodyConcepts)
  
d.BodyConcepts.initial = d.BodyConcepts[d.BodyConcepts$glotto %in% initial.possible.glotto,]
d.BodyConcepts.initial.m = data.frame.to.matrix(d.BodyConcepts.initial)

d.BodyConcepts.non.initial = d.BodyConcepts[d.BodyConcepts$glotto %in% non.initial.possible.glotto,]
d.BodyConcepts.non.initial.m = data.frame.to.matrix(d.BodyConcepts.non.initial)


d.BodyConcepts.glotto.initial = names2glotto[colnames(d.BodyConcepts.initial.m)]
d.BodyConcepts.glotto.non.initial = names2glotto[colnames(d.BodyConcepts.non.initial.m)]

families.d.BodyConcepts.initial = fam[d.BodyConcepts.glotto.initial]
areas.d.BodyConcepts.initial = area[d.BodyConcepts.glotto.initial]

families.d.BodyConcepts.non.initial = fam[d.BodyConcepts.glotto.non.initial]
areas.d.BodyConcepts.non.initial = area[d.BodyConcepts.glotto.non.initial]



  #basic actions = do,make ("9.110"),fold ("9.150"),work ("9.120"),break ("9.260"),pull("9.330"),press ("9.342"),wash("9.360"), pour("9.350"),build ("9.440")
  basic.actions.concepts= as.numeric(c("9.110","9.150","9.120","9.260","9.330","9.342","9.360","9.350","9.440"))
  d.BasicActionsConcepts = alldata[alldata$meaning.id.fixed %in% basic.actions.concepts,]
  d.BasicActionsConcepts.m = data.frame.to.matrix(d.BasicActionsConcepts)
  
d.BasicActionsConcepts.initial = d.BasicActionsConcepts[d.BasicActionsConcepts$glotto %in% initial.possible.glotto,]
d.BasicActionsConcepts.initial.m = data.frame.to.matrix(d.BasicActionsConcepts.initial)

d.BasicActionsConcepts.non.initial = d.BasicActionsConcepts[d.BasicActionsConcepts$glotto %in% non.initial.possible.glotto,]
d.BasicActionsConcepts.non.initial.m = data.frame.to.matrix(d.BasicActionsConcepts.non.initial)


d.BasicActionsConcepts.glotto.initial = names2glotto[colnames(d.BasicActionsConcepts.initial.m)]
d.BasicActionsConcepts.glotto.non.initial = names2glotto[colnames(d.BasicActionsConcepts.non.initial.m)]

families.d.BasicActions.initial = fam[d.BasicActionsConcepts.glotto.initial]
areas.d.BasicActions.initial = area[d.BasicActionsConcepts.glotto.initial]

families.d.BasicActions.non.initial = fam[d.BasicActionsConcepts.glotto.non.initial]
areas.d.BasicActions.non.initial = area[d.BasicActionsConcepts.glotto.non.initial]


# pronouns

pronouns = as.numeric(c("2.91","2.92","2.93","2.931","2.932","2.933","2.94","2.941","2.942","2.95","2.96"))
d.PronounConcepts = alldata[alldata$meaning.id.fixed %in% pronouns,]
d.PronounConcepts.m = data.frame.to.matrix(d.PronounConcepts)

d.PronounConcepts.initial = d.PronounConcepts[d.PronounConcepts$glotto %in% initial.possible.glotto,]
d.PronounConcepts.initial.m = data.frame.to.matrix(d.PronounConcepts.initial)

d.PronounConcepts.non.initial = d.PronounConcepts[d.PronounConcepts$glotto %in% non.initial.possible.glotto,]
d.PronounConcepts.non.initial.m = data.frame.to.matrix(d.PronounConcepts.non.initial)


d.PronounConcepts.glotto.initial = names2glotto[colnames(d.PronounConcepts.initial.m)]
d.PronounConcepts.glotto.non.initial = names2glotto[colnames(d.PronounConcepts.non.initial.m)]

families.d.PronounConcepts.initial = fam[d.PronounConcepts.glotto.initial]
areas.d.PronounConcepts.initial = area[d.PronounConcepts.glotto.initial]

families.d.PronounConcepts.non.initial = fam[d.PronounConcepts.glotto.non.initial]
areas.d.PronounConcepts.non.initial = area[d.PronounConcepts.glotto.non.initial]

########################
# only unanalysable data

d.random.unanalyzable = alldata[alldata$Source=='WOLD' & !is.na(alldata$analyzability) & alldata$analyzability =="unanalyzable" & (!alldata$meaning.id.fixed %in% whwords),]
d.random.unanalyzable.m = data.frame.to.matrix(d.random.unanalyzable)

d.unanalyzable.wh = alldata[alldata$Source=='WOLD' & !is.na(alldata$analyzability) & alldata$analyzability =="unanalyzable" & alldata$meaning.id.fixed %in% whwords,]
d.unanalyzable.wh.m = data.frame.to.matrix(d.unanalyzable.wh)

d.unanalyzable.BodyConcepts = alldata[alldata$Source=='WOLD' & !is.na(alldata$analyzability) & alldata$analyzability =="unanalyzable" & alldata$meaning.id.fixed %in% body.concepts,]
d.unanalyzable.BodyConcepts.m = data.frame.to.matrix(d.unanalyzable.BodyConcepts)

d.unanalyzable.BasicActionConcepts = alldata[alldata$Source=='WOLD' & !is.na(alldata$analyzability) & alldata$analyzability =="unanalyzable" & alldata$meaning.id.fixed %in% basic.actions.concepts,]
d.unanalyzable.BasicActionConcepts.m = data.frame.to.matrix(d.unanalyzable.BasicActionConcepts)

d.unanalyzable.PronounConcepts = alldata[alldata$Source=='WOLD' & !is.na(alldata$analyzability) & alldata$analyzability =="unanalyzable" & alldata$meaning.id.fixed %in% pronouns,]
d.unanalyzable.PronounConcepts.m = data.frame.to.matrix(d.unanalyzable.PronounConcepts)

# proportion of analyzable words
sum(is.na(d.PronounConcepts.m)) / prod(dim(d.PronounConcepts.m))
sum(is.na(d.unanalyzable.PronounConcepts.m)) / prod(dim(d.unanalyzable.PronounConcepts.m))

sum(is.na(d.unanalyzable.wh.m)) / prod(dim(d.unanalyzable.wh.m))


pronx = table(alldata[alldata$Source=='WOLD' & !is.na(alldata$analyzability) & alldata$meaning.id.fixed %in% pronouns,]$analyzability == 'unanalyzable')
pronx / sum(pronx)


whx = table(alldata[alldata$Source=='WOLD' & !is.na(alldata$analyzability) & alldata$meaning.id.fixed %in% whwords,]$analyzability == 'unanalyzable')
whx / sum(whx)

# take out languages with less than 4 items 
d.unanalyzable.wh.m = d.unanalyzable.wh.m[,apply(d.unanalyzable.wh.m,2,function(X){sum(!is.na(X))>=4})]
# keep only languages in d.unanalyzable.wh.m
d.random.unanalyzable.m = d.random.unanalyzable.m[,colnames(d.random.unanalyzable.m) %in% colnames(d.unanalyzable.wh.m)]
d.unanalyzable.BodyConcepts.m= d.unanalyzable.BodyConcepts.m[,colnames(d.unanalyzable.BodyConcepts.m) %in% colnames(d.unanalyzable.wh.m)]
d.unanalyzable.BasicActionConcepts.m = d.unanalyzable.BasicActionConcepts.m[,colnames(d.unanalyzable.BasicActionConcepts.m) %in% colnames(d.unanalyzable.wh.m)]
d.unanalyzable.PronounConcepts.m = d.unanalyzable.PronounConcepts.m[,colnames(d.unanalyzable.PronounConcepts.m) %in% colnames(d.unanalyzable.wh.m)]


d.unanalyzable.wh.glotto = names2glotto[colnames(d.unanalyzable.wh.m)]

families.d.unanalyzable.wh = fam[d.unanalyzable.wh.glotto]
areas.d.unanalyzable.wh = area[d.unanalyzable.wh.glotto]

d.unanalyzable.wh.initial = d.unanalyzable.wh[d.unanalyzable.wh$glotto %in% initial.possible.glotto,]
d.unanalyzable.wh.initial.m = data.frame.to.matrix(d.unanalyzable.wh.initial)
# take out languages with less than 4 items
d.unanalyzable.wh.initial.m = d.unanalyzable.wh.initial.m[,apply(d.unanalyzable.wh.initial.m,2,function(X){sum(!is.na(X))>=4})]

d.unanalyzable.wh.non.initial = d.unanalyzable.wh[d.unanalyzable.wh$glotto %in% non.initial.possible.glotto,]
d.unanalyzable.wh.non.initial.m = data.frame.to.matrix(d.unanalyzable.wh.non.initial)
# take out languages with less than 4 items
d.unanalyzable.wh.non.initial.m = d.unanalyzable.wh.non.initial.m[,apply(d.unanalyzable.wh.non.initial.m,2,function(X){sum(!is.na(X))>=4})]

d.unanalyzable.wh.initial.glotto = names2glotto[colnames(d.unanalyzable.wh.initial.m)]

families.d.unanalyzable.wh.initial = fam[d.unanalyzable.wh.initial.glotto]
areas.d.unanalyzable.wh.initial = area[d.unanalyzable.wh.initial.glotto]

d.unanalyzable.wh.non.initial.glotto = names2glotto[colnames(d.unanalyzable.wh.non.initial.m)]

families.d.unanalyzable.wh.non.initial = fam[d.unanalyzable.wh.non.initial.glotto]
areas.d.unanalyzable.wh.non.initial = area[d.unanalyzable.wh.non.initial.glotto]

# Remove all 
#loaded.functions <- as.vector(lsf.str())
# data.matrices.to.keep = ls()[grepl("\\.m",ls())]
# areasFams.to.keep = c ( ls()[grepl("area",ls())], ls()[grepl("families",ls())])
#toKeep  = c(areasFams.to.keep,data.matrices.to.keep, loaded.functions)
#rm(list = ls()[! ls() %in% toKeep])

to.remove = ls()[ (grepl("^d\\.",ls()) & !grepl("\\.m$",ls()))]
rm(list=to.remove)
