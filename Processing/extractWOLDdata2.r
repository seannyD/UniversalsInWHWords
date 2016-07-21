# see VisualiseWOLD_connections.xlsx

# start from word.csv, fill in name from unit.csv, get direct loan from target_word in loan.csv
# (may need to have seperate loan table)
# then: use couterpart.csv to link to value.csv to get meaning name from parameter.csv

setwd("/Users/pplsuser/Desktop/Stuff/EtymOnline_2013/Word loanword database/clld-wold2-dbe8bcf/data/")

library(jsonlite)

myFromJSON = function(X,f){
	#X = gsub("\t",",",X)
	#X = gsub('""','"',X)
	#X = substr(X,2,nchar(X)-1)
	return(jsonlite::fromJSON(X)[f])
}


value = read.csv("value.csv",stringsAsFactors=F)
valueset = read.csv("valueset.csv",stringsAsFactors=F)
meaning = read.csv('meaning.csv',stringsAsFactors=F)
word = read.csv("word.csv",stringsAsFactors=F)
parameter = read.csv('parameter.csv',stringsAsFactors=F)
semanticfield = read.csv("semanticfield.csv",stringsAsFactors=F)
language = read.csv('language.csv',stringsAsFactors=F)
unit = read.csv('unit.csv',stringsAsFactors=F)
loan = read.csv("loan.csv",stringsAsFactors = F)
unitdomainelement = read.csv("unitdomainelement.csv",stringsAsFactors=F)
unitvalue = read.csv("unitvalue.csv",stringsAsFactors=F)
counterpart = read.csv("counterpart.csv",stringsAsFactors=F)


word_to_unit = match(word$pk,unit$pk)

# some words do not have an entry in 'counterpart', which means it fails to get meaning for these


word$word = unit[word_to_unit,]$name
word$language_pk = unit[word_to_unit,]$language_pk
word$target_language = language[match(word$language_pk,language$pk),]$name

word$counterpart = counterpart[match(word$pk,counterpart$word_pk),]$pk
word$valueset_pk = value[match(word$counterpart,value$pk),]$valueset_pk
word$parameter_pk = valueset[match(word$valueset_pk,valueset$pk),]$parameter_pk
word$meaning = parameter[match(word$parameter_pk,parameter$pk),]$name
word$meaning.id = parameter[match(word$parameter_pk,parameter$pk),]$id

word$meaning.id= gsub("\\-",'\\.',word$meaning.id)


#word[word$target_language=="Arabic" & !is.na(word$target_language),]$meaning = unit[match(word[word$target_language=="Arabic" & !is.na(word$target_language),]$pk,unit$pk),]$description



# for some languages, meaning ids are missing
sum(is.na(word$meaning.id))

# if the meaning is missing, add the description from unit

word[is.na(word$meaning),]$meaning = unit[match(word[is.na(word$meaning),]$pk,unit$pk),]$description
word[word$meaning=='' & !is.na(word$meaning),]$meaning = NA

# find direct matches through meaning
word[is.na(word$meaning.id) & !is.na(word$meaning),]$meaning.id = word[!is.na(word$meaning.id) & !is.na(word$meaning),]$meaning.id[ match(word[is.na(word$meaning.id) & !is.na(word$meaning),]$meaning,word[!is.na(word$meaning.id) & !is.na(word$meaning),]$meaning)]
sum(is.na(word$meaning.id))

# take away 'the' etc.
xmeanings = gsub('the ','',word[is.na(word$meaning.id) & !is.na(word$meaning),]$meaning)
xmeanings = gsub('to ','',xmeanings)
word[is.na(word$meaning.id) & !is.na(word$meaning),]$meaning.id = word[!is.na(word$meaning.id) & !is.na(word$meaning),]$meaning.id[ match(xmeanings,word[!is.na(word$meaning.id) & !is.na(word$meaning),]$meaning)]
sum(is.na(word$meaning.id))

# find direct matches through description
missing.descriptions = unit[match(word[is.na(word$meaning.id) & !is.na(word$meaning),]$pk,unit$pk),]$description
wordx = word[!is.na(word$meaning.id) & !is.na(word$meaning),c("pk","meaning.id")]
word[is.na(word$meaning.id) & !is.na(word$meaning),]$meaning.id = sapply(missing.descriptions, function(X){
	if(is.na(X)){return(NA)}
	t = table(wordx[wordx$pk %in% unit[unit$description==X,]$pk,]$meaning.id)
	if(length(t)==0){return(NA)}
	return(names(t[which(t==max(t))])[1])
})
sum(is.na(word$meaning.id))


missing.meanings = gsub("\\[","",word[is.na(word$meaning.id) & !is.na(word$meaning),]$meaning)
missing.meanings = gsub("\\]","",missing.meanings)
missing.meanings = gsub("\\*","",missing.meanings)
missing.meanings = gsub("\\?","",missing.meanings)
missing.meanings = gsub("\\.","",missing.meanings)
missing.meanings = gsub("\\)","",missing.meanings)
missing.meanings = gsub("\\(","",missing.meanings)
missing.meanings = gsub("\\+","",missing.meanings)
wordx = word[!is.na(word$meaning.id) & !is.na(word$meaning),c("pk","meaning.id")]
word[is.na(word$meaning.id) & !is.na(word$meaning),]$meaning.id = sapply(missing.meanings, function(X){
	if(is.na(X)){return(NA)}	
	t = table(wordx[wordx$pk %in% unit[
		grepl(paste("[ ,^;]",X,"[$ ,;]",sep=''),unit$description)
		,]$pk,]$meaning.id)
	if(length(t)==0){return(NA)}
	ans = names(t[which(t==max(t))])[1]
	#print(c(X,ans))
	return(ans)
})
sum(is.na(word$meaning.id))


# try to find matches for meanings which have multiple entries
double.meanings = is.na(word$meaning.id) & !is.na(word$meaning) & grepl(";",word$meaning)

test = sapply(word[double.meanings,]$meaning, function(X){
	wx = strsplit(X,"; ")[[1]]
	# look in other meanings
	f = word[match(wx,word$meaning),]$meaning.id
	# look in unit descriptions
	f.unit = word[match(unit[match(wx,unit$description),]$pk, word$pk),]$meaning.id
	ret = c(f,f.unit)
	x = table(ret[!is.na(ret)])
	if(length(x)==0){return(NA)}
	return(names(x[which(x==max(x))])[1])
})

foundMeanings = unlist(tapply(word[!is.na(word$meaning) & !is.na(word$meaning.id),]$meaning,word[!is.na(word$meaning) & !is.na(word$meaning.id),]$meaning.id,function(X){unique(strsplit(gsub(',',';',X),split='; '))}))
meanings2id =  substr(names(foundMeanings),1,nchar(names(foundMeanings))-1)
names(meanings2id) = foundMeanings

mx = meanings2id[word[!is.na(word$target_language) & is.na(word$meaning.id) & !is.na(word$meaning),]$meaning]

word[!is.na(word$target_language) & is.na(word$meaning.id) & !is.na(word$meaning),][!is.na(mx),]$meaning.id = mx[!is.na(mx)]
sum(is.na(word$meaning.id))

#######
word$POS = meaning[match(word$parameter_pk,meaning$pk),]$semantic_category
word$semantic_field_pk = meaning[match(word$parameter_pk,meaning$pk),]$semantic_field_pk
word$semantic_field = semanticfield[match(word$semantic_field_pk,semanticfield$pk),]$name

#word.details = unit[word_to_unit,]$jsondata
#word$effect = NA
#word$grammatical_info = NA
#word$salience = NA
#word[,c("effect",'grammatical_info','salience')] = sapply(unit[word_to_unit,]$jsondata,function(X){fromJSON(X)[c("effect","grammatical_info","salience")]})

# match word to its original loan.  
# CAREFUL! there are more than 1:1 mapping from word$pk to loan$target_word!!
#word$loan = loan[match(word$pk,loan$target_word)]

# Maybe just make seperate table of word sources
loan$source_word = unit[match(loan$source_word_pk,unit$pk),]$name
loan$source_language_pk = unit[match(loan$source_word_pk,unit$pk),]$language_pk
loan$source_language = language[match(loan$source_language_pk,language$pk),]$name


# use unit_pk in unitvalue.csv (which is just the pk in word.csv) to link to unitdomainelement_pk in unitdomainelement.csv to get year of borrowing.

unitdomainelement$jsondata2 = gsub("[^0-9,\\-]*","",unitdomainelement$jsondata)
unitdomainelement$start_year = sapply(unitdomainelement$jsondata2,function(X){strsplit(X,",")[[1]][2]})
unitdomainelement$end_year = sapply(unitdomainelement$jsondata2,function(X){strsplit(X,",")[[1]][1]})


loan$unitdomainelement_pk = unitvalue[match(loan$target_word_pk,unitvalue$unit_pk),]$unitdomainelement_pk
loan$start_year = unitdomainelement[match(loan$unitdomainelement_pk,unitdomainelement$pk),]$start_year
loan$end_year = unitdomainelement[match(loan$unitdomainelement_pk,unitdomainelement$pk),]$end_year
loan$borrow_year = unitdomainelement[match(loan$unitdomainelement_pk,unitdomainelement$pk),]$name






write.csv(loan,"../../WOLD_dataextract/loan_processed.csv",row.names=F)
write.csv(word,"../../WOLD_dataextract/word_processed.csv",row.names=F, fileEncoding='utf-8')

