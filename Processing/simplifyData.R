rm(list=ls())

library(stringi)


try(setwd("U:/Pragmatics/Slonimska/Data/Processing/Matched_word_lists"))

try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Processing/Matched_word_lists"))

Alldata<-read.csv("Alldata.csv", stringsAsFactors=F, fileEncoding='utf-8')


# Elamite_Phonemic is actually multiple languages

el = Alldata[Alldata$Language=="Elamite_Phonemic" & !is.na(Alldata$Language),]
el = el[nchar(el$word)>0,]


Alldata = Alldata[Alldata$Language!="Elamite_Phonemic" & !is.na(Alldata$Language),]


el.split = data.frame()
for(i in 1:nrow(el)){
	elx = el[i,]
	wx = elx$word
	if(sum(grepl("\\:",wx))>0){
		wx = strsplit(wx,":")[[1]][2]
	} 
	wx = gsub(',','',wx)
	wxs = strsplit(wx,";")[[1]]
	langs = c("")
	for(j in 1:length(wxs)){
		
		langs = c("OE",'ME',"AchE",'NE')[sapply(c("OE",'ME',"AchE",'NE'), function(X){grepl(X,wxs[j])})]
				
		bits = strsplit(wxs[j],' ')[[1]]
		bits = bits[nchar(bits)>0]
		#form = bits[length(bits)]
		
		for(k in 1:(length(bits))){
			if(!bits[k]%in% c("OE",'ME',"AchE",'NE')){
				form = bits[k]
				for(lang in langs){
				el.split = rbind(el.split,elx)
				el.split[nrow(el.split),]$word = form
				el.split[nrow(el.split),]$Language = paste("Elamite_Phonemic",lang)
				}
			}
		}
	}
}

Alldata = rbind(Alldata,el.split)


Alldata$word[Alldata$word=='Unidentifiable'] = ""


Alldata$word.clean = Alldata$word

#remove diacritics
Alldata$word.clean = stri_trans_general(Alldata$word.clean , 'latin-ascii')

# remove stuff between round brackets
Alldata$word.clean = gsub("\\([^\\)]*\\)",' ',Alldata$word.clean)

Alldata$word.clean = gsub('[ʼ:ː \'\\-]','', Alldata$word.clean)

# only include greek stuff between brackets
Alldata$word.clean[Alldata$Language=='Greek' & grepl("\\[",Alldata$word)] = gsub(".+\\[(.+)\\]","\\1",Alldata$word.clean[Alldata$Language=='Greek' & grepl("\\[",Alldata$word)])

# for greek, some entries are puerly greek characters
Alldata$word.clean[Alldata$Language=='Greek' & !is.na(Alldata$Language)] = sapply(Alldata$word.clean[Alldata$Language=='Greek' & !is.na(Alldata$Language)],function(X){
	if(X==''){return(X)}
	if(
	((sum(unlist(strsplit(X,'')) %in% letters)/nchar(X)))<0.4){
		return("")} else{ return(X)}
})

# remove numbers
Alldata$word.clean = gsub("[0-9]","",Alldata$word.clean)
Alldata$word.clean = gsub("[₁₂³]","",Alldata$word.clean)

# handle question marks
# "(?)" should be removed
Alldata$word.clean = gsub("\\(\\?\\)","",Alldata$word.clean)
# in Thai, question marks are glottal stops
Alldata[Alldata$Language=='Thai',]$word.clean = gsub("\\?","ʔ",Alldata[Alldata$Language=='Thai',]$word.clean)

# in question words, final '?' should be removed
whwords= c("17.65" ,"17.62" ,'17.63',"17.61" ,"17.66" ,"17.67", "17.64" ,"17.68", "17.69")
Alldata$word.clean[Alldata$language_pk %in% whwords] = gsub("\\?$","",Alldata$word.clean[Alldata$language_pk %in% whwords])
Alldata$word.clean[Alldata$language_pk %in% whwords] = gsub("\\?$","",Alldata$word.clean[Alldata$language_pk %in% whwords])

# some entries use "?" as a splitter for alternatives
sep.langs = c("Chorote_Phonemic","Chinese","English", "Bantu","Proto-Austroasiatic","Modern Mon") # these langs should not be affected be the following rule
Alldata$word.clean[!Alldata$Language %in% sep.langs] = gsub("\\? ",'; ' ,Alldata$word.clean[!Alldata$Language %in% sep.langs])

# multiple ?
Alldata$word.clean = gsub("\\?\\?\\?","",Alldata$word.clean)
Alldata$word.clean = gsub("\\?\\?","",Alldata$word.clean)

# some languges have ? as a glottal stop
q.by.lang = sort(tapply(Alldata$word[!Alldata$language_pk %in% whwords],Alldata[!Alldata$language_pk %in% whwords,]$Language,function(X){sum(grepl("\\?",X))}))
q.with.glottal = names(q.by.lang)[q.by.lang>9]

for(qx in q.with.glottal){
	Alldata[Alldata$Language==qx,]$word.clean = gsub("\\?","ʔ",Alldata[Alldata$Language==qx,]$word.clean)
}


# question marks at start are glottal
Alldata$word.clean = gsub("^\\?","ʔ",Alldata$word.clean)
# remove all other question marks
Alldata$word.clean = gsub("\\?","",Alldata$word.clean)


Alldata[Alldata$Language=="Kotgarhi",]$word.clean = gsub("\\|","",Alldata[Alldata$Language=="Kotgarhi",]$word.clean)

Alldata[Alldata$Language=="Shan_Dialect_Northern_Shan_Phonemic",]$word.clean = gsub("\\|\\|","; ",Alldata[Alldata$Language=="Shan_Dialect_Northern_Shan_Phonemic",]$word.clean)




# ALTERNATIVES
# sometimes 'or' is used to denote alternatives
Alldata$word.clean = gsub(" or ",";",Alldata$word.clean)
# '/' is used to denote alternatives (if has spaces around it)
Alldata$word.clean = gsub(" / ",";",Alldata$word.clean)
# convert commas to semicolons
Alldata$word.clean[grepl(",",Alldata$word)] = gsub(",",";",Alldata$word.clean[grepl(",",Alldata$word)])
# ~ is used to break up alternatives
Alldata$word.clean= gsub("~",";",Alldata$word.clean)



# small fixes for phonetic transcriptions
Alldata[Alldata$word.clean=="buvol[buvol]",]$word.clean = 'buvol'
Alldata[Alldata$word.clean=='WC[vece]',]$word.clean = 'vece'
Alldata[Alldata$Language=='Karajá_Phonemic' & Alldata$word=="rora ['visit' ?]",]$word.clean = "rora"

# remove stuff between square brackets
Alldata$word.clean = gsub("\\[.+\\]","",Alldata$word.clean)

# for russian, remove stuff between round brackets
Alldata[Alldata$Language=='Russian' & !is.na(Alldata$Language),]$word.clean = gsub("\\(.+\\)","",Alldata[Alldata$Language=='Russian' & !is.na(Alldata$Language),]$word.clean)

Alldata[grepl("[дивкнчшкл]",Alldata$word.clean),]$word.clean = ""

Alldata[grepl("[ъыьҍю]",Alldata$word.clean),]$word.clean = ""

# remove round bracket characters
Alldata$word.clean = gsub("\\(","",Alldata$word.clean)
Alldata$word.clean = gsub("\\)","",Alldata$word.clean)

# remove square bracket characters
Alldata$word.clean = gsub("\\[","",Alldata$word.clean)
Alldata$word.clean = gsub("\\]","",Alldata$word.clean)

# remove full stops
Alldata$word.clean = gsub("\\.","",Alldata$word.clean)

# remove *
Alldata$word.clean = gsub("\\*","",Alldata$word.clean)



# '+' can indicate inflection + root???
Alldata$word.clean = gsub("\\+","",Alldata$word.clean)

# weird character
Alldata$word.clean = gsub("έ",'ε',Alldata$word.clean)

# dollar sign is end of line matcher
Alldata$word.clean = gsub("\\$","ʃ",Alldata$word.clean)

# reduced or uninflecter forms are seperated by '<'.  We choose to use the leftmost form
clines = grepl("<",Alldata$word.clean) & !grepl("\\*",Alldata$word.clean)
Alldata$word.clean[clines] = sapply(Alldata$word.clean[clines], function(X){
	x = strsplit(X,'<')[[1]]
	x[nchar(x)>0][1]
})

Alldata$word.clean = gsub('[/><]','',Alldata$word.clean)

############
# Make simplified column
Alldata$word.simple = Alldata$word.clean



#ac = read.delim("../CharacterSubstitutions/AllCharacters_ConsonantVowel_checked4.tab",sep=',', stringsAsFactors=F)
ac = read.delim("../CharacterSubstitutions/AllCharacters_ConsonantVowel_checked5.tab",sep=',', stringsAsFactors=F)


# Replace multi-character sounds with single characters
mapping2 = cbind(ac$Original,ac$Replacement)
mapping3 = cbind(ac$Original,ac$Simple)


# replace characters with simplified character
made.a.difference = c()
for(k in 1:nrow(mapping3)){
	orig = Alldata$word.clean
	Alldata$word.simple = gsub(mapping3[k,1],mapping3[k,2],Alldata$word.clean)		
	if(sum(orig!=Alldata$word.simple)>0){
		made.a.difference = c(made.a.difference,k)
	}
}

# there was originally a longer list, but this was cut down based on whether the character replacements actually applied.
#write.csv(ac[made.a.difference,],file="../CharacterSubstitutions/AllCharacters_ConsonantVowel_checked5.tab", fileEncoding='utf-8')


# replace multi-character sounds with single unique characters
for(k in 1:nrow(mapping2)){
	Alldata$word.clean = gsub(mapping2[k,1],mapping2[k,2],Alldata$word.clean)		

}


Alldata$word.simple = sapply(strsplit(Alldata$word.simple,''), function(X){
	ux = iconv(unlist(X),toRaw=T)
	paste(X[sapply(ux,length)<3],collapse='')
})


# remove cyrilyc characters in Bulgarian
to.take.away = Alldata[Alldata$Language=='Bulgarian' & !is.na(Alldata$Language),][1:24,]$X

Alldata[Alldata$X %in% to.take.away,]$word.clean = ""
Alldata[Alldata$X %in% to.take.away,]$word.simple = ""

# Macedonian only has cyrillic entries
Alldata[Alldata$Language=='Macedonian' & !is.na(Alldata$Language),]$word.clean = ''

Alldata[Alldata$Language=='Macedonian' & !is.na(Alldata$Language),]$word.simple = ''


unique.chars = sort(unique(unlist(strsplit(Alldata$word.simple,''))))

#unique.chars = sort(unique.chars[!unique.chars%in% letters])


write.csv(cbind(Original=unique.chars,Replacement="",Consonant="",Simple=''),file='../CharacterSubstitutions/extraChars2.csv',row.names=F, fileEncoding='utf-8',quote=F)

subs = read.csv('../CharacterSubstitutions/extraChars3.csv',quote="",stringsAsFactors=F,encoding='utf-8')

for(i in 1:nrow(subs)){
	if(nchar(subs[i,]$Original)>0){
		Alldata$word.simple = gsub(subs[i,]$Original,subs[i,]$Simple,Alldata$word.simple)
	}
}

# we have some duplicated data
Alldata = Alldata[!duplicated(Alldata[,c("Language",'Source','language_pk','word')]),]

# TODO!
# split alternatives into seperate rows 

# Alldata$originalEntry = NA
# # find rows with multiple columns
# nx = which(grepl(";", Alldata$word.simple))

# extraData = data.frame()

# for(i in nx){
	# # copy row
	# linex = Alldata[i,]
	# # find variants
	# wx = strsplit(linex$word.simple,";")[[1]]
	# if(length(wx)>1){
		# # original row is set to first variant
		# Alldata[i,]$word.simple = wx[1]
		# # keep track of where the variants came from
		# Alldata[i,]$originalEntry = i
		# for(j in 2:length(wx)){
			# # set dummy line to next variant
			# linex$word.simple=wx[j]
			# linex$originalEntry=i
			# # add extra line
			# extraData = rbind(extraData,linex)
		# }
	# }
# }
#
#Alldata = rbind(Alldata,extraData)


Alldata$language_pk = as.numeric(Alldata$language_pk)

# try guessing meaning id
meaning.ids = Alldata[nchar(Alldata$meaning)>0 & !is.na(Alldata$language_pk),]$language_pk
names(meaning.ids) = Alldata[nchar(Alldata$meaning)>0 & !is.na(Alldata$language_pk),]$meaning

# TODO: some meaning ids appear to be wrong.
number.of.ids = tapply(meaning.ids,names(meaning.ids),function(X){length(unique(X))})


# Some meanings have multiple ids
m.error = number.of.ids[number.of.ids>1]
# This is actually intended for some words
m.error = m.error[!names(m.error) %in% c('fork','knife','mortar','sow',"Easter")]

Alldata$meaning.id.fixed = Alldata$language_pk

Alldata[!is.na(Alldata$meaning) & !is.na(Alldata$language_pk) & Alldata$meaning=="Easter" & Alldata$language_pk==4.15,]$meaning.id = 22.99903

#fix odd meanings
for(mx in names(m.error)){
	tx = table(Alldata$language_pk[Alldata$meaning==mx])

	if((max(tx)/ (sum(tx))> 0.75) & sum(tx==max(tx))==1){
		correct.option = names(which(tx==max(tx)))
		#print(c(tx,mx))
		for(n in names(tx)){
			if(n != correct.option){
				Alldata[Alldata$language_pk==n & Alldata$meaning==mx & !is.na(Alldata$language_pk) & !is.na(Alldata$meaning),]$meaning.id.fixed = correct.option
				
			}
		}
	}
}


meaning.ids = Alldata[nchar(Alldata$meaning)>0 & !is.na(Alldata$language_pk),]$meaning.id.fixed
names(meaning.ids) = Alldata[nchar(Alldata$meaning)>0 & !is.na(Alldata$meaning.id.fixed),]$meaning
# Try to find missing meaning ids
# (only use meanings with a single meaning id associated with it)
single.meanings = meaning.ids[names(number.of.ids[number.of.ids==1])]

choicex = is.na(Alldata$meaning.id.fixed) & !is.na(Alldata$meaning) & nchar(Alldata$meaning)>0
Alldata[choicex,]$meaning.id.fixed = single.meanings[Alldata[choicex,]$meaning]


# change column name!
names(Alldata)[names(Alldata)=='language_pk'] = 'meaning.id'
Alldata = Alldata[,!names(Alldata) %in% c("X.1","X")]


Alldata$glotto = gsub(" ","",Alldata$glotto)

Alldata[Alldata$glotto=='noot1239' & !is.na(Alldata$glotto),]$glotto = "noot1238"
Alldata[Alldata$glotto=='tzot1264'& !is.na(Alldata$glotto),]$glotto = "tzot1259"
Alldata[Alldata$Language=='Selice Romani'& !is.na(Alldata$Language),]$glotto = "west2376"

# write data
write.csv(Alldata, file='../CleanedAndSimplifiedData/Alldata_simple.csv', fileEncoding='utf-8', row.names=F)