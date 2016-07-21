setwd("~/Documents/MPI/Switchboard/")

#d= read.csv("NewTorreiraLubbersData/datafile_fto_withSynDepth.txt")
#d= read.csv("NewTorreiraLubbersData/fto_utt_withSynDepthPLUS2_andLaughter_andSurprisal.csv",stringsAsFactors=F)

d = read.csv("NewTorreiraLubbersData/fto_utt_withSynDepthPLUS4.csv",stringsAsFactors=F)

d$time.log = log10(d$time)
d$sexA = as.factor(d$sexA)
d$sexB = as.factor(d$sexB)

whwords = c("who",'what','why','when','where','why')# how, how much, how many
whwords2 = c("who",'what','why','when','where','why','how')# how, how much, how many

whq = d[d$dialActB2.first %in% c("wh_q"),]

whq.first.word = sapply(whq$orthB,function(X){strsplit(X," ")[[1]]})

whq.numwords = sapply(whq$orthB,function(X){length(strsplit(X," ")[[1]])})
whq.whpos = sapply(whq$orthB,function(X){min(match(whwords,strsplit(X," ")[[1]]),na.rm=T)})

# TODO: count only grammatical 'what' etc.
whwords.count = rowSums(sapply(whq$orthB,function(X){match(whwords2,strsplit(X," ")[[1]])}),na.rm=T)
names(whwords.count) = whwords
sort(whwords.count)


whq.whpos[is.infinite(whq.whpos)] = NA

whq.whpos.rel = (whq.whpos-1) / (whq.numwords-1)

hist(whq.whpos.rel)

sum(whq.whpos<=2,na.rm=T) / sum(!is.na(whq.whpos))



nonwhTurns = d[!d$dialActB2.first %in% c("wh_q"),]
nonwhTurns.firstLetter = sapply(nonwhTurns$orthB,function(X){substr(X,0,1)})

# TODO: measure millisecond timing of wh word in question
sort(table(nonwhTurns.firstLetter))

nonwhTurns.firstWord = sapply(nonwhTurns$orthB,function(X){strsplit(X," ")[[1]][1]})

nonwhTurns.firstWord.wh = nonwhTurns.firstWord[substr(nonwhTurns.firstWord,0,1)=="w"]

sort(table(nonwhTurns.firstWord.wh))

nonwhTurns.firstLetter.noWell = nonwhTurns.firstLetter[nonwhTurns.firstWord!="well"]

sort(table(nonwhTurns.firstLetter.noWell))

d$firstWordB = sapply(d$orthB,function(X){strsplit(X," ")[[1]][1]})

head(d[d$firstWordB=="what" & d$dialActB2.first!="wh_q",]$orthB)




contentQ = d[d$firstWordB!="well",]$dialActB2.first %in% c("wh_q")
firstLetterW = sapply(d[d$firstWordB!="well",]$orthB,function(X){substr(X,0,1)=='w'})
table(contentQ,firstLetterW)/sum(table(d$dialActB2.first))


table(d$dialActB2.first)
