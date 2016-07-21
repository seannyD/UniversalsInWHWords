x1=names2glotto[colnames(d.wh.m)]

gcheck = grammar[grammar$Glotto %in% c(x1),]

write.csv(gcheck[,1:3],"../RAW_data/grammarCheck/LanguagesToCheck.csv", row.names = F)

write.csv(gcheck[,c(1:3,4,5,7,8,10,11)],"../RAW_data/grammarCheck/LanguagesToCheck_withReferenceAndPageNumbers.csv", row.names = F)


allGlotto = names2glotto[colnames(d.wh.m)]

allX = l.details[l.details$glotto %in% allGlotto,]
allX = allX[is.na(allX$InterrogativePosition),]

allX = allX[!allX$glotto %in% gcheck$Glotto,]

write.csv(allX[,c("N1","WALS","iso","glotto","area")],file='../RAW_data/grammarCheck/AllLangaugesInAnalysis.csv', row.names=F)

# have
have = table(l.details[l.details$glotto %in% gcheck$Glotto,]$area)
# could code
couldCode = table(l.details[l.details$glotto %in% allX$glotto,]$area)

couldCode - have[names(couldCode)]
