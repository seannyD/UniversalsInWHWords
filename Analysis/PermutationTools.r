library(entropy)

myEntropy = function(freqs,method="efficiency"){
	# where freqs is a table of frequencies
	if(method=="efficiency"){
	# Efficiency (entropy divided by maximum entropy):
	ps = freqs/sum(freqs)
	# H = -sum(ifelse(ps > 0, ps * log(ps), 0))
	# table will never return zero count
	 return(-sum( ps * log(ps)) / log(sum(freqs)))
	 }
	 else{
	#empirical entropy
	return(entropy::entropy(freqs))
	 }
}

#getEntropy = function(segments){
#	apply(segments,1,function(X){myEntropy(table(X))})
#}

shuffleMatrix2 = function(m){
	return(apply(m,2,sample))
}

shuffleMatrix1 = function(m){
	return(t(apply(m,1,sample)))
}

famSample = function(X,fam){
	ss = tapply(X,fam,function(Z){
			if(length(Z)>1){
				return(sample(Z))
			}
			else{
				return(Z)
			}
			})
	return(unlist(ss))
}


stratifiedShuffle = function(m,fam){
	return(t(apply(m,1,famSample,fam=fam)))
}

# swapData = function(wordList1,wordList2){
	# combined = cbind(wordList1,wordList2)[,sample(1:(ncol(wordList1)+ncol(wordList2)))]
	# return(list(combined[,1:ncol(wordList1)],combined[,(ncol(wordList1)+1):ncol(combined)]))
# }


getWordListEntropy = function(wordList,firstSegment=F){
	langEntropy = apply(wordList,2,function(X){	
	
		sx = unlist(strsplit(X,";"))
		if(firstSegment){
			sx = substr(sx,1,1)
		}
		segs = unlist(strsplit(sx,""))
		uSegs = unique(segs)
		uSegs = uSegs[uSegs!=""]
		charmap = 1:length(uSegs)
		names(charmap) = uSegs
		myEntropy(table(charmap[segs]))
	})
	return(langEntropy)
}

plotPermutation = function(permutedEntropy1,trueEntropySum1,permName){
	#px = gsub("_","",permName)
	px = ""
	pdf(file=paste("../Results/SimplifiedPhonology/Graphs/",permName,'.pdf',sep=''))
	try(hist(colSums(permutedEntropy1),xlim=c(min(c(colSums(permutedEntropy1),trueEntropySum1)),max(colSums(permutedEntropy1))*1.03),xlab='Sum of Entropy',ylab="Number of permutations",main=px))
	try(abline(v=trueEntropySum1,col=2))
	dev.off()
	
		trueEntropyMean = trueEntropySum1/dim(permutedEntropy1)[1]
	
		pdf(file=paste("../Results/SimplifiedPhonology/Graphs/",permName,'_Mean.pdf',sep=''))
	try(hist(colMeans(permutedEntropy1),xlim=c(min(c(colMeans(permutedEntropy1),trueEntropyMean)),max(colMeans(permutedEntropy1))*1.03),xlab='Mean Entropy',ylab="Number of permutations",main=px))
	try(abline(v=trueEntropyMean,col=2))
	dev.off()
}

clearResultsFile = function(resultsFile){	
	#header = "PermName\tTrueValue\tPermutedMean\tTrue.greater.than.perm\ttotalPerms"
	cat("",file=resultsFile)
}

savePermutation = function(number.of.perms,trueEntropySum1,permutedEntropy1,permName,blankLine=T){
		out = cbind(c("True",rep('Perm',number.of.perms)),c(trueEntropySum1,permutedEntropy1))
	colnames(out) = c("Type",'SumEntropy')
	write.csv(out,file=paste("../Results/SimplifiedPhonology/PermutationResults/",permName,'.csv',sep=''),row.names=F)
	
	summaryString = paste(permName,": True = ",trueEntropySum1,", Mean of permutations = ",mean(permutedEntropy1),". True > permutation in ",sum(trueEntropySum1>permutedEntropy1),"out of",sum(!is.na(permutedEntropy1)),"permutations.", "\n")
	if(blankLine){
		summaryString = paste(summaryString,"\n\n",sep='')
	}
	cat(summaryString,file=resultsFile,append=T)
	
}

runPermutation = function(wordList,permName,firstSegment=F,stratified=F,families=NA,plot=F,useMeans=T){
	
	trueEntropySum1 = getWordListEntropy(wordList,firstSegment=firstSegment)
	if(useMeans){
		trueEntropySum1 = mean(trueEntropySum1,na.rm=T)
	} else{trueEntropySum1 = sum(trueEntropySum1,na.rm=T)}
	
	
	permutedEntropy1 = NA
	if(stratified){
		permutedEntropy1 = replicate(number.of.perms, getWordListEntropy(stratifiedShuffle(wordList,families), firstSegment=firstSegment))
		}
		else{	
	permutedEntropy1 = replicate(number.of.perms, getWordListEntropy(shuffleMatrix1(wordList),  firstSegment=firstSegment))
	}
	#permutedEntropy1 = permutedEntropy1[!is.na(permutedEntropy1) & !is.nan(permutedEntropy1)]
	if(plot){plotPermutation(permutedEntropy1,trueEntropySum1,permName)}
	
	toSave = NA
	if(useMeans){
		toSave = colMeans(permutedEntropy1,na.rm=T)
	} else{toSave = colSums(permutedEntropy1,na.rm=T)}
	savePermutation(number.of.perms, trueEntropySum1, toSave, permName)
	
	
}


runComparisonPermutation = function(wordList1,wordList2,permName,firstSegment=F, plot=F){
	trueEntropy1 = getWordListEntropy(wordList1,firstSegment=firstSegment)
	trueEntropy2 = getWordListEntropy(wordList2,firstSegment=firstSegment)
	

	trueEntDiff = diff(c(mean(trueEntropy1,na.rm=T),mean(trueEntropy2,na.rm=T)))
	
	# swap 
	splitSets = c(rep(1,length(trueEntropy1)), rep(2,length(trueEntropy2)))
	permutedEntropy1 = replicate(number.of.perms, diff( unlist( lapply( split( sample(c(trueEntropy1,trueEntropy2)),splitSets),mean,na.rm=T))))

# Bootstrap tests	
#	num.samples=  min(length(trueEntropy1),length(trueEntropy2))
#	splitSets = c(rep(1,num.samples), rep(2,num.samples))
	
#	permutedEntropy1 = replicate(number.of.perms, diff( unlist( lapply( split( sample(c(sample(trueEntropy1,num.samples),sample(trueEntropy2,num.samples))), splitSets),mean))))	
	if(plot){
	#px = gsub("_","",permName)
	px = ""
	extra = diff(range(c(permutedEntropy1,trueEntDiff)))*1.03
	xlimX = c(min(c(permutedEntropy1,trueEntDiff))-extra,max(c(permutedEntropy1,trueEntDiff))+extra)
	
	
	pdf(file=paste("../Results/SimplifiedPhonology/Graphs/",permName,'.pdf',sep=''))
	try(hist(permutedEntropy1,xlab='Difference in Sum of Entropy',ylab="Number of permutations",main=px,xlim=xlimX))
	try(abline(v=trueEntDiff,col=2))
	dev.off()
	}
	
	savePermutation(number.of.perms, trueEntDiff, permutedEntropy1, permName, blankLine=F)
	gtZero = paste("    Number of permutations with difference greater than 0 = ",sum(permutedEntropy1>0),"out of",number.of.perms,"\n")
	cat(gtZero,file=resultsFile,append=T)
	
	gtAbs = paste("    Number of permutations where absolute difference in true entropies is greater than the absolute difference in permuted entropies = ",sum(abs(trueEntDiff)>abs(permutedEntropy1)),"out of",number.of.perms,"\n")
	cat(gtAbs,file=resultsFile,append=T)
	
	tx = t.test(trueEntropy1,trueEntropy2)
	txx = paste("    t = ",tx$statistic,", df = ",tx$parameter,", p = ",tx$p.value,"\n\n\n")
	cat(txx,file=resultsFile,append=T)
	
}

permuteStratified = function(wordList1,wordList2,strat1,strat2,firstSegment){
	wx = cbind(wordList1,wordList2)
	ustrat1 = unique(strat1)
	ustrat2 = unique(strat2)
	sx = sample(c(ustrat1,ustrat2))
	sx1 = head(sx,length(ustrat1))
	sx2 = tail(sx,length(ustrat2))
	wx1 = wx[,c(strat1,strat2) %in% sx1]
	wx2 = wx[,c(strat1,strat2) %in% sx2]
	return(diff(c(mean(getWordListEntropy(wx1,firstSegment)), mean(getWordListEntropy(wx2,firstSegment)))))
}

runComparisonPermutationStratified = function(wordList1,wordList2,strat1,strat2,permName,firstSegment=F){
	
	# Run a permutation test, swapping whole FAMILIES between groups, keeping the number of families in each group the same as in the true lists.
	
	trueEntropy1 = getWordListEntropy(wordList1,firstSegment=firstSegment)
	trueEntropy2 = getWordListEntropy(wordList2,firstSegment=firstSegment)

	trueEntDiff = diff(c(mean(trueEntropy1,na.rm=T),mean(trueEntropy2,na.rm=T)))
	
	permutedEntropy1 = replicate(number.of.perms, permuteStratified(wordList1,wordList2,strat1,strat2,firstSegment))
	
	
		savePermutation(number.of.perms, trueEntDiff, permutedEntropy1, permName, blankLine=F)
	gtZero = paste("    Number of permutations with difference greater than 0 = ",sum(permutedEntropy1>0),"out of",number.of.perms,"\n")
	cat(gtZero,file=resultsFile,append=T)
	
	gtAbs = paste("    Number of permutations where absolute difference in true entropies is greater than the absolute difference in permuted entropies = ",sum(abs(trueEntDiff)>abs(permutedEntropy1)),"out of",number.of.perms,"\n\n\n")
	cat(gtAbs,file=resultsFile,append=T)
	
	
	
}

runRandomComparison = function(Random.initial.m, Random.non.initial.m, number.of.random.samples, permName, number.of.concepts = 9) {
sapply(1:number.of.random.samples, function(run.number){
	# select random concepts
	rx = sample(1:nrow(Random.initial.m),number.of.concepts)
	initial.m = Random.initial.m[rx,]
	non.initial.m = Random.non.initial.m[rx,]
	# run a permutation (make sure there's a 'RandomInit' folder)
	runComparisonPermutation(initial.m,non.initial.m,  paste(permName,"_allSegments_",run.number,sep=''),  firstSegment=F)
	runComparisonPermutation(initial.m,non.initial.m,  paste(permName,"_firstSegment_",run.number,sep=''),  firstSegment=T)
	return(NA)
})
return(NA)
}


getRandomConceptEntropy = function(wordList,firstSegment=F, domain=NA){
  
  choose.from.rows= 1:nrow(wordList)
  if(!is.na(domain)){
    domainString = paste("^",domain,"\\.",sep='')
    
    choose.from.rows = which(grepl(domainString,as.character(rownames(wordList))),arr.ind=T)
  }
  
  #randomly sample 9 words from  meaning.id
  rw<-sample(choose.from.rows, 9)
  rw.lang.m=wordList[rw,]
  return(
    mean(getWordListEntropy(rw.lang.m,firstSegment=firstSegment),na.rm=T)
  )
}

runComparison.wh.domain.permutation = function(WHwordList1,Randomwordlist2, permName,firstSegment=F){
  # work out true entropy of wh words
  trueEntropySum1 = mean(getWordListEntropy(WHwordList1,firstSegment=firstSegment))
  
  # find domains with enough concepts
  concepts.per.domain = tapply(alldata$meaning.id.fixed,alldata$domain,function(X){length(unique(X))})
  domains.with.enough.concepts = names(concepts.per.domain)[concepts.per.domain>=9]
  
  # for each domain
  for(i in 1:length(domains.with.enough.concepts)){
    dom = domains.with.enough.concepts[i]
    # get lots of mean entropies
    random.concept.entropy = replicate(number.of.perms, getRandomConceptEntropy(Randomwordlist2,firstSegment=firstSegment, domain=dom))
    savePermutation(number.of.perms, trueEntropySum1, random.concept.entropy, paste(permName,"_concept_",dom,sep=''))
  }
}



runComparison.wh.random.permutation = function(WHwordList1,Randomwordlist2, permName,firstSegment=F){
  trueEntropySum1 = mean(getWordListEntropy(WHwordList1,firstSegment=firstSegment))
  random.concept.entropy = replicate(number.of.perms, getRandomConceptEntropy(Randomwordlist2,firstSegment=firstSegment))
  
  # plotPermutation( random.concept.entropy,trueEntropySum1,permName)
  savePermutation(number.of.perms, trueEntropySum1, random.concept.entropy, permName)
  
  
}

data.frame.to.matrix = function(dx, word.column='word.simple'){
	# merge entries, 
	meanings = unique(dx$meaning.id.fixed)
	# use tapply so order is identical to below
	langs = tapply(dx$Language,dx$Language,head,n=1)
	ret = matrix(NA,nrow=length(meanings), ncol = length(langs))
	colnames(ret) = langs
	rownames(ret) = meanings
	for(i in 1:length(meanings)){
		mx = meanings[i]
		joined.meanings = tapply(dx[dx$meaning.id.fixed==mx,word.column],dx$Language[dx$meaning.id.fixed==mx] , function(X){
			# join together all entries
			jm = paste(X,collapse=';')
			# return a list of only unique entries
			return(paste(unique(strsplit(jm,";")[[1]]),collapse=';'))
			}
			)
		
		ret[i,] = unlist(joined.meanings[langs])
		
	}
	ret[nchar(ret)==0] = NA
	return(ret)
}
	
	
	
clearResultsFolders = function(){
	# CAREFUL!  This deletes results FILES
	folders = c("../Results/SimplifiedPhonology/Graphs/","../Results/SimplifiedPhonology/Graphs/InterrogativeOrder","../Results/SimplifiedPhonology/Graphs/RandomConcepts","../Results/SimplifiedPhonology/Graphs/RandomInterrogativeOrder")
	for(f in folders){
		x = list.files(f,"*.pdf")
		file.remove(paste(f,x,sep=''))
	}
	folders = c("../Results/SimplifiedPhonology/PermutationResults/","../Results/SimplifiedPhonology/PermutationResults/InterrogativeOrder","../Results/SimplifiedPhonology/PermutationResults/RandomConcepts","../Results/SimplifiedPhonology/PermutationResults/RandomInterrogativeOrder")
	for(f in folders){
		x = list.files(f,"*.csv")
		file.remove(paste(f,x,sep=''))
	}
	
	
}



runRandomPermutation = function(wordList,permName, firstSegment=F,stratified=F,families=NA){
	sapply(1:number.of.random.samples, function(X){
		runPermutation(wordList[sample(1:nrow(wordList),9),],paste(permName,'_',X,sep=''),firstSegment,stratified,families,plot=F,useMeans=T)
	})
}




runRandomPermutation_Domain = function(Randomwordlist2, permName,firstSegment=F,stratified=F,families=NA){
  
  # find domains with enough concepts
  concepts.per.domain = tapply(alldata$meaning.id.fixed,alldata$domain,function(X){length(unique(X))})
  domains.with.enough.concepts = names(concepts.per.domain)[concepts.per.domain>=9]
  
  # for each domain
  for(i in 1:length(domains.with.enough.concepts)){
 	
    dom = domains.with.enough.concepts[i]
    domainString = paste("^",dom,"\\.",sep='')
    choose.from.rows = which(grepl(domainString,as.character(rownames(Randomwordlist2))),arr.ind=T)
    dx = Randomwordlist2[choose.from.rows,]
    # get lots of mean entropies
    random.concept.entropy = sapply(1:number.of.perms, function(X){runPermutation(dx[sample(1:nrow(dx),9),],paste(permName,"_",dom,"_",X,sep=''),firstSegment=firstSegment, stratified=stratified, families=families, plot=F,useMeans=T)})
  }
}



selectRandomIndependentSample = function(wordList,strat,limit=ncol(wordList)){
	x = wordList[,tapply(1:ncol(wordList),strat,sample,size=1)]
	# out of this list, return 'limit' x languages
	return(x[,sample(1:ncol(x),size=limit)])
}


# Random independent sample test
runComparison.randomSample = function(wordList1,wordList2,strat1,strat2,permName,firstSegment=F){
	
	n.lang = min(length(unique(strat1)),length(unique(strat2)))
	# compare the means of two random independent samples
	rand.indep.comparison = replicate(number.of.perms, 
		mean( 
		getWordListEntropy(selectRandomIndependentSample(wordList1,strat1,limit=n.lang), firstSegment)  
		) - 
		mean(
		 getWordListEntropy(selectRandomIndependentSample(wordList2,strat2,limit=n.lang),firstSegment)
		 )
	) # end of replicate
	
	#summaryString = paste(permName,":","Number of random samples where group 1 mean entropy > group 2 mean entropy = ",sum(rand.indep.comparison,na.rm=T), "out of",sum(!is.na(rand.indep.comparison)),'permutations\n\n')
	
	mz = mean(rand.indep.comparison,na.rm=T)
	z = (0-mz) / sd(rand.indep.comparison,na.rm=T)
	
	summaryString = paste(permName,
		sum(rand.indep.comparison >= 0,na.rm=T),
		mz,
		sum(!is.na(rand.indep.comparison)),
		z,
		"\n",
		sep = "\t"
		)
	cat(summaryString,file=resultsFile,append=T)
	
	permName2 = strsplit(permName,"/")[[1]]
	permName2 = permName2[length(permName2)]
	
	write.csv(data.frame(SumEntropy=t(t(rand.indep.comparison))), file=paste("../Results/SimplifiedPhonology/PermutationResults/RandomIndependentSamples/",permName2,'.csv',sep=''),row.names=F)
	
}


runComparison.RandomIndependentSample.RandomConcepts = function(wordList1,wordList2,strat1,strat2,permName,firstSegment=F){
	
	
	sapply(1:number.of.random.samples, function(X){
		# select random concepts
		cx = sample(1:nrow(wordList1),9)
		wordList1X = wordList1[cx,]
		wordList2X = wordList2[cx,]
		# Run random sample
		runComparison.randomSample(wordList1X, wordList2X, strat1, strat2, paste(permName,"_",X,sep=''), firstSegment)
	})
	return(NA)
}


runComparison.RandomIndependentSample.RandomConcepts_Domain = function(wordList1,wordList2,strat1,strat2,permName,firstSegment=F){

  concepts.per.domain = tapply(alldata$meaning.id.fixed,alldata$domain,function(X){length(unique(X))})
  domains.with.enough.concepts = names(concepts.per.domain)[concepts.per.domain>=9]
  
  # for each domain
  for(i in 1:length(domains.with.enough.concepts)){
  	
  	dom = domains.with.enough.concepts[i]
    domainString = paste("^",dom,"\\.",sep='')
    choose.from.rows1 = which(grepl(domainString,as.character(rownames(wordList1))),arr.ind=T)
    choose.from.rows2 = which(grepl(domainString,as.character(rownames(wordList2))),arr.ind=T)
    dx1 = wordList1[choose.from.rows1,]
    dx2 = wordList2[choose.from.rows2,]
    runComparison.RandomIndependentSample.RandomConcepts(dx1,dx2,strat1,strat2,paste(permName,"_",domainString,sep=''), firstSegment)
    
    
  	}	
	
}

pickWordsInLanguage = function(whWordList,wordList,actualEntropy,firstSegment){
  randomSample = getWordListEntropy(apply(wordList,2,function(X){
    sample(X[!is.na(X)],nrow(whWordList))
  }), firstSegment = firstSegment)
  
  return(actualEntropy - randomSample)
}


compareWordsWithinLanguage = function(whWordList, wordList,permName,firstSegment=F){
  
    choices = apply(wordList,2,function(X){sum(!is.na(X))})>5
    wordList =wordList[,choices]
    whWordList = whWordList[,choices]
    actualEntropy = getWordListEntropy(whWordList)
    
    ret = replicate(number.of.random.samples, pickWordsInLanguage(whWordList,wordList,actualEntropy,firstSegment))
    
    res = apply(ret,1,function(X){sum(X<0,na.rm=T)/sum(!is.na(X))})
    
    write.csv(ret,file=permName)
    
    #hist(rowMeans(ret))
    #hist(res)
    #sum(res>0.95)
    
}


compareWordsWithinLanguage_Domain = function(whWordList, wordList,permName,firstSegment=F){
  
  for(domain in 1:20){
    domainString = paste("^",domain,"\\.",sep='')
    
    choose.from.rows = which(grepl(domainString,as.character(rownames(wordList))),arr.ind=T)
    if(sum(choose.from.rows)>=9)
    dx = wordList[choose.from.rows,]
    pn = paste(permName,"_",domain,".csv",sep='')
    compareWordsWithinLanguage(whWordList, wordList, pn,firstSegment)
  }
  
  
}