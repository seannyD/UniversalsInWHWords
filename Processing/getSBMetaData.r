setwd("/Users/pplsuser/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/Spraakbanken/")

fileName = "/Users/pplsuser/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/Spraakbanken/ids-hin.csv"

getSBMetaData <- function(fileName){
	# read in the file, including all rows
	dx = read.delim(fileName,sep=':',header=F,fill=T, stringsAsFactors=F, quote='', fileEncoding='UTF-8')
	#get the iso code, removing spaces
	isoCode = gsub(" ","",dx[grepl("ISO 639",dx$V1),]$V2)[1]
	# remove start and end space from name
	name = gsub(" $","",dx[dx$V1=="Name",]$V2)[1]
	name = gsub("^ ", "", name)
	# return data
	
	return(gsub("\t","",c(isoCode,name)))
}

for(i in list.files('.','.csv')){
	print(i)
	print(getSBMetaData(i))
}