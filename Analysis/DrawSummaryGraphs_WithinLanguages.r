try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
#source("RestrictionsApplied.R") # also loads PermutationTools.R
#source("grammars.R")
#source("makeDataVariables.R")


getRes = function(filename){
  
  histCol = 'gray'
  widthx = 7
  heightx = 5
  
  d = read.csv(filename,stringsAsFactors=F)
  langNames = d[,1]
  data = d[,2:ncol(d)]
  
  graphName = paste("Graphs/WithinLanguage/",tail(strsplit(filename,'/')[[1]],1),'_Diff.pdf',sep='')
  
  xlimx = max(abs(range(rowMeans(data))))
  xlimx = c(-xlimx,xlimx)
              
  pdf(file=graphName, width=widthx, height=heightx)
  hist(rowMeans(data),xlab='Mean Difference in Entropies', border=histCol,col=histCol, breaks=20,main='', xlim=xlimx, ylab='Number of Languages')
  abline(v=0)
  dev.off()

  res = apply(data,1,function(X){sum(X<0,na.rm=T)/sum(!is.na(X))})

  gx = tail(strsplit(filename,'/')[[1]],1)
  graphName = paste("Graphs/WithinLanguage/",gx,'_Prop.pdf',sep='')
    
  pdf(file=graphName, width=widthx, height=heightx)
  hist(res, xlab='Proportion of random samples\nwith a higher entropy', border=histCol,col=histCol,breaks=20,main='',ylab='Number of Languages')
  dev.off()
  
  return(c(gx,sum(res>0.95), sum(rowMeans(data)>0),length(res)))
  
}
  
ret = data.frame(name=NA,prop95=NA,diffGT0=NA,N=NA)  
  
ret = rbind(ret,getRes("../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_FirstSegments.csv"))

ret = rbind(ret,getRes("../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/WithinRes_unanalyzable_FirstSegments.csv"))




ret = ret[!is.na(ret$name),]
ret$prop95 = as.numeric(ret$prop95)
ret$diffGT0 = as.numeric((ret$diffGT0))
ret$N = as.numeric(ret$N)
ret$prop95.prop = ret$prop95/ret$N
ret$diffGT0.prop = ret$diffGT0/ret$N
write.csv(ret,file="../Results/SimplifiedPhonology/PermutationResults/WithinLanguages/Summary.csv")
