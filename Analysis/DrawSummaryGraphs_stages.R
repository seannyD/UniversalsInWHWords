library(RColorBrewer)

setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis")

xlimx = c(0.4,0.82)
ylimx =c(0,1200)

colxA = brewer.pal(6,'Set2')
#set.seed(10)
#colxA = sample(colxA)
colx = paste(colxA,"82",sep='')

breaks = seq(0.4,1,by=0.001)

files= c("AllLangs_firstSegments.csv","AllLangs_firstSegment_byFamily.csv","AllLangs_firstSegment_byArea.csv","AllLangs_firstSegment_byAreaAndFamily.csv","RandomConcepts/Comparison_WH_Random_firstSegments.csv")
nlangs = 1

trueEntropy = read.csv("../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments.csv")[1,]$SumEntropy

actionsR = read.csv("../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_ActionDomain.csv")
actions = actionsR[1,]$SumEntropy

bodyR = read.csv("../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_BodyDomain.csv")
body = bodyR[1,]$SumEntropy

pronounR = read.csv("../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_Pronouns.csv")
pronoun = pronounR[1,]$SumEntropy


# Load random within domain results
random.within.domain = data.frame()
for(i in 1:20){
	dx = read.csv(paste("../Results/SimplifiedPhonology/PermutationResults/RandomConcepts/Comparison_WH_Domain_firstSegments_concept_",i,".csv",sep=''))
	random.within.domain = rbind(random.within.domain,dx[-1,])
}
random.within.domain.hist = hist(random.within.domain[random.within.domain$Type=='Perm',]$SumEntropy,plot=F,breaks=breaks)
random.within.domain.hist$counts = random.within.domain.hist$counts/10





makeSummaryGraph = function(filename,files=c(),colx,colxA,trueE=F,bodyE=F,actionsE=F,random.within.domainE=F, pronounE=F, write.to.file=T,make.legend=noLegend){
	if(write.to.file){
	pdf(file=filename, width=8.81, height= 6.14)
	}
	par(yaxs='i',xpd=TRUE,mar=c(5, 4, 8, 2) + 0.1)
	
	# plot other things
	for(f in 1:length(files)){
		rx = read.csv(paste("../Results/SimplifiedPhonology/PermutationResults/",files[f],sep=''))
		to.plot =rx[rx$Type=='Perm',]$SumEntropy
		to.add = f>1
		hist(to.plot,col=colxA[f],border=colxA[f],
		     xlim=xlimx,main='',
		     breaks=breaks,
		     xlab='',ylab='',
		     ylim=ylimx,
		     add=to.add)
		
	}
	
	if(random.within.domainE){
		plot(random.within.domain.hist,add=T,col=colx[f+1],border=NA,xlim=xlimx,main='',xlab='',ylab='',ylim=c(0,10000))
	}
	
	

	
	if(trueE){
		lines(c(trueEntropy,trueEntropy),c(0,ylimx[2]-100),col=2,lwd=2)
		}

	
	if(actionsE){
		lines(c(actions,actions),c(0,ylimx[2]-100),col=3,lwd=2)
	}
	if(bodyE){
	lines(c(body,body),c(0,ylimx[2]-100),col=4,lwd=2)
	}
	
	if(pronounE){
	  lines(c(pronoun,pronoun),c(0,ylimx[2]-100),col=5,lwd=2)
	}
	
	box(bty='l')
	
	title(xlab='Mean Entropy',ylab='Count')
	
	make.legend()
	if(write.to.file){
		dev.off()
	}
}

noLegend=function(){}

makeLegend = function(){

	legend(0.38,1600,legend=c("Mean Wh-word Entropy","Permuted",'  within families',"  within areas","  within families and areas")
	#,'Mean entropy of',"  Random concepts",'  Random concepts from the same domain', "  Action term concetps", "  Body term concepts")
	,col = c(2,colxA[1:4])#,NA,colxA[5:6],3,4)
	,pch=c(NA,rep(17,4))#,NA,17,17,NA,NA)
	,lty=NA,lwd=2,ncol=1,cex=1)
	
		adj = 200
	#lines(c(0.609,0.609),c(adj+1130,adj+1180),lwd=2,col=3)
	#lines(c(0.609,0.609),c(adj+1060,adj+1110),lwd=2,col=4)
	lines(c(0.391,0.391),c(adj+1360,adj+1310),lwd=2,col=2)
}

makeLegend2 = function(){
	xadj = 0.02
	legend(0.38+xadj,1700,legend=c("Mean Wh-word Entropy"#,"Permuted",'  within families',"  within areas","  within families and areas")
	,'Mean entropy of:',"  Random concepts",'  Random concepts from the same domain', "  Action term concetps", "  Body term concepts","  Pronoun concepts")
	,col = c(2,NA,colxA[4:5],NA,NA,NA)#,NA,colxA[5:6],3,4)
	,pch=c(NA,NA,rep(17,2),NA,NA,NA)#,NA,17,17,NA,NA)
	,lty=NA,lwd=2,ncol=1,cex=1)
	
		adj = 290
	lines(c(0.391,0.391)+xadj,c(adj+1060,adj+1110),lwd=2,col=3)
	lines(c(0.391,0.391)+xadj,c(adj+990,adj+1040),lwd=2,col=4)
	lines(c(0.391,0.391)+xadj,c(adj+920,adj+970),lwd=2,col=5)
	
	# wh
	lines(c(0.391,0.391)+xadj,c(adj+1360,adj+1310),lwd=2,col=2)
}



# make graphs with increasing number of elements

files= c("AllLangs_firstSegments.csv","AllLangs_firstSegment_byFamily.csv","AllLangs_firstSegment_byArea.csv","AllLangs_firstSegment_byAreaAndFamily.csv")

for(i in 1:length(files)){
	
	toMake = files[1:i]
	
	fname = strsplit(files[i],'/')[[1]]
	fname = fname[length(fname)]
	fname = paste('Graphs/Summary_',i,"_",gsub('.csv','.pdf',fname),sep='')
makeSummaryGraph(fname,toMake,colx,colxA,trueE=T,make.legend=makeLegend)
	
}



# comparison between wh word entropy and random concept entropy
makeSummaryGraph("Graphs/Comparison_WH_Random_firstSegments.pdf","RandomConcepts/Comparison_WH_Random_firstSegments.csv",colx=colx[4:5],colxA=colxA[4:5],trueE=T, make.legend=makeLegend2)

makeSummaryGraph("Graphs/Comparison_WH_Random_firstSegments_Domain.pdf","RandomConcepts/Comparison_WH_Random_firstSegments.csv",colx=colx[4:5],colxA=colxA[4:5],trueE=T,random.within.domainE=T, make.legend=makeLegend2)

makeSummaryGraph("Graphs/Comparison_WH_Random_firstSegments_Domain_BodyAction.pdf","RandomConcepts/Comparison_WH_Random_firstSegments.csv",colx=colx[4:5],colxA=colxA[4:5],trueE=T,random.within.domainE=T,body=T,action=T,pronoun=T, make.legend=makeLegend2)


# random domain permutation
#makeSummaryGraph("Graphs/MainSummaryGraph_Random_Domain.pdf",files,trueE=T,random.within.domainE=T)

#makeSummaryGraph("Graphs/MainSummaryGraph_withAction_Body.pdf",files,trueE=T,bodyE=T,actionsE=T,random.within.domainE=T)



#######


makeBodyActionPlots = function(files){
  labs=c("Interrogative words","Pronouns","Face", "Action")
  
  xlimB=c(0.4,0.9)
  ylimB = c(0,900)
  for(i in 1:length(files)){
    testR = read.csv(files[i])
    test = testR[1,]$SumEntropy
    
    hist(testR[testR$Type=='Perm',]$SumEntropy,col=colx[i],border=NA,xlim=xlimB,main='',breaks=breaks,xlab='',ylab='',ylim=ylimB,add=i>1, xaxt='n')
    
    yx = (ylimx[2] - 50)/(i+1)
    if(i==4){
      yx = yx - 25
    }
    
    lines(c(test,test),c(0,yx + 40),col=colxA[i],lwd=2)
    text(test,yx,labs[i], col = colxA[i],pos=4)
    
    lines(c(test,mean(testR[testR$Type=='Perm',]$SumEntropy)),c(yx,yx)-30, col=colxA[i], lwd=2)
  
  }
  axis(1,pos=1)
  title(xlab='Mean Entropy',ylab='Count')
}


pdf("Graphs/Comparison_WH_Pronouns_Body_Action.pdf")
makeBodyActionPlots(c("../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments.csv",
 "../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_Pronouns.csv",
 "../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_BodyDomain.csv",
 "../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_ActionDomain.csv"))
dev.off()


pdf("Graphs/Comparison_WH_Pronouns_Body_Action_byAreaAndFamily.pdf")
makeBodyActionPlots(c("../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegment_byAreaAndFamily.csv",
 "../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_PronounDomain_byFamilyAndArea.csv",
 "../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_BodyDomain_byFamilyAndArea.csv",
 "../Results/SimplifiedPhonology/PermutationResults/AllLangs_firstSegments_BasicActionsDomain_byFamilyAndArea.csv"
 ))
 dev.off()