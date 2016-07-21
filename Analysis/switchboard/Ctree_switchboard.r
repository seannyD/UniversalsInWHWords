library(party)

setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/InitialPhonemeExperiment/SwitchboardStudy/")



#d= read.csv("NewTorreiraLubbersData/datafile_fto_withSynDepth.txt")
#d= read.csv("NewTorreiraLubbersData/fto_utt_withSynDepthPLUS2_andLaughter_andSurprisal.csv",stringsAsFactors=F)

d = read.csv("csv/fto_utt_V6_FOR_AS.csv",stringsAsFactors=F)

whwords = c("who",'what','why','when','where','why')# how, how much, how many
whwords2 = c("who",'what','why','when','where','why','how')# how, how much, how many

d$whq = d$dialActB2.first %in% c("wh_q")

d$prev.Act = as.factor(d$dialActA2.last)

# remove tunrs within first 5 seconds
d$prev.Act[d$time< 5000] = NA

d$firstPhone = sapply(d$turnPhonesB,function(X){
  # split into words
  wd = strsplit(X," ")[[1]]
  # take out words we don't want
  wd = wd[!wd %in% c("um",'er')]
  # take first word (now that the words we don't want are gone)
  firstWord = wd[1]
  # replace syllable delimiter with phoneme delimiter
  firstWord = gsub("_","\\.",firstWord)
  # split into phonemes
  phones = strsplit(wd[1],"\\.")[[1]]
  # return first phoneme
  return(phones[1])
})

d$firstPhone = as.factor(d$firstLetter)

d$prev.statement = d$prev.Act=='statement'

wx = d[!is.na(d$whq) & !is.na(d$prev.Act) & !is.na(d$firstLetter),c("whq",'prev.statement','firstLetter','prev.Act')]

d.ctree = ctree(whq~.,data=wx)#,controls = ctree_control(maxdepth=3))

pdf(file='results/tmp.pdf',width=200,height=100)
plot(d.ctree,inner_panel=node_inner(d.ctree,id=F),terminal_panel=node_barplot)
dev.off()

