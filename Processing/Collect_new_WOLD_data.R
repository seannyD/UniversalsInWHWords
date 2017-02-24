try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/RAW_data/WOLD_new"))


woldFiles = list.files("wold-dataset.cldf/","*.csv$")

wf = data.frame()
for(f in woldFiles){
  
  dx = read.csv(paste("wold-dataset.cldf/",f,sep=''), stringsAsFactors = F, fileEncoding = 'utf-8', encoding = 'utf-8')
  
  dx = data.frame(language_pk=dx$WOLD_Meaning_ID, 
                  meaning=dx$Parameter_name, 
                  word=dx$Value,
                  Source="WOLD",
                  Language=dx$Language_name,
                  iso=NA,
                  glotto=dx$Language_ID,
                  borrowed_score= dx$Borrowed_score,
                  analyzability= dx$Analyzability
                  )
  wf = rbind(wf,dx)
}


wf$language_pk = gsub("-",'.',wf$language_pk)

write.csv(wf, "../../RAW_data/WOLD_new/WOLD_words.csv", fileEncoding = 'utf-8', row.names = F)

# (Should now run addTranscriptions_new.R)