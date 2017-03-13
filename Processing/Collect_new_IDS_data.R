setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Processing/")


files = list.files("../RAW_data/IDS_new/ids-dataset.cldf/","*.csv$")

idsLangs = data.frame()

include.langs = c("ids-dictionary-416.csv")

for(file in files){
  dx = read.csv(paste("../RAW_data/IDS_new/ids-dataset.cldf/",file,sep=''), stringsAsFactors = F, fileEncoding = "UTF-8", encoding = 'UTF-8')
  
  if("AlternateTranscription" %in% names(dx)){
    print ("!!!!!!!")
  }
  
  useLang= F
  
  tr1 = dx$Transcription[!is.na(dx$Transcription)][1]
  
  if((!is.na(tr1) & 
     tr1 %in% c("Phonemic","phonetic",'IPA')) |
     file %in% include.langs
     ){
    useLang = T
  }else {
    if("AlternativeTranscription" %in% names(dx)){
      dx = dx[dx$AlternativeTranscription == "Phonemic",]
      dx$Value = dx$AlternativeValue
      useLang = T
    }
  }

  
  
  if(useLang){
    
    dx$meaning.ids = sapply(dx$ID, function(X){
      paste(strsplit(X,"-")[[1]][1:2], collapse='.')
    })

    dx = dx[!is.na(dx$meaning.ids),]
    dx = dx[!is.na(dx$Value),]
    
    # Check more than 200 meanings
    if(nrow(dx)>200){
      dx = data.frame(language_pk=dx$meaning.ids, 
                 meaning=dx$Concept, 
                 word=dx$Value,
                 Source="IDS",
                 Language=dx$Language_name,
                 iso=NA,
                 glotto=dx$Language_ID)
      idsLangs = rbind(idsLangs,dx)
    } 
  } 
    
}

idsLangs$glotto = as.character(idsLangs$glotto)
idsLangs[idsLangs$Language=='Thai (Korat variety)',]$glotto = 'sout2745'
# nung1283 is Nung ninbei


write.csv(idsLangs, 
          file= "../Processing/Matched_word_lists/IDSlist_new.csv",
          quote = T,  row.names = F, fileEncoding = 'UTF-8')
