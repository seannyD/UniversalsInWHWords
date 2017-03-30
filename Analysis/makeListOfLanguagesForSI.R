try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

l.details.out = l.details[l.details$glotto %in% names2glotto[colnames(d.wh.m)],]#c(
#  "N1",
#  "glotto",
#  "iso",
#  "langFam",
#  "area",
#  "possiblegrammars")
#  "Source",
#  ]

l.details.out = l.details.out[!duplicated(l.details.out$glotto),]

l.details.out$Interrogative.Position.Source = ""
l.details.out[!is.na(l.details.out$WALS.qpos),]$Interrogative.Position.Source = "WALS"


l.details.out[!is.na(l.details.out$S.qpos) & is.na(l.details.out$WALS.qpos),]$Interrogative.Position.Source = "S&R"
l.details.out[l.details.out$glotto=="basq1248",]$Interrogative.Position.Source = "WALS"

l.details.out$ref = ""

##### Correct up to here

sel = l.details.out$Interrogative.Position.Source=="S&R"
l.details.out[sel,]$ref = 
  grammar[match(l.details.out[sel,]$glotto, grammar$Glotto),]$ref

l.details.out$ref = gsub(" ","",l.details.out$ref)
l.details.out$ref = gsub("&","",l.details.out$ref)

#table(l.details.out$Interrogative.Position.Source, l.details.out$possiblegrammars)

l.details.out = l.details.out[order(
  l.details.out$langFam,
  l.details.out$Source,
  l.details.out$Interrogative.Position.Source
),]



names(l.details.out)[names(l.details.out)=='qpos'] = "Interrogative Position"
names(l.details.out)[names(l.details.out)=='Interrogative.Position.Source'] = "IP Source"


l.details.out$`Interrogative Position`[l.details.out$`Interrogative Position` == "1 Initial interrogative phrase"] = "Initial"
l.details.out$`Interrogative Position`[l.details.out$`Interrogative Position` == "2 Not initial interrogative phrase"] = "Non-Initial"
l.details.out$`Interrogative Position`[l.details.out$`Interrogative Position` == "3 Mixed"] = "Mixed"


l.details.out = l.details.out[,c(
  "Language","glotto",'wals.code','langFam','area','Source',
  "Interrogative Position","IP Source","ref"
),]
names(l.details.out) = c("Name","Glottocode","WALS code",'Family','Area','Source',"Interrogative","IP Source","Reference")

l.details.out$Name = gsub("\\(.+\\)","",l.details.out$Name)

library(xtable)

x = print(xtable(l.details.out), include.rownames=F)
x = gsub("\\$\\\\backslash\\$","\\\\", x)
x = gsub("\\\\\\{","{",x)
x = gsub("\\\\\\}","}",x)
x = gsub("\\\\_","_",x)
x = gsub("tabular","longtable",x)
x = gsub("\\\\begin\\{table","%\\\\begin\\{table",x)
cat(x,file="../Writeup/SupportingInformation/Grammars/GrammarTable.tex")
