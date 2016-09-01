try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
source("RestrictionsApplied.R") # also loads PermutationTools.R
source("grammars.R")
source("makeDataVariables.R")

d.wh.glotto = names2glotto[colnames(d.wh.m)]


l.details2 = l.details[l.details$glotto %in% d.wh.glotto,]
l.details2 = l.details2[!duplicated(l.details2$glotto),]
library(maps)

map(interior = F, fill=T, col='gray', )
points(l.details2$longitude,l.details2$latitude,col=rgb(1,0,0),  pch= 16)

library(ggplot2)

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

set.seed(12)
cols = sample(rainbow(length(unique(l.details2$area))))
l.details2$area.colour = cols[as.numeric(as.factor(l.details2$area))]

#Now Layer the cities on top
mp <- mp+ geom_point(aes(y=l.details2$latitude, x=l.details2$longitude) ,color=l.details2$area.colour, size=3) 
mp




mostCommonPhoneme = apply(d.wh.m, 2, function(X){
  sx = unlist(strsplit(X,";"))
  sx = substr(sx,1,1)
  segs = unlist(strsplit(sx,""))
  names(sort(table(segs)))[1]
})

l.details2$mostCommonPhoneme = mostCommonPhoneme[ match(l.details2$glotto, names2glotto[names(mostCommonPhoneme)]) ]

map(interior = F)
text(l.details2$longitude,l.details2$latitude, l.details2$mostCommonPhoneme)
