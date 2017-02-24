try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))


# load data and filter unwanted languages
# (creates variable 'alldata')
#source("RestrictionsApplied.R") # also loads PermutationTools.R
#source("grammars.R")
#source("makeDataVariables.R")

#d.wh.glotto = names2glotto[colnames(d.wh.m)]


l.details2 = read.csv("LangsInAnalysis_withGeoData.csv")

#l.details2 = l.details[l.details$glotto %in% d.wh.glotto,]
l.details2 = l.details2[!duplicated(l.details2$glotto),]
library(maps)
library(maptools)

map(interior = F, fill=T, col='gray' )
points(l.details2$longitude,l.details2$latitude,col=rgb(1,0,0),  pch= 16)

library(ggplot2)

set.seed(120)
cols = sample(rainbow(length(unique(l.details2$area)), alpha = 0.75))

l.details2$area.colour = cols[as.numeric(as.factor(l.details2$area))]
l.details2$area.colour[l.details2$area=="Indic"] = 'black'
l.details2$area.colour[l.details2$area=="N Coast Asia"] = 'dark grey'

world <- map_data("world", interior = F)
world <- world[world$region != "Antarctica",]

gg <- ggplot()
gg <- gg + geom_map(data=world, map=world,
                    aes(x=long, y=lat, map_id=region),
                    fill="#7f7f7f", size=0.05, alpha=1/4) +
            xlab('') + ylab("") 
  

finalMap= gg + 
  geom_point(aes(y=l.details2$latitude, x=l.details2$longitude) ,
             color=l.details2$area.colour, size=2) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

pdf("../Writeup/images/mapOfLanguages.pdf", width=8, height=4.5)
finalMap
dev.off()
png("../Writeup/images/mapOfLanguages.png", width=800, height=450)
finalMap
dev.off()



#############
mostCommonPhoneme = apply(d.wh.m, 2, function(X){
  sx = unlist(strsplit(X,";"))
  sx = substr(sx,1,1)
  segs = unlist(strsplit(sx,""))
  names(sort(table(segs)))[1]
})

l.details2$mostCommonPhoneme = mostCommonPhoneme[ match(l.details2$glotto, names2glotto[names(mostCommonPhoneme)]) ]

map(interior = F)
text(l.details2$longitude,l.details2$latitude, l.details2$mostCommonPhoneme)
