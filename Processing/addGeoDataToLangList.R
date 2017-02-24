try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Processing"))

l.details = read.csv("../Analysis/LangsInAnalysis.csv", fileEncoding = 'utf-8', encoding = 'utf-8', stringsAsFactors = F)

l.details$area = NA
load("~/Documents/MPI/Neandertals_Collab/fossilSept/pca.per.family/autotyp2015-06-09T09-00.rData")

areas = tapply(as.character(autotyp.geography$area),as.character(autotyp.geography$glottolog_LID.2014),head,n=1)

#l.details$area[nchar(l.details$area)<2] = NA
l.details$area = areas[l.details$glotto]

l.details[l.details$glotto=="sout2746",]$area = 'Southeast Asia'
l.details[l.details$glotto=="enap1235",]$area = 'NE South America'
l.details[l.details$glotto=="sion1247",]$area = 'Andean'


g = read.csv("~/Documents/MPI/Glottolog/glottolog-languoid.csv/languoid.csv",stringsAsFactors=F)
g$fam = g[match(g$family_pk,g$pk),]$name
g$fam[is.na(g$family_pk)] = g$name[is.na(g$family_pk)]
rownames(g) = g$id

l.details$langFam = g[match(l.details$glotto, g$id),]$fam

# Tzotsil is "bookkeeping", real glotto should be tzot1259
l.details[l.details$glotto=='tzot1264',]$langFam = "Mayan"

l.details$latitude = g[l.details$glotto,]$latitude
l.details$longitude = g[l.details$glotto,]$longitude

# wrong lat/long
l.details[l.details$glotto=='rotu1241',c("latitude",'longitude')] = c(-12.5008,177.066)
l.details[l.details$glotto=='hawa1245',]$latitude = 19.6297
l.details[l.details$glotto=='hawa1245',]$longitude = -155.43

#l.details[!is.na(l.details$glotto) & l.details$glotto=='nucl1241',c("longitude","latitude")] = c(115.33,38.87)


geo.fix =matrix(c(
  # glotto   #lat  #long
  'nucl1241',38.87, 115.33,
  "olda1245",42.14,37.11,
  "limo1249",18.13, -77.26,
  "nuuc1236",47.4623893,-118.3132951,
  "gela1265",22.56, 104.70,
  "roma1329",49.6558485,18.0328078,
  "anga1295",-22.11, -58.91,
  "kami1255",25.89, 109.22,
  "tokh1242",40.333402, 87.248417,
  "tokh1243",39.5708233,71.3739565,
  "oldh1241",51.0851933,5.9698196,
  "west2376",48.090278, 17.97,
  'sout1528',21.92, 44.32,
  'east2283', 45, 40,
  'west2348', 45, 40,
  'sout2745', 14.33,102.99,
  'east1436', 27.87,83.39,
  'tzot1264', 16.64,-92.74,
  'inxo1238', 46.00, 42.27,
  'xvar1237', 46.00, 42.27,
  'khoc1238', 46.03, 42.12,
  'tlya1238', 46.03, 42.12
),nrow=3)

geo.fix = geo.fix[,geo.fix[1,] %in% l.details$glotto]
for(i in 1:ncol(geo.fix)){
  l.details[!is.na(l.details$glotto) & l.details$glotto == geo.fix[1,i],]$latitude = geo.fix[2,i]
  l.details[!is.na(l.details$glotto) & l.details$glotto == geo.fix[1,i],]$longitude = geo.fix[3,i]
}

l.details[grepl("Avar ", l.details$Language),c('longitude','latitude')]= c(46.558, 41.7047)
l.details[grepl("Dargwa ", l.details$Language),c('longitude','latitude')]= c(47.4388 , 42.4257)
l.details[grepl("Lezgian ", l.details$Language),c('longitude','latitude')]= c(47.8951,  41.5157)
l.details[grepl("Andi ", l.details$Language),c('longitude','latitude')]= c(46.2919  ,42.8078)
l.details[grepl("Karata ", l.details$Language),c('longitude','latitude')]= c(46.3151 , 42.6501)
l.details[grepl("Tsez ", l.details$Language),c('longitude','latitude')]= c(45.8096,  42.2646)
l.details[grepl("Tabasaran ", l.details$Language),c('longitude','latitude')]= c(47.8379 , 42.0198)
l.details[grepl("Aghul ", l.details$Language),c('longitude','latitude')]= c(47.5843 , 41.9242)
l.details[grepl("Chamalal ", l.details$Language),c('longitude','latitude')]= c(45.995,  42.5024)
l.details[grepl("Bezhta ", l.details$Language),c('longitude','latitude')]= c(45.995,  42.5024)
l.details[grepl("Rutul ", l.details$Language),c('longitude','latitude')]= c(47.3244,  41.6187)

l.details[is.na(l.details$longitude),c("Language",'glotto')]


# Use autotup geo position to find closest area
library(fields)
missing = cbind(as.numeric(l.details[is.na(l.details$area),]$longitude),
      as.numeric(l.details[is.na(l.details$area),]$latitude))
autotyp.loc = cbind(autotyp.geography$longitude,
                    autotyp.geography$latitude)

distx = rdist.earth(missing,autotyp.loc)
closest.match = as.character(autotyp.geography[apply(distx,1,function(X){which(X==min(X,na.rm = T))[1]}),]$area)
l.details[is.na(l.details$area),]$area = closest.match


# Check long differences?
ax = autotyp.geography[autotyp.geography$glottolog_LID.2014 %in% l.details$glotto,c("longitude",'latitude')]
ax.rn = autotyp.geography[autotyp.geography$glottolog_LID.2014 %in% l.details$glotto,c("glottolog_LID.2014")]
lx = l.details[l.details$glotto %in% autotyp.geography$glottolog_LID.2014,c("longitude",'latitude')]
lx.rn = l.details[l.details$glotto %in% autotyp.geography$glottolog_LID.2014,c('glotto')]
lx[,1] = as.numeric(lx[,1])
lx[,2] = as.numeric(lx[,2])
distx2 = rdist.earth(ax[match(lx.rn,ax.rn),],lx)
distx3 = diag(distx2)
lx.rn[which(distx3>1000)]


wals = read.csv("../Analysis/SubjectVerbOrder/wals-language.csv/language.csv", stringsAsFactors = F, fileEncoding = 'utf-8')
wals$glottocode[!is.na(wals$glottocode) & wals$glottocode==''] = NA
l.details$WALS.qpos = wals[match(l.details$glotto, wals$glottocode),]$X93A.Position.of.Interrogative.Phrases.in.Content.Questions

l.details$WALS.qpos[l.details$WALS.qpos==''] = NA

c.grammars = read.csv("../RAW_data/Grammars.csv", stringsAsFactors = F)
c.grammars[c.grammars$Positioning=='',]$Positioning = NA
c.grammars[c.grammars$Possible.Positioning=='',]$Possible.Positioning = NA
c.grammars[is.na(c.grammars$Possible.Positioning),]$Possible.Positioning = c.grammars[is.na(c.grammars$Possible.Positioning),]$Positioning

c.grammars[c.grammars$Glotto=='tzot1259',]$Glotto = 'tzot1264'
c.grammars[c.grammars$Glotto=='gheg1238',]$Glotto = 'tosk1239'
c.grammars[c.grammars$Glotto=='croa1245',]$Glotto = 'sout1528'

c.grammars[!c.grammars$Glotto %in% l.details$glotto,c("X",'IDS',"Glotto")]



l.details$S.qpos = c.grammars[match(l.details$glotto, c.grammars$Glotto),]$Possible.Positioning

l.details$qpos = l.details$WALS.qpos
l.details[is.na(l.details$qpos),]$qpos = l.details[is.na(l.details$qpos),]$S.qpos


l.details = l.details[order(l.details$area,l.details$langFam, l.details$Language),]

write.csv(l.details, file="../Analysis/LangsInAnalysis_withGeoData.csv", fileEncoding = 'utf-8', row.names = F)
