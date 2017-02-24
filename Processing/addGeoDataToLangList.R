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

l.details$latitude = g[l.details$glotto,]$latitude
l.details$longitude = g[l.details$glotto,]$longitude

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
  'sout2745', 102.99,14.33,
  'east1436', 83.39,27.87
),nrow=3)

geo.fix = geo.fix[,geo.fix[1,] %in% l.details$glotto]
for(i in 1:ncol(geo.fix)){
  l.details[!is.na(l.details$glotto) & l.details$glotto == geo.fix[1,i],]$latitude = geo.fix[2,i]
  l.details[!is.na(l.details$glotto) & l.details$glotto == geo.fix[1,i],]$longitude = geo.fix[3,i]
}


# Use autotup geo position to find closest area
library(fields)
missing = cbind(as.numeric(l.details[is.na(l.details$area),]$longitude),
      as.numeric(l.details[is.na(l.details$area),]$latitude))
autotyp.loc = cbind(autotyp.geography$longitude,
                    autotyp.geography$latitude)

distx = rdist.earth(missing,autotyp.loc)
closest.match = as.character(autotyp.geography[apply(distx,1,function(X){which(X==min(X,na.rm = T))[1]}),]$area)
l.details[is.na(l.details$area),]$area = closest.match



wals = read.csv("../Analysis/SubjectVerbOrder/wals-language.csv/language.csv", stringsAsFactors = F, fileEncoding = 'utf-8')
wals$glottocode[!is.na(wals$glottocode) & wals$glottocode==''] = NA
l.details$WALS.qpos = wals[match(l.details$glotto, wals$glottocode),]$X93A.Position.of.Interrogative.Phrases.in.Content.Questions

l.details$WALS.qpos[l.details$WALS.qpos==''] = NA

c.grammars = read.csv("../RAW_data/Grammars.csv", stringsAsFactors = F)
c.grammars[c.grammars$Positioning=='',]$Positioning = NA
c.grammars[c.grammars$Possible.Positioning=='',]$Possible.Positioning = NA
c.grammars[is.na(c.grammars$Possible.Positioning),]$Possible.Positioning = c.grammars[is.na(c.grammars$Possible.Positioning),]$Positioning

c.grammars[!c.grammars$Glotto %in% l.details$glotto,c("X",'IDS',"Glotto")]



l.details$S.qpos = c.grammars[match(l.details$glotto, c.grammars$Glotto),]$Possible.Positioning

l.details$qpos = l.details$WALS.qpos
l.details[is.na(l.details$qpos),]$qpos = l.details[is.na(l.details$qpos),]$S.qpos


l.details = l.details[order(l.details$area,l.details$langFam, l.details$Language),]

write.csv(l.details, file="../Analysis/LangsInAnalysis_withGeoData.csv", fileEncoding = 'utf-8', row.names = F)
