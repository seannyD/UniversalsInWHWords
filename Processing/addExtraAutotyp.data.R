try(setwd("U:/Pragmatics/New/Analysis/"))
try(setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/NewAnalysis/Pragmatics_Slonimska/Analysis"))

l.details = read.csv("../RAW_data/Data_clean_up.csv",stringsAsFactors=F)
l.details$continent = l.details$area
l.details$area = NA
load("~/Documents/MPI/Neandertals_Collab/fossilSept/pca.per.family/autotyp2015-06-09T09-00.rData")

areas = tapply(as.character(autotyp.geography$area),as.character(autotyp.geography$glottolog_LID.2014),head,n=1)

#l.details$area[nchar(l.details$area)<2] = NA
l.details$area = areas[l.details$glotto]


area.fixes = matrix(c(
"cent2050","African Savannah",
"maha1287","Indic",
"nucl1241","Southeast Asia",  # Li of Baoding
"plat1254","Southeast Asia",
"oroq1238","N Coast Asia",
"gela1265","Southeast Asia",
"nucl1655","SE South America",
"west2376","Europe",
"gheg1238","Europe",
"olda1245","Greater Mesopotamia",
"sout2996","Andean",
"pano1254","NE South America",
"chew1245","Oceania",
"chad1240","Southeast Asia",
"zaca1242","Mesoamerica",
"iyow1239","SE South America",
"nort2740","Southeast Asia",
"lang1316","Southeast Asia",
"limo1249","Mesoamerica",
"maon1241","Southeast Asia",
"nege1244","Mesoamerica",
"noot1238","Alaska-Oregon",
"polc1243","African Savannah",
"sout2746","Southeast Asia",
"tokh1242","Europe",
"tokh1243","Europe",
"trin1274","NE South America",
"yavi1244","NE South America",
"minz1236","Southeast Asia"
),nrow=2)

for(i in 1:ncol(area.fixes)){
	l.details[!is.na(l.details$glotto) & l.details$glotto == area.fixes[1,i],]$area = area.fixes[2,i]
}

area.fixes2 = c(
  "Pacahuara" = "NE South America",
  "Proto Austronesian" = "Oceania",
  "Proto Polynesian" = "Oceania",
  "Slavonic, Old Church" = "Europe",
  "Konso" = "Greater Abyssinia",
  "Serbian" = "Europe",
  "Middle Chinese" = "Southeast Asia"
)

for(i in 1:length(area.fixes2)){
  l.details[!is.na(l.details$N1) & l.details$N1 == names(area.fixes2)[i],]$area = area.fixes2[i]
}


g = read.csv("~/Documents/MPI/Glottolog/glottolog-languoid.csv/languoid.csv",stringsAsFactors=F)
g$fam = g[match(g$family_pk,g$pk),]$name
g$fam[is.na(g$family_pk)] = g$name[is.na(g$family_pk)]
rownames(g) = g$id
l.details$langFam = g[l.details$glotto,]$fam
l.details$langFam = g[l.details$glotto,]$fam

l.details$latitude = g[l.details$glotto,]$latitude
l.details$longitude = g[l.details$glotto,]$longitude

#l.details[!is.na(l.details$glotto) & l.details$glotto=='nucl1241',c("longitude","latitude")] = c(115.33,38.87)


geo.fix =matrix(c(
# glotto   #lat  #long
'nucl1241',38.87, 115.33,
"olda1245",42.14,37.11,
"limo1249",18.13, -77.26,
"noot1238",47.4623893,-118.3132951,
"gela1265",22.56, 104.70,
"roma1329",49.6558485,18.0328078,
"nucl1655",-22.11, -58.91,
"kami1255",25.89, 109.22,
"tokh1242",40.333402, 87.248417,
"tokh1243",39.5708233,71.3739565,
"oldh1241",51.0851933,5.9698196,
"west2376",48.090278, 17.97
),nrow=3)


for(i in 1:ncol(geo.fix)){
	l.details[!is.na(l.details$glotto) & l.details$glotto == geo.fix[1,i],]$latitude = geo.fix[2,i]
	l.details[!is.na(l.details$glotto) & l.details$glotto == geo.fix[1,i],]$longitude = geo.fix[3,i]
}


write.csv(l.details,file="../RAW_data/Data_clean_up2.csv")


# draw autotyp map
long.mean = tapply(autotyp.geography$longitude,autotyp.geography$area,mean)
lat.mean = tapply(autotyp.geography$latitude,autotyp.geography$area,mean)

lat.mean["Oceania"] = lat.mean["Oceania"]+10
long.mean["Oceania"] = long.mean["Oceania"]+10

long.mean["N Coast Asia"] = long.mean["N Coast Asia"]+45

lat.mean["North"]

cols = sample(rainbow(length(long.mean),alpha=0.3))
names(cols) = names(long.mean)

library(maps)
map()#database='world', projection = 'gall',parameters=c(lat0=0),orientation=c(90,150,0), wrap=T)
for(i in 1:length(cols)){
  if(names(cols)[i]!="Oceania" & names(cols)[i]!="N Coast Asia"){
    dx = autotyp.geography[autotyp.geography$area==names(cols)[i],]
    dx  = dx[!is.na(dx$longitude),]
    cx = chull(dx$longitude, dx$latitude)
    polygon(dx[cx,]$longitude,dx[cx,]$latitude, col=cols[i])
  }
}
#points(autotyp.geography$longitude,autotyp.geography$latitude,col=cols[as.character(autotyp.geography$area)])
text(long.mean,lat.mean,names(long.mean), col='black')

#text(autotyp.geography$longitude,autotyp.geography$latitude,autotyp.geography$language,cex=0.5)


