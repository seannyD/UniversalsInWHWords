resultsFile = "tmp.txt"
#Removes old info


number.of.perms = 20000
number.of.random.samples = 100


d.wh.possible.non.initial.m = d.wh.possible.non.initial.m[,!colnames(d.wh.possible.non.initial.m) %in% lx]

families.d.wh.possible.non.initial = families.d.wh.possible.non.initial[!names(families.d.wh.possible.non.initial) %in% names2glotto[lx]]


runComparison.randomSample(d.wh.possible.initial.m,d.wh.possible.non.initial.m,families.d.wh.possible.initial,families.d.wh.possible.non.initial,"InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_firstSegments_TMPTMPTMP",T)


initial.e = getWordListEntropy(d.wh.possible.initial.m, T)
non.initial.e = getWordListEntropy(d.wh.possible.non.initial.m, T)

mean(initial.e)
mean(non.initial.e)

par(mfrow=c(1,2))
hist(initial.e, breaks=20)
hist(non.initial.e, breaks=20)


dx = data.frame(e = c(initial.e,non.initial.e),
                qpos = c(rep('initial',length(initial.e)),rep('non.initial',length(non.initial.e))),
                family = c(families.d.wh.possible.initial,families.d.wh.possible.non.initial),
                area = c(areas.d.wh.possible.initial, areas.d.wh.possible.non.initial)
                )

dx$e.norm = dx$e - mean(dx$e)


library(lme4)

m0 = lmer(e.norm ~ 1 + (1+qpos|family) + (1+qpos|area), data=dx)
m1 = lmer(e.norm ~ 1 + qpos + (1+qpos|family) + (1+qpos|area), data=dx)
anova(m0,m1)


original.langs = c("olda1245","polc1243","haus1257","gaww1239","iraq1241","tari1263","mapu1245","mash1270","wapi1253","waur1244","trin1274","yavi1244","wayu1243","igna1246","swah1253","khas1269","chew1245","viet1252","tuam1242","rapa1244","rotu1241","hawa1245","maor1246","taki1248","indo1316","plat1254","sout2996","colo1256","chac1249","basq1248","waiw1244","enap1235","maqu1238","macu1259","gali1262","cayu1262","bari1297","chib1270","nort2972","epen1239","onaa1245","tehu1242","cofa1242","telu1262","tami1289","moco1246","pila1245","toba1269","nort2938","hmon1333","limo1249","roma1329","tokh1242","tokh1243","gheg1238","nege1244","croa1245","east2295","nucl1235","bret1244","bulg1262","west2369","russ1263","stan1288","maha1287","beng1280","mara1378","panj1256","lowe1385","west2376","dutc1256","oldh1241","sara1340","sese1246","stan1293","roma1327","iton1250","nucl1643","agua1253","karo1304","qawa1238","leng1262","nucl1655","iyow1239","maca1260","niva1238","wich1264","kekc1242","tzot1259","mose1249","movi1243","hupd1244","bezh1248","arch1244","nepa1252","kain1272","zaca1242","mezq1235","paez1247","guri1247","pano1254","yami1256","chac1251","ship1254","yagu1244","puel1244","puin1248","pume1238","imba1240","cent2050","seri1257","nung1282","tibe1272","mana1288","mand1415","arao1248","cavi1250","esee1248","taca1256","lang1316","lakk1238","nucl1241","mula1253","gela1265","shan1277","kami1255","sout2746","suii1243","minz1236","chad1240","maon1241","luuu1242","thai1261","khun1259","nort2740","nung1283","trum1247","nucl1649","sion1247","tuyu1244","oroq1238","ache1246","east2555","siri1273","para1311","waya1270","yaku1245","erzy1239","komi1268","east2328","nene1249","udmu1245","nort2671","esto1258","finn1318","khan1273","mans1258","selk1253","hung1274","kild1236","chip1262","high1278","yaqu1251","noot1238","waor1240","yama1264","yano1262","nina1238","kett1243","yuwa1244","ayor1240","zuni1245")

original.initial = names2glotto[colnames(d.wh.possible.initial.m)] %in% original.langs
original.non.initial = names2glotto[colnames(d.wh.possible.non.initial.m)] %in% original.langs

original.d.wh.possible.initial.m = d.wh.possible.initial.m[,original.initial]
original.d.wh.possible.non.initial.m = d.wh.possible.non.initial.m[,original.non.initial]

original.families.d.wh.possible.initial = families.d.wh.possible.initial[original.initial]
original.families.d.wh.possible.non.initial = families.d.wh.possible.non.initial[original.non.initial]

runComparison.randomSample(
  original.d.wh.possible.initial.m,
  original.d.wh.possible.non.initial.m,
  original.families.d.wh.possible.initial,
  original.families.d.wh.possible.non.initial,
  "InterrogativeOrder/InterrogativeOrder_RandomIndependentSamples_firstSegments_ORIG_TMPTMPTMP",T)


new.initial = !names2glotto[colnames(d.wh.possible.initial.m)] %in% original.langs
new.non.initial = !names2glotto[colnames(d.wh.possible.non.initial.m)] %in% original.langs

new.d.wh.possible.initial.m = d.wh.possible.initial.m[,new.initial]
new.d.wh.possible.non.initial.m = d.wh.possible.non.initial.m[,new.non.initial]

new.families.d.wh.possible.initial = families.d.wh.possible.initial[new.initial]
new.families.d.wh.possible.non.initial = families.d.wh.possible.non.initial[new.non.initial]

e.initial.new = getWordListEntropy(new.d.wh.possible.initial.m,T)
e.non.initial.new = getWordListEntropy(new.d.wh.possible.non.initial.m,T)

#Aymara  Hindi Nepali, (Chechen. Punjabi, Tzova-Tush)

