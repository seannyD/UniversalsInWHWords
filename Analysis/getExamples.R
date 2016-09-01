

jap = 
  alldata[alldata$meaning.id.fixed %in% whwords & alldata$Language=="Japanese",c("word","meaning",'analyzability')]

write.csv(jap, "../Writeup/examples/Japanese_analyzability.csv")


unit = read.csv("~/Desktop/Stuff/EtymOnline_2013/Word loanword database/clld-wold2-dbe8bcf/data/unit.csv", stringsAsFactors = F)


t(unit[unit$name %in% jap$word,c("name",'jsondata')])


"iku-tsu some-CLASS"

do-ko Q-place
"do-no Q-ATTR\"

"d\\u014d-s-ite how-do-CONV\"

"iku-ra some-PL\"

nan-ji what-hour
nani-semu-ni what-do-ADV