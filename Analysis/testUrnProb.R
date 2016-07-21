urnProb = function(choice,pop,px=1){
  # recursive algorithm to work out probability of picking choice out of pop
  # for raw vector of items
  if(length(choice)==1){
    return(px * sum(pop==choice[1])/length(pop))
  } else{
    thisp = sum(pop==choice[1])/length(pop)
    pop = pop[-which(pop==choice[1])[1]]
    choice = choice[2:length(choice)]
    return(urnProb(choice,pop,px * thisp))
  }
}

getP = function(X,k, target){
  return(sum(sort(sample(X,k)) != sort(target))==0)
}

ch2 = function(sub,pop){
  factorial(pop) / (factorial(pop) * factorial(pop-sub))
}

mhd = function(choice,pop){
  # Multivariate hypergeometric distribution
  # (probability of choosing choice out of pop)
  tx = cbind(table(choice), table(pop[pop %in% choice]))
  return(
    prod(
      apply(tx,1,function(X){
        choose(X[2],X[1])
        }
      )
    ) / choose(length(pop),length(choice))
    )
}


real = c()
guesses = c()
upto = 6
for(i in 1:upto){
  t = sample(1:10,sample(2:4,1))
  pool = c(t,sample(1:10,5))
  
  px = replicate(10000,getP(pool,length(t),t))
  #sum(px)/length(px)
  
  #urnProb(t,pool) * factorial(length(t))
  #urnProb(t,pool) * choose(length(t),length(t))
  
  #urnProb(t,pool) * factorial(length(t))/(factorial(length(t)-1))
  #ch2(length(t),length(pool))
  #fx = (sum(px)/length(px))/urnProb(t,pool)
  guesses = c(guesses,mhd(t,pool))
  real = c(real,sum(px)/length(px))
}

plot(real,guesses )
abline(a=0,b=1)
lm(real~guesses )
