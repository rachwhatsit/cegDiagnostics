library(partitions)
#figure out what the one step ahead probability distribution should be 

#take all the possible stages for a representation 
rho.test <- 0.8 #baseline parameter between 0 and 1 
#THIS IS CODED FOR local L 1 MOVES ONLY.
epsilon.test <- 1.2 #represents what sort of changes in stage we're willing to admit
which.cut.test <- 3#what varaible are we concerned with
stage.key <- cegb.stage.key
#stg.trans.prob <- function( stage.key, which.cut, rho,epsilon){

df.cut <- df[1:20,] #this will be looped over


##GET THE MARKOV TRANSITION PROBABILITIES

  S <- dim(stage.key[[which.cut]])[1]#how many level combos are there
  possible.colorings <- listParts(S)##removin the one where no one gets a color
  
  num.partitions <- as.vector(unlist(lapply(possible.colorings, function(x){length(x)})))
  number.of.stages <- length(unique(stage.key[[which.cut]]$stage))#UPDATE THIS AFTERWARDS
  idx <- which(num.partitions==number.of.stages)
  possible.colorings[idx]-> same.num.colrs
  for (i in 1:length(idx)){
    stg <- stage.key[[which.cut]]$stage[as.vector(unlist(same.num.colrs[[i]][1]))]#check to see if the groups have their same colors 
    if(all(rep(stg[1],length(stg))== stg)==TRUE){crrnt.stg  <- i}#found correct stage
    else{next}
  }
  
  idx.coarse <- which(num.partitions==length(unique(stage.key[[which.cut]]$stage))-1)#returs stagings with 2 colors 
  first.set <- lapply(possible.colorings[idx.coarse], '[[', 1) 
  hasse.coarse <- c()
  for (i in 1:length(idx.coarse)){
    if (all(as.vector(unlist(possible.colorings[[idx[crrnt.stg]]][1])) %in% as.vector(unlist(first.set[[i]]))) ){hasse.coarse[i] <- TRUE}
    else {hasse.coarse[i] <- FALSE}
  }
  
  #figure out how to pull the finer partitions
  idx.fine <- which(num.partitions==length(unique(stage.key[[which.cut]]$stage))+1)#returs stagings with 2 colors 
  first.set <- lapply(possible.colorings[idx.fine], '[[', 1) 
  hasse.fine <- c()
  for (i in 1:length(idx.fine)){
    if (all(as.vector(unlist(possible.colorings[[idx[crrnt.stg]]][1]))[1] %in% as.vector(unlist(first.set[[i]]))) ){hasse.fine[i] <- TRUE}
    else {hasse.fine[i] <- FALSE}
  }
  
  
  B <- sum(length(which(hasse.coarse==TRUE)), length(which(hasse.fine==TRUE)))# normalizing constant 
  
  p.trans <- rep(0, length(num.partitions))
  p.trans[idx[crrnt.stg]] <- rho
  p.trans[idx.coarse[hasse.coarse==TRUE]] <- (1/B)*(1-rho)
  p.trans[idx.fine[hasse.fine==TRUE]] <- (1/B)*(1-rho)
  
  if(sum(p.trans)!=1) print('panic at the disco!')
  
  current.stage <- idx[crrnt.stg]
  possible.stage <- c(idx.coarse[hasse.coarse==TRUE],idx.fine[hasse.fine==TRUE])
  ##END OF THE COMPUTING OF THE MARKOV STAGE PROBABILITIES
 # return(list(current.stage, possible.stage, p.trans))
#}

#EXAMPLE
#stg.trans.prob(stage.key=cegb.stage.key, rho=rho.test,which.cut=which.cut.test, epsilon=epsilon.test)
# num.colors <- S #total number of colors 
# p.trans

#initalize p(U_t-1 | x^t-1), called p2 vec here. 
p2 <- rep(0, length(possible.stage)+1)
for (i in 1:length(p2)){ 
  if(i==length(p2)) {
    clr <- possible.colorings[[current.stage]];
  } else {
    clr <- possible.colorings[[(possible.stage[i])]];
  }#partition of interest

  n <- prod(apply(df, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
  p.stgng <- rep(0, length(clr))#total number of groups in the partition
  for (j in 1:length(clr)){# loop over number of colors in proposed staging
    #find ref prior for the 
      another.n <- length(levels(as.factor(df[,which.cut])))
      alpha.bar <- rep(n/(dim(stage.key[[which.cut]])[1]), another.n)*length(clr[[j]])/another.n
      
      in.paths<-stage.key[[which.cut]][(clr[[j]]),]#id the incoming pathways
      #stages.of.interest <- merge(in.paths[,1:(target.cut-1)], stage.key[[(target.cut-1)]][,c(1:(target.cut-1),dim(stage.key[[(target.cut-1)]])[2])])$stage
      #ref.prior.idx <- unlist(lapply(stages.of.interest, function(x){as.numeric(substr(x,nchar(x),nchar(x)))+1}))#gives the stage number, because of weird indexing, want 
      #in.path.idx <- which(stage.key[[target.cut]]$stage==target.stage)
      df_cuts <- list()
      for (l in 1:length(clr[[j]])){
        df_cuts[[l]] <- df.cut
        for(m in 1:(length(colnames(stage.key[[which.cut]]))-2)){
          df_cuts[[l]] <- filter(df_cuts[[l]], UQ(sym(colnames(df_cut)[l]))==unlist(stage.key[[which.cut]][m,clr[[j]][l]]))#filter according to the matching indices 
        }
      }
      df_paths <- do.call(rbind, df_cuts)
      # df_paths <- filter(df_paths,Economic==cndtnl.stage.val) 
      obsv.stage.count <- count(df_paths,UQ(sym(colnames(df_paths)[which.cut])))#how many counts we observe in each stage
      if(sum(obsv.stage.count$n)==0){
        counts <- rep(0, length(alpha.bar))
      } else {
        counts <- obsv.stage.count$n
      }
      alpha.star <-alpha.bar + counts
      p.stgng[j] <- lgamma(sum(alpha.bar))-lgamma(sum(alpha.star)) + sum(lgamma(alpha.star) - lgamma(alpha.bar))
    
  }
  p2[i] <- sum(p.stgng)
}


p2.exp <- exp(p2) /sum(exp(p2))
  
clr <- possible.colorings[[current.stage]]
n <- prod(apply(df, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
p.stgng <- rep(0, length(clr))#total number of groups in the partition
for (j in 1:length(clr)){# loop over number of colors in proposed staging
  #find ref prior for the 
  another.n <- length(levels(as.factor(df[,which.cut])))
  alpha.bar <- rep(n/(dim(stage.key[[which.cut]])[1]), another.n)*length(clr[[j]])/another.n
  
  in.paths<-stage.key[[which.cut]][(clr[[j]]),]#id the incoming pathways
  #stages.of.interest <- merge(in.paths[,1:(target.cut-1)], stage.key[[(target.cut-1)]][,c(1:(target.cut-1),dim(stage.key[[(target.cut-1)]])[2])])$stage
  #ref.prior.idx <- unlist(lapply(stages.of.interest, function(x){as.numeric(substr(x,nchar(x),nchar(x)))+1}))#gives the stage number, because of weird indexing, want 
  #in.path.idx <- which(stage.key[[target.cut]]$stage==target.stage)
  df_cuts <- list()
  for (l in 1:length(clr[[j]])){
    df_cuts[[l]] <- df.cut
    for(m in 1:(length(colnames(stage.key[[which.cut]]))-2)){
      df_cuts[[l]] <- filter(df_cuts[[l]], UQ(sym(colnames(df_cut)[l]))==unlist(stage.key[[which.cut]][m,clr[[j]][l]]))#filter according to the matching indices 
    }
  }
  df_paths <- do.call(rbind, df_cuts)
  # df_paths <- filter(df_paths,Economic==cndtnl.stage.val) 
  obsv.stage.count <- count(df_paths,UQ(sym(colnames(df_paths)[which.cut])))#how many counts we observe in each stage
  if(sum(obsv.stage.count$n)==0){
    counts <- rep(0, length(alpha.bar))
  } else {
    counts <- obsv.stage.count$n
  }
  alpha.star <-alpha.bar + counts
  p.stgng[j] <- lgamma(sum(alpha.bar))-lgamma(sum(alpha.star)) + sum(lgamma(alpha.star) - lgamma(alpha.bar))
  
}
exp(p.stgng)
a <- sum(p.trans[c(possible.stage,current.stage)] * p2.exp) #gives the probability of transitioning to another local stage
b <- prod(exp(p.stgng))
a*b
# 
# 
# for (i in 1:length(stagings[[which.cut]])){
#   colorings[[i]] <-combn(1:S,i)
#   if( any(dim(as.matrix(colorings[[i]]))==1)){
#     num.colorings <- sum(num.colorings,1)}
#   else{
#     num.colorings <- sum(num.colorings, as.matrix(dim(colorings[[i]]))[2])}
#   }#returns all the possible colorings of the stages in the cut 
# 
# #find the probability of transitioning to the other staging
# 
# 
# 
# 
# 
# pi.trans <- rep(NA, length(num.colorings)) #probability of changing to a new key 
# 
# S <- length(stage.key[[which.cut]])#how many things do we have to color
# all.subsets <- function(set) {
#   n <- length(set)
#   bin <- expand.grid(rlply(n, c(F, T)))
#   mlply(bin, function() { set[c(…)] })
# }
