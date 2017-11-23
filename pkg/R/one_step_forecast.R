library(partitions)
#figure out what the one step ahead probability distribution should be 

#take all the possible stages for a representation 
rho <- 0.8 #baseline parameter between 0 and 1 
#THIS IS CODED FOR L 1 MOVES ONLY.
epsilon <- 1.2 #represents what sort of changes in stage we're willing to admit
which.cut <- 3#what varaible are we concerned with
stage.key <- cegb.stage.key
#stg.trans.prob <- function( stage.key, which.cut, rho,epsilon){

df.cut <- df[1:20,]

  
  S <- dim(stage.key[[which.cut]])[1]#how many level combos are there
  possible.colorings <- listParts(S)##removin the one where no one gets a color
  
  num.partitions <- as.vector(unlist(lapply(possible.colorings, function(x){length(x)})))
  
  idx <- which(num.partitions==length(unique(stage.key[[which.cut]]$stage)))
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
#  return(list(current.stage, possible.stage, p.trans))


#EXAMPLE
stg.trans.prob(stage.key, rho,which.cut, epsilon)
# num.colors <- S #total number of colors 
# p.trans

#initalize p(U_t-1 | x^t-1), called p2 vec here. 
p2 <- rep(0, length(possible.stage))
for (i in 1:length(p2)){ 
  clr <- possible.colorings[[(possible.stage[i])]]#stage of interest
  n <- prod(apply(df, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
  
  p.stage 
  for (j in 1:length(clr)){# loop over number of colors in proposed staging
    #find ref prior for the 
    for (k in 1:length(clr[[j]])){#number of stages in the proposed coloring
      another.n <- length(levels(as.factor(df[,which.cut])))
      alpha.bar <- rep(n/(dim(stage.key[[which.cut]])[1]), another.n)*length(clr[[j]])/another.n
      
    }
  }
  
  #for each stage in the new coloring
  p2[i] <- lgamma(sum(alpha.bar))-lgamma(sum(alpha.star)) + sum(lgamma(alpha.star))-sum(lgamma(alpha.bar))
  }


post.trans <- rep(0,length(possible.stage)+1)#compute the non-zero probs for each possible staging
for (i in 1:length(post.trans)){
  color <- possible.colorings[[idx[i]]]
  for (j in 1:length(color)){#get each of the possible stagings
    #determine what the new prior is for the new staging 
    #determine the number of counts for the new staging 
    
    stg.clr <- as.character(j)
    stage.key[[]]
  }
  
}

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
