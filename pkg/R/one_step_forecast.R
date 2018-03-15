#tau is the rate of forgetting
#k is some param between 0 and 1
one.step.forecast <- function(rho, epsilon, df_cut,which.cut,stage.key, n.monitor, crrnt.stg,k=0.8,tau=5){
possible.colorings <- listParts(dim(stage.key[[which.cut]])[1])##removin the one where no one gets a color
num.partitions <- as.vector(unlist(lapply(possible.colorings, function(x){length(x)})))
p.monitor <- rep(0, n.monitor-4)
change.points <- c()
for (t in 5:n.monitor){
  df.cut <- df_cut[1:t,]#the counts to be considered 
  #determine the possible coarser partitions
  idx.coarse <- which(num.partitions==length(possible.colorings[[crrnt.stg]])-1)#returs stagings with 2 colors 
  if (length(idx.coarse)==0){
    hasse.coarse <- c()
  } else{
    first.set <- lapply(possible.colorings[idx.coarse], '[[', 1) 
    hasse.coarse <- c()
    for (i in 1:length(idx.coarse)){
      if (all(as.vector(unlist(possible.colorings[[crrnt.stg]][1])) %in% as.vector(unlist(first.set[[i]]))) ){hasse.coarse[i] <- TRUE}
      else {hasse.coarse[i] <- FALSE}
    }
  }
  #determine the possible finer partitons
  idx.fine <- which(num.partitions==length(possible.colorings[[crrnt.stg]])+1)#returs stagings with 2 colors 
  if (length(idx.fine)==0) {hasse.fine <- c()
    } else {
                first.set <- lapply(possible.colorings[idx.fine], '[[', 1) 
                hasse.fine <- c()
                        for (i in 1:length(idx.fine)){
                if (all( as.vector(unlist(first.set[[i]])) %in% as.vector(unlist(possible.colorings[[crrnt.stg]][1]))    ) ){
                  hasse.fine[i] <- TRUE
                } else {
                  hasse.fine[i] <- FALSE
                  }
              } 
            }
  
  B <- sum(length(which(hasse.coarse==TRUE)), length(which(hasse.fine==TRUE)))# normalizing constant 
  p.trans <- rep(0, length(num.partitions))
  p.trans[idx.coarse[hasse.coarse==TRUE]] <- (1/B)*(1-rho)
  p.trans[idx.fine[hasse.fine==TRUE]] <- (1/B)*(1-rho)
  p.trans[crrnt.stg] <- rho
  #print(p.trans)
  current.stage <- crrnt.stg#idx[crrnt.stg]
  possible.stage <- c(idx.coarse[hasse.coarse==TRUE],idx.fine[hasse.fine==TRUE])
  crrnt.stg <- ifelse(p.trans[crrnt.stg]==p.trans[which(p.trans==max(p.trans))[1]], crrnt.stg, which(p.trans==max(p.trans))[1])
  #print(crrnt.stg)
  if(current.stage != crrnt.stg) {change.points <- c(change.points, t)}
   
p2 <- rep(0, length(possible.stage)+1)
  #staging distributions
  for (i in 1:length(p2)){ 
    if(i==length(p2)) {
      clr <- possible.colorings[[current.stage]];
    } else {
      clr <- possible.colorings[[(possible.stage[i])]];
    }#partition of interest
  
    n <- prod(apply(df_cut, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
    p.stgng <- rep(0, length(clr))#total number of groups in the partition
    for (j in 1:length(clr)){# loop over number of colors in proposed staging
      #find ref prior for the 
        another.n <- length(levels(as.factor(df_cut[,which.cut])))
        alpha.bar <- rep(n/(dim(stage.key[[which.cut]])[1]), another.n)*length(clr[[j]])/another.n
        
        in.paths<-stage.key[[which.cut]][(clr[[j]]),]#id the incoming pathways
        #in.path.idx <- which(stage.key[[target.cut]]$stage==target.stage)
        df_cuts <- list()
        for (l in 1:length(clr[[j]])){
          df_cuts[[l]] <- df.cut
          for(m in 1:(length(colnames(stage.key[[which.cut]]))-2)){
            df_cuts[[l]] <- filter(df_cuts[[l]], UQ(sym(colnames(df_cut)[m]))==unlist(stage.key[[which.cut]][clr[[j]][l],m]))#filter according to the matching indices 
          }
        }
        df_paths <- do.call(rbind, df_cuts)
        # df_paths <- filter(df_paths,Economic==cndtnl.stage.val) 
        obsv.stage.count <- count(df_paths,UQ(sym(colnames(df_paths)[which.cut])))#how many counts we observe in each stage
    
        if(sum(obsv.stage.count$n)==0){
          counts <- rep(0, length(alpha.bar))
        } else if ((dim(obsv.stage.count[,1])[[1]]) !=another.n){ 
          counts<-rep(0,length(alpha.bar))
          counts[1:(dim(obsv.stage.count[,1])[[1]])] <- obsv.stage.count$n#THERE HAS GOT TO BE A BETTER WAY
          counts <- unlist(counts)
        } else {
          counts <- obsv.stage.count$n
        }
        print(counts)
        #alpha.star <-alpha.bar + counts
        alpha.star <- (k^(t-1))*(alpha.bar-1) + (k^(t-tau))*counts + 1
        p.stgng[j] <- lgamma(sum(alpha.bar))-lgamma(sum(alpha.star)) + sum(lgamma(alpha.star) - lgamma(alpha.bar))
      
    }
    p2[i] <- sum(p.stgng)
    print(p2)
  }
  #p2.exp <- exp(p2) /sum(exp(p2))
  p2.exp <- exp(p2)
  
  
  clr <- possible.colorings[[current.stage]]
  n <- prod(apply(df_cut, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
  p.stgng <- rep(0, length(clr))#total number of groups in the partition
  #sampling and the parameter distribution 
  for (j in 1:length(clr)){# loop over number of colors in proposed staging
    #find ref prior for the
    another.n <- length(levels(as.factor(df[,which.cut])))
    alpha.bar <- rep(n/(dim(stage.key[[which.cut]])[1]), another.n)*length(clr[[j]])/another.n
    
    #in.paths<-stage.key[[which.cut]][(clr[[j]]),]#id the incoming pathways
    #stages.of.interest <- merge(in.paths[,1:(target.cut-1)], stage.key[[(target.cut-1)]][,c(1:(target.cut-1),dim(stage.key[[(target.cut-1)]])[2])])$stage
    #ref.prior.idx <- unlist(lapply(stages.of.interest, function(x){as.numeric(substr(x,nchar(x),nchar(x)))+1}))#gives the stage number, because of weird indexing, want 
    #in.path.idx <- which(stage.key[[target.cut]]$stage==target.stage)
    df_cuts <- list()
    for (l in 1:length(clr[[j]])){
      df_cuts[[l]] <- df.cut
      for(m in 1:(length(colnames(stage.key[[which.cut]]))-2)){
        df_cuts[[l]] <- filter(df_cuts[[l]], UQ(sym(colnames(df_cut)[m]))==unlist(stage.key[[which.cut]][clr[[j]][l],m]))#filter according to the matching indices 
      }
}
    df_paths <- do.call(rbind, df_cuts)
    # df_paths <- filter(df_paths,Economic==cndtnl.stage.val) 
    obsv.stage.count <- count(df_paths,UQ(sym(colnames(df_paths)[which.cut])))#how many counts we observe in each stage
    if(sum(obsv.stage.count$n)==0){
      counts <- rep(0, length(alpha.bar))
    } else if ((dim(obsv.stage.count[,1])[[1]]) !=another.n){ 
      counts<-rep(0,length(alpha.bar))
      counts[1:(dim(obsv.stage.count[,1])[[1]])] <- obsv.stage.count$n#THERE HAS GOT TO BE A BETTER WAY
      counts <- unlist(counts)
    } else {
      counts <- obsv.stage.count$n
    }
    print(counts)
    #alpha.star <-alpha.bar + counts
    
    alpha.star <- (k^(t-1))*(alpha.bar-1) + (k^(t-tau))*(counts + 1)
    p.stgng[j] <- lgamma(sum(alpha.bar))-lgamma(sum(alpha.star)) + sum(lgamma(alpha.star) - sum(lgamma(alpha.bar)))
    
  }
  exp(p.stgng)->p3.exp
  a <- sum(p.trans[c(possible.stage,current.stage)] * p2.exp) #gives the probability of transitioning to another local stage
  b <- exp(sum(p.stgng))
  p.monitor[t] <- a*b
#  }
}
return(p.monitor)
}
