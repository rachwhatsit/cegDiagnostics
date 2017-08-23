rm(list=ls())
library(dplyr);library(ggplot2);library(tidyr)
library(tidyverse)
setwd("/Users/hoban/Documents/diagnostics/diagnostics")
df<-read.csv(file = "CHDS.latentexample1.csv")
radical <- read.csv(file= "data1.csv")

##CHDS EXAMPLE

#HOW TO FUNCTIONIZE?  
##THIS IS FOR CEG-a
cega.stages <- list("cega.w0", "cega.w1", "cega.w2", "cega.w3", "cega.w4", "cega.w5", "cega.w6", "cega.w7", "cega.w8", "cega.w9")

cega.w0 <- df %>% count(Social)
cega.w1 <- df %>% filter(Social=="High") %>% count(Economic)
cega.w2 <- df %>% filter(Social=="Low") %>% count(Economic)
cega.w3 <- df %>% filter(Social=="High", Economic=="High") %>% count(Events)
cega.w4 <- df %>% filter(Social=="High", Economic=="Low") %>% count(Events)
cega.w5 <- df %>% filter(Social=="Low", Economic=="High") %>% count(Events)
cega.w6 <- df %>% filter(Social=="Low", Economic=="Low") %>% count(Events)
cega.w7 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Low") |  (Social=="High" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
cega.w8 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Average") | 
                      (Social=="High" & Economic=="Low"  & Events=="Average") | 
                      (Social=="Low" & Economic=="High"  & Events=="Low") | 
                      (Social=="Low" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
cega.w9 <- df %>% filter((Social=="Low" & Economic=="High"  & Events=="Average") | 
                      (Social=="Low" & Economic=="High"  & Events=="High") | 
                      (Social=="High" & Economic=="High"  & Events=="High") | 
                      (Social=="High" & Economic=="Low"  & Events=="High") |
                      (Social=="Low" & Economic=="Low"  & Events=="High") |
                      (Social=="Low" & Economic=="Low"  & Events=="Average")) %>% count(Admission)
cega.struct <- list(cega.w0, cega.w1, cega.w2, cega.w3, cega.w4, cega.w5, cega.w6, cega.w7, cega.w8, cega.w9)#this is the observed values for each of the stages

#can initalize this, and prompt user to input the pathways for the particular tree
cega.stage.key <- list()
count(df) -> cega.stage.key[[1]]
df %>% count(Social) -> cega.stage.key[[2]]
df %>% count(Social, Economic) -> cega.stage.key[[3]]
df %>% count(Social, Economic, Events) -> cega.stage.key[[4]]
#define a stage key for each cut in the data 
#Q: how does this change for asymmetries?
cega.stage.key[[1]]$stage <- c("cega.w0")
cega.stage.key[[2]]$stage <- c("cega.w1", "cega.w2")
cega.stage.key[[3]]$stage <- c("cega.w3", "cega.w4", "cega.w5", "cega.w6")
cega.stage.key[[4]]$stage <- c("cega.w8", "cega.w7", "cega.w9", "cega.w8", "cega.w7", "cega.w9", "cega.w9", "cega.w8", "cega.w9", "cega.w9", "cega.w8", "cega.w9")#this contains the structure

##THIS IS FOR CEG-b
cegb.w0 <- df %>% count(Social)
cegb.w1 <- df %>% filter(Social=="High") %>% count(Economic)
cegb.w2 <- df %>% filter(Social=="Low") %>% count(Economic)
cegb.w3 <- df %>% filter(Social=="High", Economic=="High" | 
                           Social == "High", Economic=="Low") %>% count(Events)
cegb.w4 <- df %>% filter(Social=="Low", Economic=="High") %>% count(Events)
cegb.w5 <- df %>% filter(Social=="Low", Economic=="Low") %>% count(Events)
cegb.w6 <- df %>% filter((Social=="High" & Economic=="Low" & Events=="Low") | 
                           (Social=="High" & Economic=="High" & Events=="Low")) %>% count(Admission)
cegb.w7 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Average") | 
                           (Social=="High" & Economic=="Low"  & Events=="Average") | 
                           (Social=="Low" & Economic=="High"  & Events=="Low")|  
                           (Social=="Low" & Economic=="High"  & Events=="Average")|  
                           (Social=="Low" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
cegb.w8 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="High") | 
                      (Social=="High" & Economic=="Low"  & Events=="High") | 
                        (Social=="Low" & Economic=="High"  & Events=="High")| 
                      (Social=="Low" & Economic=="Low"  & Events=="Average") | 
                      (Social=="Low" & Economic=="Low"  & Events=="High")) %>% count(Admission)
cegb.struct <- list(cegb.w0, cegb.w1, cegb.w2, cegb.w3, cegb.w4, cegb.w5, cegb.w6, cegb.w7, cegb.w8)#this is the observed values for each of the stages

#can initalize this, and prompt user to input the pathways for the particular tree
cegb.stage.key <- list()
count(df) -> cegb.stage.key[[1]]
df %>% count(Social) -> cegb.stage.key[[2]]
df %>% count(Social, Economic) -> cegb.stage.key[[3]]
df %>% count(Social, Economic, Events) -> cegb.stage.key[[4]]
#define a stage key for each cut in the data 
#Q: how does this change for asymmetries?
cegb.stage.key[[1]]$stage <- c("cegb.w0")##THIS MUST START AT 0 AND NUM MUST BE LAST CHAR
cegb.stage.key[[2]]$stage <- c("cegb.w1", "cegb.w2")
cegb.stage.key[[3]]$stage <- c("cegb.w3", "cegb.w3", "cegb.w4", "cegb.w5")
cegb.stage.key[[4]]$stage <- c("cegb.w7", "cegb.w8","cegb.w6","cegb.w7",
                               "cegb.w8","cegb.w6","cegb.w7","cegb.w8",
                               "cegb.w7","cegb.w8","cegb.w8","cegb.w7")#this contains the structure
cegb.stages <- list("cegb.w0", "cegb.w1", "cegb.w2", "cegb.w3", "cegb.w4", "cegb.w5", "cegb.w6", "cegb.w7", "cegb.w8")


##GET THE REFERENCE PRIORS
cegb.prior <- get.ref.prior(df, cegb.struct, cuts, cegb.stage.key, cegb.stages)
cega.prior <- get.ref.prior(df, cega.struct, cuts, cega.stage.key, cega.stages)

#############################
###FUNCTION TO GET THE REFERENCE PRIOR
get.ref.prior <- function(df, st ruct, cuts, stage.key,stages) { #returns the reference prior of a CEG 
  #FIND THE REFERENCE PRIOR for each stage 
  n <- prod(apply(df, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
  cuts <- colnames(df) #each of the cuts that each variable must pass through
  alpha.bar <- max(apply(df, 2, function(x){length(levels(as.factor(x)))})) #max number of categories at each level in the dataset 
  obsv <- lapply(struct, function(x){x$n}) #takes the observed values for each of the priors
  ref.prior <- list()#will have prior for each stage (9 in case of CHDS example)
  ref.prior[[1]] <- rep(n/length(levels(struct[[1]][[cuts[[1]]]])), length(levels(struct[[1]][[cuts[[1]]]]))) #initialize the prior for w0
  counter=1
  #print(ref.prior[[1]])
  for (k in 2:length(cuts)){#start at the second cut
    for (l in 1:length(unique(stage.key[[k]]$stage))){#for each unique stage in the cut
      counter=counter+1 #which stage we're on
      #       print(counter)
      stage <- stages[counter]#moving through the stages top to bottom
      #    print(stage)
      in.paths<-stage.key[[k]][which(stage.key[[k]]$stage==stage),]#id the incoming pathways
      stages.of.interest <- merge(in.paths[,1:(k-1)], stage.key[[(k-1)]][,c(1:(k-1),dim(stage.key[[(k-1)]])[2])])$stage
      ref.prior.idx <- unlist(lapply(stages.of.interest, function(x){as.numeric(substr(x,nchar(x),nchar(x)))+1}))#gives the stage number, because of weird indexing, want 
      #^this is actually the stage index
      numtor <- sum(sapply(ref.prior[ref.prior.idx], FUN = `[[`, 1))#the 2 here is the index of the cut that we want it to pull out of the prior lists.
      denom <- dim(struct[[counter]])[1]#number of outgoing edges
      ref.prior[[counter]] <- rep(numtor/denom, denom)
      #   print(ref.prior[[counter]])
    }
  }
  return(ref.prior)
}
###############################################
##BATCH MONTIOR FOR THE BN


###############################################
##BATCH MONITORS FOR THE CEG 
ceg.batch.monitor <- function(df, struct, stage.key, stages, which.cut){ 
  #which.cut is an integer that determines the cut for which we want to compute the batch monitors of two CEGs.
  colnames(df) -> cuts 
  obsv <- stage.key[[which.cut]]$n #how many observed in each pathway through cut
  prior <- get.ref.prior(df, struct, cuts, stage.key, stages) #compute the prior
  which.to.add <- unlist(lapply(stage.key[[which.cut]]$stage, function(x){as.numeric(substr(x,nchar(x),nchar(x)))}))
  expct.cut <- unlist(prior[unlist(lapply(unique(stage.key[[which.cut-1]]$stage), function(x){as.numeric(substr(x,nchar(x),nchar(x)))+1}))])
  
  prior.vec <- rep(0, length(unique(which.to.add)))
  obsv.vec <- rep(0, length(unique(which.to.add)))
  for (i in 1:length(unique(which.to.add))){
    target.stage <- unique(which.to.add)[i]
    prior.vec[i] <- sum(expct.cut[which(which.to.add==which.to.add[i])])
    obsv.vec[i] <- sum(obsv[which(which.to.add==which.to.add[i])])
  }

  expct.vec <- (prior.vec/n)*dim(df)[1]#scale the expected vec
  pearson <- sum((obsv.vec-expct.vec)^2/expct.vec)
  return(pearson) #returns the pearson coefficient for the pathways which can be used to find the p-value
  #go back in and add the discount factor
  }
 
 cega.batch.monitor <- ceg.batch.monitor(df, cega.struct, cega.stage.key, cega.stages,3)
 cegb.batch.monitor <- ceg.batch.monitor(df, cegb.struct, cegb.stage.key, cegb.stages,3)
 
###############################################################
##UNCONDITIONAL NODE MONITOR FOR BOTH BNS AND CEGS (ideal for root nodes in CEGs)
#df is the data in question, col_name is the stage in question, prior is the set prior (must have right number of iterations), n is the max sample size we wish to consider.
 #df should be filtered first
 ceg.uncondtnl.stage.monitor <- function(df, col_name="Events", prior, n=50, learn=FALSE) {#dataframes should also be added for the counts
  #add checks to make sure that prior has same number of items as counts in dataframe
  Zm <- rep(NA, n)
  Sm <- rep(NA, n)
  Em <- rep(NA, n)
  Vm <- rep(NA, n)
  p <- rep(NA, n)
  
  if(learn==FALSE){
    for (i in 2:n){
      df_cut <- df[2:i,] 
      df_cut %>%
        group_by_(col_name) %>% #groups by the stage of interest
        tally() -> u1 #stage1
      counts = u1$n 
      p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#logprobability
      #compute the z statistics
     Sm[i]=-p[i]
     Em[i]=sum((prior/sum(prior))*sum(counts))
     Vm[i]=sum(prior*(sum(prior)-prior))/(sum(prior)^2*(sum(prior)+1))
     Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
    }
  }
  else{
    for (i in 2:n){
      newprior <- prior
      df_cut <- df[2:i,] 
      df_cut %>%
        group_by_(col_name) %>% #groups by the stage of interest
        tally() -> u1 #stage1
      counts = u1$n 
      p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#logprobability
      #compute the z statistics
      Sm[i]=-p[i]
      Em[i]=sum((newprior/sum(newprior))*sum(counts))
      Vm[i]=sum(newprior*(sum(newprior)-newprior))/(sum(newprior)^2*(sum(newprior)+1))
      Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
      newprior <- (prior+counts)/sum(counts+1)
    }  
  }
  return(list(Sm,Zm, Em, Vm))
}

mod1 <- ceg.uncondtnl.node.monitor(df,col_name = "Social", prior=c(10,1),n=50)
mod2 <- ceg.uncondtnl.node.monitor(df,col_name = "Social", prior=c(.05,.05),n=50)

plot(mod2[[2]],xlab='Relevant sample size', ylab = 'Cumulative logarithmic penalty')
lines(mod1[[2]])
legend(2,25,c("Dir(1,10)", "Dir(6,4)"))
title("Diagnostics for u1")
plot(mod2[[1]],xlab='Relevant sample size', ylab = 'z statistic')
lines(mod1[[1]])


#############################################
##CONDITIONAL NODE MONITOR FOR BNS
bn.cndtl.node.monitor <- function(df, parents, parent.values, child, n=50, learn=FALSE) {#dataframes should also be added for the counts
  #add checks to make sure that prior has same number of items as counts in dataframe
  
  #passing col names to the filtering bit
  #p.sym <- sym(parents)
  #p.sym <- lapply(parents, sym)
  c.sym <- sym(child)
  alpha.bar <- max(apply(df, 2, function(x){length(levels(as.factor(x)))})) #max number of categories at each level in the dataset 
  prior <- rep(1, length(levels(df[[child]])))/length(levels(df[[child]]))
  
  #initialize log penalty scores
  Zm <- rep(NA, n)
  Sm <- rep(NA, n)
  Em <- rep(NA, n)
  Vm <- rep(NA, n)
  p <- rep(NA, n)
  
    for (i in 1:n){
      
      df_cut <- df[1:i,] 
      #for each parent, filter it off 
      for (j in 1: length(parents)){
        df_cut <- filter(df_cut, UQ(sym(parents[j])) == parent.values[j])   
      }
      
      df_cut %>% count(!!c.sym) -> counts.tbl
      counts = counts.tbl$n 
      
      p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#logprobability
      #compute the z statistics
      Sm[i]=-p[i]
      Em[i]=sum((prior/sum(prior))*sum(counts))
      Vm[i]=sum(prior*(sum(prior)-prior))/(sum(prior)^2*(sum(prior)+1))
      Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
  }
  return(list(Sm,Zm, Em, Vm))
}

##EXAMPLES OF BNS
#testex
bn.mod1 <- bn.cndtl.node.monitor(df, parents = "Social", parent.values = "High", child = "Economic",n=50)
plot(bn.mod1[[1]]) 

#conditional node montiors for Events
#BN-a 
bn.modA <- bn.cndtl.node.monitor(df, parents = c("Social", "Economic"), parent.values = c("Low","Low"), child = "Events",n=50)
plot(bn.modA[[1]])
#BN-b
bn.modB <- bn.cndtl.node.monitor(df, parents = "Social", parent.values = "Low", child = "Events",n=50)
plot(bn.modB[[1]]) 

#conditional node montior for Social 
#BN-c
bn.modC <- bn.cndtl.node.monitor(df, parents = "Economic", parent.values = "Low", child = "Social",n=50)
plot(bn.modC[[1]]) 
#BN-d
bn.modD <- cndtl.node.monitor(df, parents = "Events", parent.values = "Low", child = "Social",n=50)
plot(bn.modD[[1]]) 

##############################################



  
#TODO: can implement checks that the counts in each cut sum to the number of observations in the dataframe 

#CEG cndtl stage monitor-- check to see what the contribution of a specific pathway is (say the impact of w0, w2, w5 on w8 in CEG A)
#CEG uncndtnl stage monitor-- check the prior taking all contributions.

ceg.uncndtl.stage.monitor <- function(df, stage, stage.key, struct, n=50) {
  #the stages start at w0 OTHERWISE YOU WILL LAND IN INDEXING HELL. 
  
  #figure out what the reference prior is on the stage of interest 
  n <- prod(apply(df, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
  cuts <- colnames(df) #each of the cuts that each variable must pass through
  alpha.bar <- max(apply(df, 2, function(x){length(levels(as.factor(x)))})) #max number of categories at each level in the dataset 
  
  obsv <- lapply(struct, function(x){x$n}) #takes the observed values for each of the priors
  
  #FIND THE REFERENCE PRIOR for each stage 
  ref.prior <- list()#will have prior for each stage (9 in case of CHDS example)
  ref.prior[[1]] <- rep(n/length(levels(struct[[1]][[cuts[[1]]]])), length(levels(struct[[1]][[cuts[[1]]]]))) #initialize the prior for w0
  counter=1
#print(ref.prior[[1]])
    for (k in 2:length(cuts)){#start at the second cut
      for (l in 1:length(unique(stage.key[[k]]$stage))){#for each unique stage in the cut
        counter=counter+1 #which stage we're on
 #       print(counter)
      stage <- stages[counter]#moving through the stages top to bottom
  #    print(stage)
        in.paths<-stage.key[[k]][which(stage.key[[k]]$stage==stage),]#id the incoming pathways
      stages.of.interest <- merge(in.paths[,1:(k-1)], stage.key[[(k-1)]][,c(1:(k-1),dim(stage.key[[(k-1)]])[2])])$stage
      ref.prior.idx <- unlist(lapply(stages.of.interest, function(x){as.numeric(substr(x,2,2))+1}))#gives the stage number, because of weird indexing, want 
      #^this is actually the stage index
      numtor <- sum(sapply(ref.prior[ref.prior.idx], FUN = `[[`, 1))#the 2 here is the index of the cut that we want it to pull out of the prior lists.
      denom <- dim(struct[[counter]])[1]#number of outgoing edges
      ref.prior[[counter]] <- rep(numtor/denom, denom)
   #   print(ref.prior[[counter]])
      }
    }

  
  #initialize log penalty scores
  Zm <- rep(NA, n)
  Sm <- rep(NA, n)
  Em <- rep(NA, n)
  Vm <- rep(NA, n)
  p <- rep(NA, n)
  
  for (i in 1:n){
    
    df_cut <- df[1:i,] 
    #for each parent, filter it off 
    for (j in 1: length(parents)){
      df_cut <- filter(df_cut, UQ(sym(parents[j])) == parent.values[j])   
    }
    
    
    df_cut %>% count(!!c.sym) -> counts.tbl
    
    counts = counts.tbl$n 
    p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#logprobability
    #compute the z statistics
    Sm[i]=-p[i]
    Em[i]=sum((prior/sum(prior))*sum(counts))
    Vm[i]=sum(prior*(sum(prior)-prior))/(sum(prior)^2*(sum(prior)+1))
    Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
  }
  return(list(Sm,Zm, Em, Vm))
}

