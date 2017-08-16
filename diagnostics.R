rm(list=ls())
library(dplyr);library(ggplot2)
setwd("/home/rachel/Documents/diagnostics/")
df<-read.csv(file = "CHDS.latentexample1.csv")


# #how do we store positions of the CEG in the dplyr function? 
# df %>% group_by(Social) %>% tally() #w1
# df %>% filter(Social=="High") %>% group_by(Economic) %>% tally() #w2
# df %>% filter(Social=="Low") %>% group_by(Economic) %>% tally() #w3
# df %>% filter(Social=="High") %>% group_by(Events) %>% tally() #w4
# df %>% filter(Social=="Low", Economic=="High") %>% group_by(Events) %>% tally() #w5
# df %>% filter(Social=="Low", Economic=="Low") %>% group_by(Events) %>% tally() #w6
# df %>% filter(Social=="High",Events=="Low") %>% group_by(Admission) %>% tally() #w7

###############################################
##BATCH MONITORS FOR THE CEG 
ceg.batch.monitor <- function(df, struct){ 
  colnames(df) -> cuts
  
  df %>%
    group_by(Social, Economic, Events, Admission) %>%
    tally() -> obsv #counts for all pathways in the CEG
  
  n.cat <- apply(df, 2, function(x){length(levels(as.factor(x)))})
  alpha.bar=prod(n.cat)
  }
 
df %>%
   group_by(Social, Economic, Events, Admission) %>%
   tally() -> counts #counts for all pathways in the CEG
 #write.csv(counts,'counts.csv')

#how do we code up a CEG; JH- as a list of vertices and priors associated with each one
df %>% count(Social)
df %>% count(Social, Economic)
df %>% count(Social, Economic, Events)
df %>% count(Social, Economic, Events, Admission)
 
###############################################################
##UNCONDITIONAL NODE MONITOR FOR BOTH BNS AND CEGS (ideal for root nodes in CEGs)
#df is the data in question, col_name is the stage in question, prior is the set prior (must have right number of iterations), n is the max sample size we wish to consider.
 #df should be filtered first
 ceg.uncondtnl.node.monitor <- function(df, col_name="Events", prior, n=50, learn=FALSE) {#dataframes should also be added for the counts
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

##EXAMPLES
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


##CONDITIONAL STAGE MONITOR FOR THE CEG 

strcut <- list(w0, w1, w2, w3, w5, w6, w7, w8, w9, wINF)

#take the sturated tree
saturated.tree <- count(df, Social, Economic, Events, Admission)

#set the priors on the stages
#HOW TO FUNCTIONIZE?  
##THIS IS FOR CEG-a
w0 <- df %>% count(Social)
w1 <- df %>% filter(Social=="High") %>% count(Economic)
w2 <- df %>% filter(Social=="Low") %>% count(Economic)
w3 <- df %>% filter(Social=="High", Economic=="High") %>% count(Events)
w4 <- df %>% filter(Social=="High", Economic=="Low") %>% count(Events)
w5 <- df %>% filter(Social=="Low", Economic=="High") %>% count(Events)
w6 <- df %>% filter(Social=="Low", Economic=="Low") %>% count(Events)
w7 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Low") |  (Social=="High" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
w8 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Average") | 
                      (Social=="High" & Economic=="Low"  & Events=="Average") | 
                      (Social=="Low" & Economic=="High"  & Events=="Low") | 
                      (Social=="Low" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
w9 <- df %>% filter((Social=="Low" & Economic=="High"  & Events=="Average") | 
                             (Social=="Low" & Economic=="High"  & Events=="High") | 
                             (Social=="High" & Economic=="High"  & Events=="High")| 
                             (Social=="High" & Economic=="Low"  & Events=="High") |
                             (Social=="Low" & Economic=="Low"  & Events=="High") |
                             (Social=="Low" & Economic=="Low"  & Events=="Average")) %>% count(Admission)
struct <- list(w0, w1, w2, w3, w5, w6, w7, w8, w9)#this is the observed values for each of the stages
stage = w8 #sample for buildling function

#can initalize this, and prompt user to input the pathways for the particular tree
stage.key <- list()
df %>% count(Social) -> stage.key[[1]]
df %>% count(Social, Economic) -> stage.key[[2]]
df %>% count(Social, Economic, Events) -> stage.key[[3]]
#define a stage key for each cut in the data 
#Q: how does this change for asymmetries?
stage.key[[1]]$stage <- c("w1", "w2")
stage.key[[2]]$stage <- c("w3", "w4", "w5", "w6")
stage.key[[3]]$stage <- c("w8", "w7", "w9", "w8", "w7", "w9", "w9", "w8", "w9", "w9", "w8", "w9")#this contains the structure


  
#TODO: can implement checks that the counts in each cut sum to the number of observations in the dataframe 

#CEG cndtl stage monitor-- check to see what the contribution of a specific pathway is (say the impact of w0, w2, w5 on w8 in CEG A)
#CEG uncndtnl stage monitor-- check the prior taking all contributions.

ceg.uncndtl.node.monitor <- function(df, stage, stage.key, struct, n=50) {
  
  #figure out what the reference prior is on the stage of interest 
  n <- prod(apply(df, 2, function(x){length(levels(as.factor(x)))})) #total number of pathways in the CEG 
  cuts <- colnames(df) #each of the cuts that each variable must pass through
  alpha.bar <- max(apply(df, 2, function(x){length(levels(as.factor(x)))})) #max number of categories at each level in the dataset 
  
  obsv <- lapply(struct, function(x){x$n}) #takes the observed values for each of the priors
  
  #find the reference prior for each thing in the stage 
  ref.prior <- list()#will have prior for each stage (9 in case of CHDS example)
  
  for (k in 2:length(cuts)){
    for (l in 1:length(unique(stage.key[[k]]$stage)))#for each unique stage in the cut
      numtor <- ref.prior[[1]][1] #how many came in from the previous stage 
      denom <- length(levels(w0[[UQ(cuts[k])]]))
  }
  
  #to figure this out
  ref.prior[[1]] <- c(12,12)
  ref.prior[[2]] <- c(6,6)
  ref.prior[[3]] <- c(6,6)
  ref.prior[[4]] <- c(2,2,2)
  ref.prior[[5]] <- c(2,2,2)
  ref.prior[[6]] <- c(2,2,2)
  ref.prior[[7]] <- c(2,2,2)
  
  #now for stage 8 
  in.paths<-stage.key[[3]][which(stage.key[[3]]$stage=="w8"),]
  stages.of.interest <- stage.key[[2]]$stage[which(in.paths[["Social"]]==stage.key[[2]][["Social"]] & in.paths[["Economic"]]==stage.key[[2]][["Economic"]])]
  ref.prior.idx <- unlist(lapply(stages.of.interest, function(x){as.numeric(substr(x,2,2))-1}))#gives the stage number, because of weird indexing, want 
  sum(sapply(ref.prior[ref.prior.idx], FUN = `[[`, 2))#the 2 here is the index of the cut that we want it to pull out of the prior lists.
  
  #take the cut-th factor level from prior and sum
    
  ref.prior[[1]] <- rep(n/length(levels(w0[[UQ(cuts[1])]])),length(levels(w0[[UQ(cuts[1])]])))   #for stage w0, divide n by number of levels
  ref.prior[[2]] <- rep(12/length(levels(w0[[UQ(cuts[2)]]))),length(levels(w0[[UQ(cuts[2])]]))) #for stage w
  ref.prior[[3]] <- rep(12/length(levels(w0[[UQ(cuts[2])]])),length(levels(w0[[UQ(cuts[2])]]))) #for stage w
  ref.prior[[4]] <- rep(6/length(levels(w0[[UQ(cuts[3])]])),length(levels(w0[[UQ(cuts[3])]]))) #for stage w
  ref.prior[[5]] <- rep(6/length(levels(w0[[UQ(cuts[3])]])),length(levels(w0[[UQ(cuts[3])]]))) #for stage w
  ref.prior[[6]] <- rep(6/length(levels(w0[[UQ(cuts[3])]])),length(levels(w0[[UQ(cuts[3])]]))) #for stage w
  ref.prior[[7]] <- rep(6/length(levels(w0[[UQ(cuts[3])]])),length(levels(w0[[UQ(cuts[3])]]))) #for stage w
  
  #figure out which edges are coming in
  
  
  #prior <- rep(1, length(levels(df[[child]])))/length(levels(df[[child]]))
  
  
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

