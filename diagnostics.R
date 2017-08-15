rm(list=ls())
library(dplyr);library(ggplot2)
setwd("/home/rachel/Documents/diagnostics/")
df<-read.csv(file = "CHDS.latentexample1.csv")

 df %>%
   group_by(Social, Economic, Events, Admission) %>%
   tally() -> counts #counts for all pathways in the CEG
 #write.csv(counts,'counts.csv')

 
#df is the data in question, col_name is the stage in question, prior is the set prior (must have right number of iterations), n is the max sample size we wish to consider.
 #df should be filtered first
 get.zhed.ceg <- function(df, col_name="Events", prior, n=50, learn=FALSE) {#dataframes should also be added for the counts
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

mod1 <- get.zhed.ceg(df,col_name = "Social", prior=c(10,1),n=50)
mod2 <- get.zhed.ceg(df,col_name = "Social", prior=c(.05,.05),n=50)

plot(mod2[[2]],xlab='Relevant sample size', ylab = 'Cumulative logarithmic penalty')
lines(mod1[[2]])
legend(2,25,c("Dir(1,10)", "Dir(6,4)"))
title("Diagnostics for u1")
plot(mod2[[1]],xlab='Relevant sample size', ylab = 'z statistic')
lines(mod1[[1]])

#for stage 2
mod3 <- get.zhed.ceg(filter(df, Social == "High", Economic == "Low",),col_name = "Events", prior=c(3,3,3),n=20,learn=TRUE)
plot((mod3[[1]]))



#how do we store positions of the CEG in the dplyr function? 
df %>% group_by(Social) %>% tally() #w1
df %>% filter(Social=="High") %>% group_by(Economic) %>% tally() #w2
df %>% filter(Social=="Low") %>% group_by(Economic) %>% tally() #w3
df %>% filter(Social=="High") %>% group_by(Events) %>% tally() #w4
df %>% filter(Social=="Low", Economic=="High") %>% group_by(Events) %>% tally() #w5
df %>% filter(Social=="Low", Economic=="Low") %>% group_by(Events) %>% tally() #w6
df %>% filter(Social=="High",Events=="Low") %>% group_by(Admission) %>% tally() #w7


#the above describes the procedure for the stage monitor of a CEG 
#now we would like a node monitor for BNs to compare it to 

#take BN-A from the CEG book
parents <- "Social"
parent.values <- "High"
child <- "Economic"

####COPIED FROM ABOVE
cndtl.node.monitor <- function(df, parents, parent.values, child, n=50, learn=FALSE) {#dataframes should also be added for the counts
  #add checks to make sure that prior has same number of items as counts in dataframe
  
  #passing col names to the filtering bit
  #p.sym <- sym(parents)
  #p.sym <- lapply(parents, sym)
  c.sym <- sym(child)
  alpha.bar <- max(apply(df, 2, function(x){length(levels(as.factor(x)))})) #max number of categories at each level in the dataset 
  prior <- rep(1, length(levels(df[[child]])))/3
  
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

#testex
bn.mod1 <- cndtl.node.monitor(df, parents = "Social", parent.values = "High", child = "Economic",n=50)
plot(bn.mod1[[1]]) 

#BN-b
bn.mod2 <- cndtl.node.monitor(df, parents = "Social", parent.values = "Low", child = "Events",n=50)
plot(bn.mod2[[1]]) 
#BN-a 
bn.mod3 <- cndtl.node.monitor(df, parents = c("Social", "Economic"), parent.values = c("Low","Low"), child = "Events",n=50)
plot(bn.mod3[[1]]) 


