library(rlang);library(tidyverse); library(DiagrammeR)

#radical dataset 

radical.df<- read.csv(file = "data1.csv")
radical.sst <- CEG.AHC(radical.df) #find best with the AHC algorithm

#translate the format to feed into the new function format
radical.stage.key <- tostagekey(radical.df, radical.sst)#the stage key
map(radical.stage.key[-1], ~select(.x,-key))->radical.sk#a little bit of finagling to make it not blow up immediately.
radical.sk <- c(radical.stage.key[1],radical.sk)
pull(map_df(radical.stage.key, ~distinct(.x,stage)),stage )-> radical.stages#the stages
radical.struct <- to.struct(radical.df,radical.stage.key,radical.sst) #the struct. a weird name for the data results of the AHC alg
radical.cuts <- colnames(radical.df)

renderCEG(radical.stage.key,radical.df)#plot the new stratified CEG

radical.sst$lik #Bayes Factor of -40021.6 with reference prior that has sample size, alpha=3

#now examine the component monitors. these have wildly different priors
component.monitor.ceg(radical.df, target.stage='w3',condtnl.stage = 'w2',target.cut = 3,stages=radical.stages,stage.key=radical.sk,struct=radical.struct)

radical.stage.num <- do.call(rbind, lapply(radical.sst$comparisonset, length)) #gives the number of stages (except the root node)


allComponents <- function(targetCut, condtnlCut) {#a useful fn that runs component monitors for the entire model
  crossing(unique(radical.sk[[targetCut]]$stage), unique(radical.sk[[condtnlCut]]$stage))->whichstages
  whichstages$score <-rep(NA,length(whichstages[,1]))
  for (i in 1:dim(whichstages)[1]){
  new <- component.monitor.ceg(radical.df, target.stage=as.character(whichstages[i,1]), condtnl.stage = as.character(whichstages[i,2]),target.cut=targetCut,stages=radical.stages,stage.key=radical.sk, struct=radical.struct)
  whichstages$score[i] <- new
  }
  names(whichstages)[1:2] <- c('targetStage','condtnlStage')
  return(whichstages)
}

allComponents(3,2)
allComponents(4,3)
allComponents(5,4)
#should purrr the heck out of this to get the component score of this CEG 

#after finding out which components are troublesome...might like to visualize this. 
#try to renderCEG but where the circle size shows the length of the Bayes Facot 


radical.prior <-get.ref.prior(df=radical.df,struct=radical.struct,cuts=radical.cuts,stage.key=radical.sk,stages=radical.stages)#check that we can get the prior

ceg.child.parent.monitor(df=radical.df,
                         target.stage = "w3",
                         target.cut = 3, 
                         condtnl.stage = "w2",
                         struct = radical.struct,
                         stage.key = radical.sk, 
                         stages=radical.stages,
                         n = 50,
                         learn = F)



#CHDS example
chds.df <-read_csv(file = "CHDS.latentexample1.csv")
chds.df <-read.csv(file = "CHDS.latentexample1.csv")#,stringsAsFactors = F) #note: sst requires factors
chds.sst <- jCEG.AHC(chds.df)
chds.sst$result

library(ceg)
R.sst <-Stratified.staged.tree(chds.df)
Stratified.ceg.model(R.sst)->chds.ceg
plot(chds.ceg)
