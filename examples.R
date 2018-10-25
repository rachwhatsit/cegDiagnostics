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

renderpart <- function(colvec, df){
  cut.df <- df[,colvec]
  sst <- CEG.AHC(cut.df)
  sk <- tostagekey(cut.df, sst)
  renderCEG(sk,cut.df)
}
radical.sst$lik #Bayes Factor of -40021.6 with reference prior that has sample size, alpha=3

#now examine the component monitors. these have wildly different priors
component.monitor.ceg(radical.df, target.stage='w3',condtnl.stage = 'w2',target.cut = 3,stages=radical.stages,stage.key=radical.sk,struct=radical.struct)

radical.stage.num <- do.call(rbind, lapply(radical.sst$comparisonset, length)) #gives the number of stages (except the root node)


allComponents <- function(targetCut, condtnlCut,df, stage.key,stages,struct,prior) {#a useful fn that runs component monitors for the entire model
  crossing(unique(stage.key[[targetCut]]$stage), unique(stage.key[[condtnlCut]]$stage))->whichstages
  whichstages$score <-rep(NA,length(whichstages[,1]))
  for (i in 1:dim(whichstages)[1]){
  new <- component.monitor.ceg(df, target.stage=as.character(whichstages[i,1]), condtnl.stage = as.character(whichstages[i,2]),target.cut=targetCut,stages=stages,stage.key=stage.key, struct=struct,prior)
  whichstages$score[i] <- new
  }
  names(whichstages)[1:2] <- c('targetStage','condtnlStage')
  return(whichstages)
}

allComponents(3,2)
allComponents(4,3)
allComponents(5,4)

##checking that the score function works
# score <- function(alpha, N){
# sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) - lgamma(sum(alpha + N)))->p
#   return(p)
# }

# radical.sst$data[!is.na(radical.sst$data)]
# 
# idx <-
#   which(!is.na(unlist(lapply(
#     radical.sst$data, '[[', 1
#   ))) == TRUE)
# radical.sst$data[idx] -> radicalN
# 
# components <- c()
# for (i in 1:37){
#   components[i]  <- score(unlist(radical.prior[i]), unlist(radicalN[i]))
# }
# 

#after finding out which components are troublesome...might like to visualize this. 
#try to renderCEG but where the circle size shows the length of the Bayes Factor


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


###########################################################chds
#CHDS example
#chds.df <-read_csv(file = "CHDS.latentexample1.csv")#don't use this for AHC
chds.df <-read.csv(file = "CHDS.latentexample1.csv")#,stringsAsFactors = F) #note: sst requires factors
chds.sst <- CEG.AHC(chds.df)
chds.sst$result
#translate to run the diagnostic code 
tostagekey(chds.df, chds.sst)-> chds.stage.key
pull(map_df(chds.stage.key, ~distinct(.x,stage)),stage )-> chds.stages#the stages
chds.struct <- to.struct(chds.df,chds.stage.key,chds.sst) #the struct. a weird name for the data results of the AHC alg
chds.cuts <- colnames(chds.df)

renderCEG(chds.stage.key,chds.df)

##for comparison with rodriguo's code 
library(ceg)
R.sst <-Stratified.staged.tree(chds.df)
Stratified.ceg.model(R.sst)->chds.ceg
plot(chds.ceg)#we get the same thing, so this is solid
R.sst@model.score
#get the likelihood
chds.sst$lik#right score fo=rom the book

#slight difference between RC and LB code here 
#-2485.54 for RC and -2478.49 for LB 


#test against another ordering
chds.sst2 <- CEG.AHC(chds.df[,c(2,1,3,4)]) #varorder E S L H 
chds.sst2$lik; #equivalent to chds.sst so same likelihoods 
chds.sst2.sk <- tostagekey(chds.df[,c(2,1,3,4)],sst = chds.sst2)
renderCEG(chds.sst2.sk, chds.df[,c(2,1,3,4)])

chds.sst3 <- CEG.AHC(chds.df[,c(1,3,2,4)])
chds.sst3$lik #same likelihoods because statistically equivalent
chds.sst3.sk <- tostagekey(chds.df[,c(1,3,2,4)],sst = chds.sst3) # var order S L E H 
renderCEG(chds.sst3.sk, chds.df[,c(1,3,2,4)])
chds.sst4 <- CEG.AHC(chds.df[,c(3,1,2,4)])
chds.sst4$lik #same likelihood because statistically equivalent
chds.sst4.sk <- tostagekey(chds.df[,c(3,1,2,4)],sst = chds.sst4) # var order L S E H 
renderCEG(chds.sst4.sk, chds.df[,c(3,1,2,4)])
chds.sst4$result

map(chds.stage.key[-1], ~select(.x,-key))->chds.sk#a little bit of finagling 
chds.sk <- c(chds.stage.key[1],chds.sk)
map(chds.sk, ~select(.x,-color))->chds.sk.nocol
chds.prior <-get.ref.prior(df=chds.df,struct=chds.struct,cuts=chds.cuts,stage.key=chds.sk.nocol,stages=chds.stages)#check that we can get the prior
#df=chds.df;struct=chds.struct;cuts=chds.cuts;stage.key=chds.sk.nocol;stages=chds.stages#fortroubleshooting

#what to do about the first stage
allComponents(targetCut = 2,condtnlCut = 1,df = chds.df,stage.key = chds.sk.nocol,stages = chds.stages,struct = chds.struct,chds.prior)
allComponents(targetCut = 3,condtnlCut = 2,df = chds.df,stage.key = chds.sk.nocol,stages = chds.stages,struct = chds.struct,chds.prior)
#much of the BF score comes from stage w3 to w1
allComponents(targetCut = 4,condtnlCut = 3,df = chds.df,stage.key = chds.sk.nocol,stages = chds.stages,struct = chds.struct,chds.prior)

new <- component.monitor.ceg(chds.df, target.stage='w3', condtnl.stage = 'w1',target.cut=3,stages=chds.stages,stage.key=chds.sk.nocol struct=chds.struct,chds.prior)
chds.df; target.stage='w3'; condtnl.stage = 'w1';target.cut=3;stages=chds.stages;stage.key=chds.sk.nocol;struct=chds.struct;prior=chds.prior

###########NEW FN TO FIND THE DATA FOR THE POSITIONS

getdata(chds.df, chds.sk.nocol) -> chds.data
chds.data[[1]]<- chds.sk.nocol[[2]]$n

chds.prior

score <- function(alpha, N){
sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) - lgamma(sum(alpha + N)))->p
  return(p)
}


components <- c()
for (i in 1:length(chds.prior)){
  components[i]  <- score(unlist(chds.prior[i]), unlist(chds.data[i]))
}

cut=4
colnames(stage.key[[3]])[4] <- "from.stage"
colnames(stage.key[[4]])[5] <- "to.stage"
full_join(stage.key[[4]],stage.key[[3]], by=c("Social", "Economic")) %>%  
  group_by(Events, from.stage) %>% distinct(Events, from.stage, .keep_all=TRUE) %>% summarise(cnts = sum(n.x)) -> data.df

data.df$cnts[which(data.df$from.stage=='w3')]

full_join(stage.key[[3]],stage.key[[4]],by = c(Social, Economic)) %>% group_by(from.stage)

full_join(stage.key[[3]],stage.key[[4]])
stage.key[[4]] %>% select(Events, stage,n) %>% distinct(Events,stage)
# components <- function(sst, prior) {
#   idx <-
#     which(!is.na(unlist(lapply(
#       sst$data, '[[', 1
#     ))) == TRUE)
#   sst$data[idx] -> NN
#   
#   components <- c()
#   for (i in 1:length(prior)){
#     components[i]  <- score(unlist(prior[i]), unlist(NN[i]))
#   }
# }


chds.stage.key[[3]] #High Social, High Economic, High Social and Low Economic are triggering a pretty high error.
#is it strangethat these get lumped in the same stage??
renderCEG(chds.stage.key, chds.df)

#Is there another staging of the situations in cut 3 that would be more appropriate? 
chds.part.monitor <- part.monitor(rho = .7,epsilon = 1.2,df_cut = chds.df,which.cut = 3,stage.key = chds.sk.nocol,n.monitor = 860)
#k is 0.9 in freeman paper
#tau is length of lag

#rho = .7;epsilon = 1.2;df_cut = chds.df;which.cut = 3;stage.key = chds.sk;n.monitor = 200#for trblshtng

chds.crrnt.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 4)) #possible.coloring 3
chds.alt1.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 1))#possible.coloring 10
chds.alt2.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 2))#possible.coloring 11
chds.alt3.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 3))#possible.coloring 13

possible.colorings[[3]] #(HH, HL, LH) (LL)
possible.colorings[[13]]#(HH) (HL, LH) (LL)

chds.part.df <- as.data.frame(cbind(5:860,chds.crrnt.stg.probs, chds.alt1.stg.probs, chds.alt2.stg.probs, chds.alt3.stg.probs))
colnames(chds.part.df) <- c("t", "Staging1", "Staging2", "Staging3", "Staging4")
chds.part.df %>% 
  gather(key, value, -t) %>%
  ggplot(aes(x=t, y=value, colour=key)) + geom_line()

chds.w7w3.pach <- ceg.child.parent.monitor(df=chds.df,
                         target.stage = "w7",
                         target.cut = 4, 
                         condtnl.stage = "w3",
                         struct = chds.struct,
                         stage.key = chds.sk.nocol, 
                         stages=chds.stages,
                         prior=chds.prior,
                         n = 860,
                         learn = F)
plot(chds.w7w3.pach[,1])

chds.w7w5.pach <- ceg.child.parent.monitor(df=chds.df,
                                         target.stage = "w7",
                                         target.cut = 4, 
                                         condtnl.stage = "w5",
                                         struct = chds.struct,
                                         stage.key = chds.sk.nocol, 
                                         stages=chds.stages,
                                         prior=chds.prior,
                                         n = 860,
                                         learn = F)
lines(chds.w7w5.pach[,1])
chds.pach <- as.data.frame(cbind(1:860,chds.w7w3.pach[,1],chds.w7w5.pach[,1]))
colnames(chds.pach) <- c('t','w7w3','w7w5')

chds.pach %>%
  gather(key,value, -t) %>% 
  ggplot(aes(x=t,y=value,colour=key))+geom_line()
