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

#renderCEG(radical.stage.key,radical.df)#plot the new stratified CEG

#visualize the first few cuts of the CEG, could try with the data.tree
renderpart <- function(colvec, df){#running AHC on a subset of the data returns entirely different results 
  cut.df <- df[,colvec]
  sst <- CEG.AHC(cut.df)
  sk <- tostagekey(cut.df, sst)
  renderCEG(sk,cut.df)
}
renderpart(1:4,radical.df)


radical.sst$lik #Bayes Factor of -40021.6 with reference prior that has effective sample size 

map(radical.stage.key[-1], ~select(.x,-key))->radical.sk#a little bit of finagling 
radical.sk <- c(radical.stage.key[1],radical.sk)
map(radical.sk, ~select(.x,-pos))->radical.sk.nocol

#the real component monitor 
getdata(radical.df, radical.sk.nocol) -> radical.data
radical.data[[1]]<- radical.sk.nocol[[2]]$n
radical.prior <- radical.sst$prior[which(!is.na(unlist(lapply(radical.sst$prior, '[[', 1))) == TRUE)]
#get the componens 
radical.components <- component.monitor(radical.data,radical.prior)

cbind(radical.stages, radical.components)

radical.part.monitor <- part.monitor(rho = .7,epsilon = 1.2,df_cut = radical.df,which.cut = 2,stage.key = radical.sk.nocol,n.monitor = 200)

####


#now examine the component monitors. these have wildly different priors
component.monitor.ceg(radical.df, target.stage='w3',condtnl.stage = 'w2',target.cut = 3,stages=radical.stages,stage.key=radical.sk,struct=radical.struct)

radical.stage.num <- do.call(rbind, lapply(radical.sst$comparisonset, length)) #gives the number of stages (except the root node)

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
# library(ceg)
# R.sst <-Stratified.staged.tree(chds.df)
# Stratified.ceg.model(R.sst)->chds.ceg
# plot(chds.ceg)#we get the same thing, so this is solid
# R.sst@model.score
# #get the likelihood
# chds.sst$lik#right score fo=rom the book

#slight difference between RC and LB code here 
#-2485.54 for RC and -2478.49 for LB 

chds.stage.key -> chds.bn.stage.key
chds.bn.stage.key[[3]]$pos <- c('w3','w4','w5','w6')
chds.bn.stage.key[[3]]$stage <- c('u3','u4','u5','u6')
chds.bn.stage.key[[4]]$pos <- c('w7','w8', 'w9','w7','w8','w9','w10','w11','w12','w10','w11','w12')
chds.bn.stage.key[[4]]$stage <- c('u7','u8','u9','u7','u8','u9','u10','u11','u12','u10','u11','u12')
renderCEG(chds.bn.stage.key,chds.df)

renderCEG(chds.stage.key,chds.df)
chds.sst1 <- CEG.AHC(chds.df[,c(1,2,4,3)]) #varorder S E H L 
chds.sst1$lik; #equivalent to chds.sst so same likelihoods 
chds.sst1.stage.key <- tostagekey(chds.df[,c(1,2,4,3)],sst = chds.sst1)
renderCEG(chds.sst1.sk, chds.df[,c(1,2,4,3)])

##COMPONENT MONITOR FOR AHC CEG 
map(chds.stage.key[-1], ~select(.x,-key))->chds.sk#a little bit of finagling 
chds.sk <- c(chds.stage.key[1],chds.sk)
map(chds.sk, ~select(.x,-pos))->chds.sk.nocol

chds.stage.key <-tostagekey(chds.df, chds.sst)

#df=chds.df;struct=chds.struct;cuts=chds.cuts;stage.key=chds.sk.nocol;stages=chds.stages#fortroubleshooting
chds.prior <- chds.sst$prior[which(!is.na(unlist(lapply(chds.sst$prior, '[[', 1))) == TRUE)]
chds.bn.prior <- chds.bn.sst$prior[which(!is.na(unlist(lapply(chds.bn.sst$prior, '[[', 1))) == TRUE)]#need to fix prior for this one
chds.sst1.prior <- chds.sst1$prior[which(!is.na(unlist(lapply(chds.sst1$prior, '[[', 1))) == TRUE)]

#component monitors for all three
chds.components <- format(round(component.monitor.sst(chds.sst),3),nsmall=3)
chds.bn.components <- format(round(component.monitor(chds.bn.data, chds.bn.prior),3),nsmall=3)
chds.sst1.components <- format(round(component.monitor.sst(chds.sst1),3),nsmall=3)

chds.components.df <- cbind(chds.stages,chds.components)
chds.bn.components.df <- cbind(chds.bn.stages,chds.bn.components)
chds.sst1.components.df <- cbind(chds.sst1.stages,chds.sst1.components)

library(stargazer)
stargazer(chds.components.df)
stargazer(chds.bn.components.df)
stargazer(chds.sst1.components.df)

##COMPONENT MONITOR FOR BN CEG 
map(chds.bn.stage.key[-1], ~select(.x,-key))->chds.bn.sk#a little bit of finagling 
chds.bn.sk <- c(chds.bn.stage.key [1],chds.bn.sk)
map(chds.bn.sk, ~select(.x,-pos))->chds.bn.sk.nocol
pull(map_df(chds.bn.stage.key, ~distinct(.x,stage)),stage )-> chds.bn.stages#the stages
chds.bn.prior <-get.ref.prior(df=chds.df,struct=chds.bn.struct,cuts=chds.cuts,stage.key=chds.bn.sk.nocol,stages=chds.bn.stages)#check that we can get the prior
chds.bn.data <- getdata(chds.df, chds.bn.sk.nocol)
chds.bn.data[[1]] <- chds.bn.sk.nocol[[1]]$n
chds.bn.components <- component.monitor(chds.bn.data, chds.bn.prior)

cbind(chds.bn.stages, chds.bn.components)

chds.prior <-get.ref.prior(df=chds.df,struct=chds.struct,cuts=chds.cuts,stage.key=chds.sk.nocol,stages=chds.stages)#check that we can get the prior
chds.data <- getdata(chds.df, chds.sk.nocol)
chds.data[[1]] <- c(507,383)x
chds.components <- component.monitor(chds.data,chds.prior)

####COMPONENT MONITOR FOR CEG C 
chds.sst1.stage.key <- tostagekey(chds.df[,c(1,2,4,3)],chds.sst1)
map(chds.sst1.stage.key[-1], ~select(.x,-key))->chds.sst1.sk#a little bit of finagling 
chds.sst1.sk <- c(chds.sst1.stage.key [1],chds.sst1.sk)
map(chds.sst1.sk, ~select(.x,-pos))->chds.sst1.sk.nocol
chds.sst1.struct <- to.struct(chds.df[,c(1,2,4,3)], chds.sst1.sk.nocol,chds.sst1)
pull(map_df(chds.sst1.stage.key, ~distinct(.x,stage)),stage )-> chds.sst1.stages#the stages
#chds.sst1.prior <-get.ref.prior(df=chds.df[,c(1,2,4,3)],struct=chds.sst1.struct,cuts=chds.cuts,stage.key=chds.sst1.sk.nocol,stages=chds.sst1.stages)#check that we can get the prior
#chds.sst1.data <- getdata(chds.df[,c(1,2,4,3)], chds.sst1.sk.nocol)
#chds.sst1.data[[1]] <- chds.sst1.sk.nocol[[1]]$n
#chds.sst1.components <- component.monitor(chds.sst1.data, chds.sst1.prior)


####PARTITION MONITORRRRR

#THIS TAKES OVER 12 HOURS. DO NOT RUN HERE. chds.part.monitor <- part.monitor(rho = .7,epsilon = 1.2,df_cut = chds.df,which.cut = 4,stage.key = chds.sk.nocol,n.monitor = 860)

chds.part.monitor <- part.monitor(rho = .7,epsilon = 1.2,df_cut = chds.df,which.cut = 3,stage.key = chds.sk.nocol,n.monitor = 860)
chds.bn.part.monitor <- part.monitor(rho = .7,epsilon = 1.2,df_cut = chds.df,which.cut = 3,stage.key = chds.bn.sk.nocol,n.monitor = 860)
chds.sst1.part.monitor <- part.monitor(rho = .7,epsilon = 1.2,df_cut = chds.df[,c(1,2,4,3)],which.cut = 3,stage.key = chds.sst1.sk.nocol,n.monitor = 860)

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

 
renderCEG(chds.stage.key, chds.df)

#Is there another staging of the situations in cut 3 that would be more appropriate? 
chds.part.monitor <- part.monitor(rho = .7,epsilon = 1.2,df_cut = chds.df,which.cut = 3,stage.key = chds.sk.nocol,n.monitor = 860)
#k is 0.9 in freeman paper
#tau is length of lag

#rho = .7;epsilon = 1.2;df_cut = chds.df;which.cut = 3;stage.key = chds.sk;n.monitor = 200#for trblshtng
########PLOT FOR THE AHC CEG 
chds.crrnt.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 4)) #possible.colorin 3
chds.alt1.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 1))#possible.coloring  10
chds.alt2.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 2))#possible.coloring 11
chds.alt3.stg.probs <- do.call("rbind", lapply(chds.part.monitor[[3]], "[[", 3))#possible.coloring 13


chds.part.df <- as.data.frame(cbind(5:860,chds.crrnt.stg.probs, chds.alt1.stg.probs, chds.alt2.stg.probs, chds.alt3.stg.probs))
colnames(chds.part.df) <- c("t", "U1=(1,2,3)(4)", "U2=(1,2)(3)(4)", "U3=(1,3)(2)(4)", "U4=(2,3)(1)(4)")
chds.part.df %>% 
  gather(key, value, -t) %>%
  ggplot(aes(x=t, y=value, colour=key)) + geom_line()

#########PLOT FOR THE BN CEG
chds.bn.crrnt.stg.probs <- do.call("rbind", lapply(chds.bn.part.monitor[[3]], "[[", 7)) #possible.coloring 15
chds.bn.alt1.stg.probs <- do.call("rbind", lapply(chds.bn.part.monitor[[3]], "[[", 1))#possible.coloring 9 
chds.bn.alt2.stg.probs <- do.call("rbind", lapply(chds.bn.part.monitor[[3]], "[[", 2))#possible.coloring 10
chds.bn.alt3.stg.probs <- do.call("rbind", lapply(chds.bn.part.monitor[[3]], "[[", 3))#possible.coloring 11
chds.bn.alt4.stg.probs <- do.call("rbind", lapply(chds.bn.part.monitor[[3]], "[[", 4))#possible.coloring12
chds.bn.alt5.stg.probs <- do.call("rbind", lapply(chds.bn.part.monitor[[3]], "[[", 5))#possible.coloring 13
chds.bn.alt6.stg.probs <- do.call("rbind", lapply(chds.bn.part.monitor[[3]], "[[", 6))#possible.coloring 14

possible.colorings[[3]] #(HH, HL, LH) (LL)
possible.colorings[[13]]#(HH) (HL, LH) (LL)

chds.bn.part.df <- as.data.frame(cbind(5:860,chds.bn.crrnt.stg.probs, chds.bn.alt1.stg.probs, chds.bn.alt2.stg.probs, chds.bn.alt3.stg.probs, chds.bn.alt4.stg.probs, chds.bn.alt5.stg.probs, chds.bn.alt6.stg.probs))
colnames(chds.bn.part.df) <- c("t", "U1=(1)(2)(3)(4)", "U2=(1,4)(2)(3)", "U3=(1,2)(3)(4)", "U4=(1,3)(2)(4)", "U5=(2,4)(1)(3)", "U6=(2,3)(1)(4)", "U7=(3,4)(1)(2)")
chds.bn.part.df %>% 
  gather(key, value, -t) %>%
  ggplot(aes(x=t, y=value, colour=key)) + geom_line()
#stagings
#9 (14)(2)(3)
#10 (12) (3) (4)
#11 (13)(2)(4)
#15* (1)(2)(3)(4)


#########


chds.w7w3.pach <- ceg.child.parent.monitor(df=chds.df,
                         target.stage = "u3",
                         target.cut = 3, 
                         condtnl.stage = "u1",
                         struct = chds.struct,
                         stage.key = chds.sk.nocol, 
                         stages=chds.stages,
                         prior=chds.prior,
                         n = 860,
                         learn = F)
chds.w7w3.pacht <- ceg.child.parent.monitor(df=chds.df,
                                           target.stage = "u3",
                                           target.cut = 3, 
                                           condtnl.stage = "u1",
                                           struct = chds.struct,
                                           stage.key = chds.sk.nocol, 
                                           stages=chds.stages,
                                           prior=chds.prior,
                                           n = 860,
                                           learn = T)
plot(chds.w7w3.pach[,1])
chds.w7w4.pach <- ceg.child.parent.monitor(df=chds.df,
                                           target.stage = "u3",
                                           target.cut = 3, 
                                           condtnl.stage = "u2",
                                           struct = chds.struct,
                                           stage.key = chds.sk.nocol, 
                                           stages=chds.stages,
                                           prior=chds.prior,
                                           n = 860,
                                           learn = F)
chds.w7w4.pacht <- ceg.child.parent.monitor(df=chds.df,
                                           target.stage = "u3",
                                           target.cut = 3, 
                                           condtnl.stage = "u2",
                                           struct = chds.struct,
                                           stage.key = chds.sk.nocol, 
                                           stages=chds.stages,
                                           prior=chds.prior,
                                           n = 860,
                                           learn = T)


chds.pach <- as.data.frame(cbind(1:860,chds.w7w3.pach[,1],chds.w7w3.pacht[,1],chds.w7w4.pach[,1],chds.w7w4.pacht[,1]))
chds.pach <- as.data.frame(cbind(1:860,chds.w7w3.pach[,2],chds.w7w3.pacht[,2],chds.w7w4.pach[,2],chds.w7w4.pacht[,2],chds.w7w5.pach[,2]))
colnames(chds.pach) <- c('t','U1 Reference prior, no learning','U1 Reference prior, learning','U2 Reference prior, no learning','U2 Reference prior, learning')

chds.pach[1:150,] %>%
  gather(key,value, -t) %>% 
  ggplot(aes(x=t,y=value,colour=key))+geom_line() + ylab('Cumulative logarithmic penalty')


########CHDS BN PACH MONITOR 
bn.pach.nl <- bn.parent.child.monitor(df = chds.df,'Social','High','Economic',n=150)
bn.pach.l <- bn.parent.child.monitor(df = chds.df,'Social','High','Economic',n=150,learn = T)

chds.bn.pach <- as.data.frame(cbind(1:length(bn.pach.l[,1]), bn.pach.nl[,1], bn.pach.l[,1]))
colnames(chds.bn.pach) <- c('t','Ref prior, no learning','Ref prior, learning')

chds.bn.pach %>%
  gather(key,value, -t) %>% 
  ggplot(aes(x=t,y=value,colour=key))+geom_line() + ylab('Cumulative logarithmic penalty')

########CHDS STAGE MONITORS  
pass.message(df = chds.df, stage.key = chds.sk.nocol, evidence = chds.df[1:10,],prior = chds.prior,struct = chds.struct,stages = chds.stages )
df = chds.df; stage.key = chds.sk.nocol; evidence = chds.df[1:10,];prior = chds.prior;struct = chds.struct;stages = chds.stages 


p.monitor1 <- do.call("rbind", lapply(p.monitor, "[[", 1))#possible.coloring 9 
p.monitor2 <- do.call("rbind", lapply(p.monitor, "[[", 2))#possible.coloring 10
p.monitor3 <- do.call("rbind", lapply(p.monitor, "[[", 3))#possible.coloring 11
p.monitor4 <- do.call("rbind", lapply(p.monitor, "[[", 4))#possible.coloring12
p.monitor5 <- do.call("rbind", lapply(p.monitor, "[[", 5))#possible.coloring 13
p.monitor6 <- do.call("rbind", lapply(p.monitor, "[[", 6))#possible.coloring 14
p.monitor7 <- do.call("rbind", lapply(p.monitor, "[[", 7))#possible.coloring 14
p.monitor8 <- do.call("rbind", lapply(p.monitor, "[[", 8))#possible.coloring 14
p.monitor9 <- do.call("rbind", lapply(p.monitor, "[[", 9))#possible.coloring 14
p.monitor10 <- do.call("rbind", lapply(p.monitor, "[[", 10))#possible.coloring 14
p.monitor11 <- do.call("rbind", lapply(p.monitor, "[[", 11))#possible.coloring 14
p.monitor12 <- do.call("rbind", lapply(p.monitor, "[[", 12))#possible.coloring 14
p.monitor13 <- do.call("rbind", lapply(p.monitor, "[[", 13))#possible.coloring 14


u5monitor <- as.data.frame(cbind(1:length(p.monitor1),p.monitor1,p.monitor2,p.monitor3,p.monitor4,p.monitor5,p.monitor6,p.monitor7,p.monitor8,p.monitor9,p.monitor10,p.monitor11,p.monitor12,p.monitor13))
u6monitor <- as.data.frame(cbind(1:length(p.monitor1),p.monitor1,p.monitor2,p.monitor3,p.monitor4,p.monitor5,p.monitor6,p.monitor7,p.monitor8,p.monitor9,p.monitor10,p.monitor11,p.monitor12,p.monitor13))
u7monitor <- as.data.frame(cbind(1:length(p.monitor1),p.monitor1,p.monitor2,p.monitor3,p.monitor4,p.monitor5,p.monitor6,p.monitor7,p.monitor8,p.monitor9,p.monitor10,p.monitor11,p.monitor12,p.monitor13))
colnames(u5monitor) <- c("t", '1,','2','3','4','5','6','7','8','9','10','11','12','13')
colnames(u6monitor) <- c("t", '1,','2','3','4','5','6','7','8','9','10','11','12','13')
colnames(u7monitor) <- c("t", '1,','2','3','4','5','6','7','8','9','10','11','12','13')
                               
u7monitor %>% 
  gather(key, value, -t) %>%
  ggplot(aes(x=t, y=value, colour=key)) + geom_line()
#stagings

#new BF stage monitors

chds.which.stage <- c(1,2,2,3,3,3,4,4,4)
expected.counts(chds.prior, chds.which.stage)
expctBF <-component.monitor(expct.cnts,chds.prior)
actualBF <-component.monitor(chds.data, chds.prior)
actualBF/expctBF

chi <- list()
for(i in 1:length(chds.data)){
  chi[[i]]<-(chds.data[[i]]-expct.cnts[[i]])^2/expct.cnts[[i]]
}

chds.prior <- chds.sst$prior[which(!is.na(unlist(lapply(chds.sst$prior, '[[', 1))) == TRUE)]
chds.loo.counts <- one.out.getdata(chds.df,chds.sk.nocol) #is missing the bit at the root node 
chds.loo.counts <- c(list(chds.data[[1]]), chds.loo.counts)#success 


loo.component.monitor <- function(loo.data, prior){###THIS IS
  loo.components <- c()
  for (i in 1:length(prior)){#for the number of stages 
    alpha <- unlist(prior[i])#the prior
    cmp.vec <- c()
    for (j in 1:length(loo.data[[i]])){
      N <- unlist(loo.data[[i]][j])
      cmp.vec[j] <- sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) - lgamma(sum(alpha + N)))
    }
    loo.components[i] <- sum(cmp.vec)
  }
  return(loo.components)
}

loo.component.monitor(chds.loo.counts, chds.prior);actualBF

