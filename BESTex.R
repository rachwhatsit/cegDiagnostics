library(ceg)
library(plyr)
library(tidyverse)
library(reshape2)
#df.orig <- read.csv("180112_BEST_EQ5D3L_ALLTIMEPOINTS.csv",stringsAsFactors = F)
df.orig <- read.csv("180112_BEST_EQ5D3L_ALLTIMEPOINTS_TX.csv", stringsAsFactors = F)


df <- df.orig[-which(df.orig==-99,arr.in=TRUE),]#remove 99s
df <- df[-which(is.na(df)==TRUE),]#remove incompletes
#df[which(df==3,arr.ind = TRUE)]<- 2 #binned into any problems
#binning only necessary for rodriguo's code
df <- as.data.frame(lapply(df, factor))#factorized  
df <- CheckAndCleanData(df)
df0.wcode <- df[,c(2:6,22)] #initial time point
df3.wcode<- df[,c(7:11,22)] #3 month time point
df6.wcode<- df[,c(12:16,22)] #6 month time point
df12.wcode<- df[,c(17:21,22)] # 12 month time point

# ordering
ordering <- c(2,4,3,5,1)
df0.am <- df0.wcode[which(df0.wcode$randcode=="am"),ordering]
df0.amcbt <- df0.wcode[-which(df0.wcode$randcode=="am"),ordering]
df3.am <- df0.wcode[which(df3.wcode$randcode=="am"),ordering]
df3.amcbt <- df0.wcode[-which(df3.wcode$randcode=="am"),ordering]
df6.am <- df0.wcode[which(df6.wcode$randcode=="am"),ordering]
df6.amcbt <- df0.wcode[-which(df6.wcode$randcode=="am"),ordering]
df12.am <- df12.wcode[which(df12.wcode$randcode=="am"),ordering]
df12.amcbt <- df12.wcode[-which(df12.wcode$randcode=="am"),ordering]

#segmenting them according to characteristics
colnames(df0.am) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df0.am, vars=colnames(df0.am))->states0
colnames(df3.am) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df3.am, vars=colnames(df3.am))->states3
colnames(df6.am) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df6.am, vars=colnames(df6.am))->states6
colnames(df12.am) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df12.am, vars=colnames(df12.am))->states12

states0$state <- paste0(states0$selfcare, states0$pain, states0$activity, states0$anxiety, states0$mobility)
states3$state <- paste0(states3$selfcare, states3$pain, states3$activity, states3$anxiety, states3$mobility)
states6$state <- paste0(states6$selfcare, states6$pain, states6$activity, states6$anxiety, states6$mobility)
states12$state <- paste0(states12$selfcare, states12$pain, states12$activity, states12$anxiety, states12$mobility)

states0 <- states0[,-c(1:5)]
states3 <- states3[,-c(1:5)]
states6 <- states6[,-c(1:5)]
states12 <- states12[,-c(1:5)]
states.sum<- full_join(full_join(full_join(states0, states3, by='state'),states6,by='state'),states12,by='state')
states.sum<- states.sum[,c(2,1,3,4,5)]
colnames(states.sum) <- c('state','m0','m3','m6','m12')

#same for the cbt group
colnames(df0.amcbt) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df0.amcbt, vars=colnames(df0.amcbt))->statescbt0
colnames(df3.amcbt) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df3.amcbt, vars=colnames(df3.amcbt))->statescbt3
colnames(df6.amcbt) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df6.amcbt, vars=colnames(df6.amcbt))->statescbt6
colnames(df12.amcbt) <- c('selfcare','pain','activity','anxiety','mobility'); plyr:::count(df12.amcbt, vars=colnames(df12.amcbt))->statescbt12

statescbt0$state <- paste0(statescbt0$selfcare, statescbt0$pain, statescbt0$activity, statescbt0$anxiety, statescbt0$mobility)
statescbt3$state <- paste0(statescbt3$selfcare, statescbt3$pain, statescbt3$activity, statescbt3$anxiety, statescbt3$mobility)
statescbt6$state <- paste0(statescbt6$selfcare, statescbt6$pain, statescbt6$activity, statescbt6$anxiety, statescbt6$mobility)
statescbt12$state <- paste0(statescbt12$selfcare, statescbt12$pain, statescbt12$activity, statescbt12$anxiety, statescbt12$mobility)

statescbt0 <- statescbt0[,-c(1:5)]
statescbt3 <- statescbt3[,-c(1:5)]
statescbt6 <- statescbt6[,-c(1:5)]
statescbt12 <- statescbt12[,-c(1:5)]
statescbt.sum<- full_join(full_join(full_join(statescbt0, statescbt3, by='state'),statescbt6,by='state'),statescbt12,by='state')
statescbt.sum<- statescbt.sum[,c(2,1,3,4,5)]
colnames(statescbt.sum) <- c('state','m0','m3','m6','m12')

#make plots of individuals
head(df)
selfcare <- df[,c(1,3,8,13,18,22)]  
pain <- df[,c(1,2,7,12,17,22)]
activity <- df[,c(1,4,9,14,19,22)]
anxiety <- df[,c(1,5,10,15,20,22)]
mobility <- df[,c(1,6,11,16,21,22)]

selfcare.m <- melt(selfcare, id.vars=c('ID','randcode'));selfcare.m$value <-as.numeric(selfcare.m$value)
pain.m <- melt(pain, id.vars=c('ID','randcode'));pain.m$value <-as.numeric(pain.m$value)
activity.m <- melt(activity, id.vars=c('ID','randcode'));activity.m$value <-as.numeric(activity.m$value)
anxiety.m <- melt(anxiety, id.vars=c('ID','randcode'));anxiety.m$value <-as.numeric(anxiety.m$value)
mobility.m <- melt(mobility, id.vars=c('ID','randcode'));mobility.m$value <-as.numeric(mobility.m$value)

ggplot(selfcare.m, aes(x = variable, y = jitter(value))) + geom_line(aes(color = randcode, group = ID,alpha=.7)) + ggtitle('Selfcare')
ggplot(pain.m, aes(x = variable, y = jitter(value))) + geom_line(aes(color = randcode, group = ID,alpha=.7)) + ggtitle('Pain')
ggplot(activity.m, aes(x = variable, y = jitter(value))) + geom_line(aes(color = randcode, group = ID,alpha=.7)) + ggtitle('Activity')
ggplot(anxiety.m, aes(x = variable, y = jitter(value))) + geom_line(aes(color = randcode, group = ID,alpha=.7)) + ggtitle('Anxiety')
ggplot(mobility.m, aes(x = variable, y = jitter(value))) + geom_line(aes(color = randcode, group = ID,alpha=.7)) + ggtitle('Mobility')

#want to show the changes over time
vapply(selfcare[,c(2:5)],as.numeric(as.character()))
d3 <- as.numeric(selfcare$euroqol2_3m)-as.numeric(selfcare$euroqol2_b)


#parallel coordinates will scale the nodes together
#I'd like to retian some notion of what it is that they do differently...
#what kind of monitor can we show for a dynamic CEG? the same as the regular CEG, I guess?


#import Jane's code here
df0.am.sst <- jCEG.AHC(df0.am,priorN = 2)
df0.amcbt.sst <- jCEG.AHC(df0.amcbt)
df3.am.sst <- jCEG.AHC(df3.am)
df3.amcbt.sst <- jCEG.AHC(df3.amcbt)
df6.am.sst <- jCEG.AHC(df6.am)
df6.amcbt.sst <- jCEG.AHC(df6.amcbt)
df12.am.sst <- jCEG.AHC(df12.am)
df12.amcbt.sst <- jCEG.AHC(df12.amcbt)

#GLOBAL MONITORS FOR IDLE SYSTEM
df0.am.sst$lik  
df3.am.sst$lik  
df6.am.sst$lik  
df12.am.sst$lik  #change in BF here

#GLOBAL MONITORS FOR CONTROLLED SYSTEM
df0.amcbt.sst$lik
df3.amcbt.sst$lik
df6.amcbt.sst$lik
df12.amcbt.sst$lik #similarly, a change in BF here

df0.am.sst$stages
df3.am.sst$stages
df6.am.sst$stages
df12.am.sst$stages#change in stage structure, stage 21 combined with 16

df0.amcbt.sst$stages
df3.amcbt.sst$stages
df6.amcbt.sst$stages #check the partitions stages for the last cut, particularly 22 and 28 
df12.amcbt.sst$stages #changes to two different stages. 3 and 4 separated here, and 22 is consolidated, 28 separated

#PARENT CHILD MONITORS FOR IDLE AND CONTROLLED SYSTEMS
#component scores for each time series
am.bf <- as.data.frame(cbind(unlist(df0.am.sst$pach), unlist(df3.am.sst$pach), unlist(df6.am.sst$pach), unlist(df12.am.sst$pach)))
amcbt.bf <- as.data.frame(cbind(unlist(df0.amcbt.sst$pach), unlist(df3.amcbt.sst$pach), unlist(df6.amcbt.sst$pach), unlist(df12.amcbt.sst$pach)))
library(gridExtra)
pdf("ambf.pdf", height=11, width=8.5)
grid.table(am.bf)
dev.off()
pdf("amcbt.pdf", height=11, width=8.5)
grid.table(amcbt.bf)
dev.off()


#convert from sst output to stage.key to use partition monitor 
make.stage.key <- function(df,sst){#require(tidyverse)
  stage.key <- list()
  count(df) -> stage.key[[1]]#number at root node
  for (i in 2:sst$no.vars){
    count(df, UQ(sym(colnames(df)[1:i])))-> stage.key[[i]]
  }
  u.stage <- paste0(rep('u',length(sst$stages)), as.character(1:length(sst$stages))) #listed stages here
  
  stage.no <- 1
  currentcut 
  for (i in 1:length(sst$stages)){
    sst$result[[2]]
    
  }
  return(stage.key)
}

##ugh, give up on converting to the stage.key 
stage.key <- list()
count(df0.am) -> stage.key[[1]]
df0.am %>% count(euroqol2_b) -> stage.key[[2]]
df0.am %>% count(euroqol2_b, euroqol4_b) -> stage.key[[3]]
df0.am %>% count(euroqol2_b, euroqol4_b, euroqol3_b) -> stage.key[[4]]
df0.am %>% count(euroqol2_b, euroqol4_b, euroqol3_b, euroqol5_b) -> stage.key[[5]]
df0.am %>% count(euroqol2_b, euroqol4_b, euroqol3_b, euroqol5_b, euroqol1_b) -> stage.key[[6]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
stage.key[[1]]$stage <- c("u1")
stage.key[[2]]$stage <- c("u2", "u2")
stage.key[[3]]$stage <- c("u3", "u4", "u5")
stage.key[[4]]$stage <- c("u6", "u7", "u7", "u8", "u7", "u7")#this contains the structure
stage.key[[5]]$stage <- c("u10","u10","u10","u10","u11","u12","u9","u11","u10")
stage.key[[6]]$stage <- rep("cega.winf", length(stage.key[[6]]$n))
#PARTITION MONITOR


#NODE MONITORS



