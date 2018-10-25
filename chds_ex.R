#CHDS
library(ggplot2)
library(tidyverse)
setwd("C:/Users/rachel/Documents/diagnostics/")
df<-read.csv(file = "CHDS.latentexample1.csv")
radical <- read.csv(file= "data1.csv")

##CHDS EXAMPLE
#this example looks at inputting the structure by hand.

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
df %>% count(Social, Economic, Events, Admission) -> cega.stage.key[[5]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
cega.stage.key[[1]]$stage <- c("cega.w0")
cega.stage.key[[2]]$stage <- c("cega.w1", "cega.w2")
cega.stage.key[[3]]$stage <- c("cega.w3", "cega.w4", "cega.w5", "cega.w6")
cega.stage.key[[4]]$stage <- c("cega.w8", "cega.w7", "cega.w9", "cega.w8", "cega.w7", "cega.w9", "cega.w9", "cega.w8", "cega.w9", "cega.w9", "cega.w8", "cega.w9")#this contains the structure
cega.stage.key[[5]]$stage <- rep("cega.winf", length(cega.stage.key[[5]]$n))

##THIS IS FOR CEG-c
cegc.stages <- list("cegc.w0", "cegc.w1", "cegc.w2", "cegc.w3", "cegc.w4", "cegc.w5", "cegc.w6", "cegc.w7", "cegc.w8", "cegc.w9")

cegc.w0 <- df %>% count(Economic)
cegc.w1 <- df %>% filter(Economic=="High") %>% count(Social)
cegc.w2 <- df %>% filter(Economic=="Low") %>% count(Social)
cegc.w3 <- df %>% filter(Economic=="High", Social=="High") %>% count(Events)
cegc.w4 <- df %>% filter(Economic=="High", Social=="Low") %>% count(Events)
cegc.w5 <- df %>% filter(Economic=="Low", Social=="High") %>% count(Events)
cegc.w6 <- df %>% filter(Economic=="Low", Social=="Low") %>% count(Events)
cegc.w7 <- df %>% filter((Economic=="High" & Social=="High"  & Events=="Low") |  (Economic=="High" & Social=="Low"  & Events=="Low")) %>% count(Admission)
cegc.w8 <- df %>% filter((Economic=="High" & Social=="High"  & Events=="Average") |
                           (Economic=="High" & Social=="Low"  & Events=="Average") |
                           (Economic=="Low" & Social=="High"  & Events=="Low") |
                           (Economic=="Low" & Social=="Low"  & Events=="Low")) %>% count(Admission)
cegc.w9 <- df %>% filter((Economic=="Low" & Social=="High"  & Events=="Average") |
                           (Economic=="Low" & Social=="High"  & Events=="High") |
                           (Economic=="High" & Social=="High"  & Events=="High") |
                           (Economic=="High" & Social=="Low"  & Events=="High") |
                           (Economic=="Low" & Social=="Low"  & Events=="High") |
                           (Economic=="Low" & Social=="Low"  & Events=="Average")) %>% count(Admission)
cegc.struct <- list(cegc.w0, cegc.w1, cegc.w2, cegc.w3, cegc.w4, cegc.w5, cegc.w6, cegc.w7, cegc.w8, cegc.w9)#this is the observed values for each of the stages

#can initalize this, and prompt user to input the pathways for the particular tree
cegc.stage.key <- list()
count(df) -> cegc.stage.key[[1]]
df %>% count(Economic) -> cegc.stage.key[[2]]
df %>% count(Economic, Social) -> cegc.stage.key[[3]]
df %>% count(Economic, Social, Events) -> cegc.stage.key[[4]]
df %>% count(Economic, Social, Events, Admission) -> cegc.stage.key[[5]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
cegc.stage.key[[1]]$stage <- c("cegc.w0")
cegc.stage.key[[2]]$stage <- c("cegc.w1", "cegc.w2")
cegc.stage.key[[3]]$stage <- c("cegc.w3", "cegc.w4", "cegc.w5", "cegc.w6")
cegc.stage.key[[4]]$stage <- c("cegc.w8", "cegc.w7", "cegc.w9", "cegc.w8", "cegc.w7", "cegc.w9", "cegc.w9", "cegc.w8", "cegc.w9", "cegc.w9", "cegc.w8", "cegc.w9")#this contains the structure
cegc.stage.key[[5]]$stage <- rep("cegc.winf", length(cegc.stage.key[[5]]$n))


#######################################CEG B  
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
df %>% count(Social, Economic, Events,Admission) -> cegb.stage.key[[5]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
cegb.stage.key[[1]]$stage <- c("cegb.w0")##THIS MUST START AT 0 AND NUM MUST BE LAST CHAR
cegb.stage.key[[2]]$stage <- c("cegb.w1", "cegb.w2")
cegb.stage.key[[3]]$stage <- c("cegb.w3", "cegb.w3", "cegb.w4", "cegb.w5")
cegb.stage.key[[4]]$stage <- c("cegb.w7", "cegb.w8","cegb.w6","cegb.w7",
                               "cegb.w8","cegb.w6","cegb.w7","cegb.w8",
                               "cegb.w7","cegb.w8","cegb.w8","cegb.w7")#this contains the structure
cegb.stage.key[[5]]$stage <-rep("cegb.winf", length(cegb.stage.key[[5]]$n))
cegb.stages <- list("cegb.w0", "cegb.w1", "cegb.w2", "cegb.w3", "cegb.w4", "cegb.w5", "cegb.w6", "cegb.w7", "cegb.w8")


stages=cegb.stages; struct=cegb.struct; stage.key=cegb.stage.key
stages=cega.stages; struct=cega.struct; stage.key=cega.stage.key

##############

ceg.pachL <- ceg.child.parent.monitor(df, target.stage="cega.w3",condtnl.stage = "cega.w1",
                                                       target.cut=3,stages=cega.stages,
                                                       stage.key=cega.stage.key,struct=cega.struct,n=500,learn = TRUE)
ceg.pach <- ceg.child.parent.monitor(df, target.stage="cega.w3",condtnl.stage = "cega.w1",
                                                       target.cut=3,stages=cega.stages,
                                                       stage.key=cega.stage.key,struct=cega.struct,n=500,learn = FALSE)
cegC.pachL <- ceg.child.parent.monitor(df, target.stage="cega.w3",condtnl.stage = "cega.w1",
                                                       target.cut=3,stages=cegc.stages,
                                                       stage.key=cegc.stage.key,struct=cegc.struct,n=500,learn = TRUE)
cegC.pach <- ceg.child.parent.monitor(df, target.stage="cega.w3",condtnl.stage = "cega.w1",
                                                       target.cut=3,stages=cegc.stages,
                                                       stage.key=cegc.stage.key,struct=cegc.struct,n=500,learn = FALSE)

bn.a.pach<-bn.parent.child.monitor(df,"Social","High","Economic",n=200)
ceg.pachL <- ceg.child.parent.monitor(df, target.stage="cega.w0",condtnl.stage = "cega.w1",
                                                       target.cut=2,stages=cega.stages,
                                                       stage.key=cega.stage.key,struct=cega.struct,n=50,learn = TRUE)
ceg.a.pach <- ceg.child.parent.monitor(df, target.stage="cega.w1",condtnl.stage = "cega.w0",#this doesn't work for the first stage, but this is equivalent to the BN monitor 
                                           target.cut=2,stages=cega.stages,
                                           stage.key=cega.stage.key,struct=cega.struct,n=200,learn = FALSE)
#w0 -> w1 and w0 -> w2 are equivalent to the BN monitor here.

ceg.pach$p <- exp(-log(ceg.pach$Sm))
brier <- function(x){
  x <- na.exclude(x)
  x<- x[!is.infinite(x)]
  y <- rep(0,length(x))
  for (i in 1:length(x)){
    p = x[i]
    y[i]<-2*p-sum((x[1:i])^2) - 1
  }
  return(y)
}

spherical <- function(x,alpha=2) {
  x <- na.exclude(x)
  x <- x[!is.infinite(x)]
  y <- rep(0,length(x))
  for(i in 1:length(x)){
    p = x[i]
    y[i] = (p^(alpha-1))/((sum(x[1:i]))^((alpha-1)/alpha))
  }
}

ggplot(ceg.pachL, aes(x=1:500,y=Sm,color='With Learning')) + 
  geom_line() + 
  geom_line(data= ceg.pach, aes( color= 'Without Learning')) +
  ggtitle('Parent child monitor for stage w3 | w1') + 
  xlab('Relevant Sample Size') + ylab('Cumulative Log Penalty') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/chdscegpach.jpeg')

ceg.pachL$Em <- exp(-ceg.pachL$Sm)*ceg.pachL$Sm
ceg.pachL$Vm <- exp(-ceg.pachL$Sm)*(ceg.pachL$Sm)^2 -(ceg.pachL$Em)^2
ceg.pachL$Zm <- (ceg.pachL$Sm - ceg.pachL$Em )/sqrt(ceg.pachL$Vm)

ceg.pach$Em <- exp(-ceg.pach$Sm)*ceg.pach$Sm
ceg.pach$Vm <- exp(-ceg.pach$Sm)*(ceg.pach$Sm)^2 - (ceg.pach$Em)^2
ceg.pach$Zm <- (ceg.pach$Sm - ceg.pach$Em )/sqrt(ceg.pach$Vm)

ggplot(ceg.pachL[1:10,], aes(x=1:10,y=Zm,color='With Learning')) + 
  geom_line() + 
  geom_line(data= ceg.pach[1:10,], aes(color= 'Without Learning')) +
  ggtitle('Floret Monitor for stage w3 | w1') + 
  xlab('Relevant Sample Size') + ylab('Standardized Test Statistic') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/chdscegpachz.jpeg')

which.cut=2
possible.colorings <- listParts(dim(stage.key[[which.cut]])[1])##removin the one where no one gets a color
chds.social <- one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=2,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 1)
chds.social2 <- one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=2,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 2)
df.s <- data.frame(cbind(x=1:200, -log(chds.social), -log(chds.social2))); head(df.s)
ggplot(df.s[-c(1:6),], aes(x, y=V2, color='S(1)')) + 
  geom_line() + 
  geom_line(aes(x, y=V3, color='S(2)')) + 
  ggtitle('CEG Parent Child Monitor') + 
  xlab('Relevant Sample Size') + ylab('Cumulative Log Penalty') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/chdsStageMonitorS.jpeg')


possible.colorings <- listParts(dim(cega.stage.key[[which.cut]])[2])##removin the one where no one gets a color
chds.econ <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 15))
chds.econ2 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 14))
chds.econ3 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 13))
chds.econ4 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 12))
chds.econ5 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 11))
chds.econ6 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 10))

chdsB.econ <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cegb.stage.key, n.monitor=200, crrnt.stg = 15))
chdsB.econ2 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cegb.stage.key, n.monitor=200, crrnt.stg = 14))
chdsB.econ3 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cegb.stage.key, n.monitor=200, crrnt.stg = 13))
chdsB.econ4 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cegb.stage.key, n.monitor=200, crrnt.stg = 12))
chdsB.econ5 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cegb.stage.key, n.monitor=200, crrnt.stg = 11))
chdsB.econ6 <- log(one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cegb.stage.key, n.monitor=200, crrnt.stg = 10))


df.e <- data.frame(cbind(1:200,chds.econ,chds.econ2, chds.econ3, chds.econ4, chds.econ5,chds.econ6))
ggplot(df.e[-c(1:4),], aes(V1, y=chds.econ, color='S(15)')) + 
  geom_point() + 
  geom_point(aes(V1, y=chds.econ2, color='S(14)')) + 
  geom_point(aes(V1, y=chds.econ3, color='S(13)')) + 
  geom_point(aes(V1, y=chds.econ4, color='S(12)')) + 
  geom_point(aes(V1, y=chds.econ5, color='S(11)')) + 
  geom_point(aes(V1, y=chds.econ6, color='S(10)')) + 
  ggtitle('CEG Parent Child Monitor') + 
  xlab('Relevant Sample Size') + ylab('Weighted Cumulative Log Penalty') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/chdsStageMonitorEweighted.jpeg')

df2.e <- data.frame(cbind(1:200,chdsB.econ,chdsB.econ2, chdsB.econ3, chdsB.econ4,chdsB.econ5,chdsB.econ6))
ggplot(df2.e[-c(1:4),], aes(V1, y=chdsB.econ, color='S(15)')) + 
  geom_point() + 
  geom_point(aes(V1, y=chdsB.econ2, color='S(14)')) + 
  geom_point(aes(V1, y=chdsB.econ3, color='S(13)')) + 
  geom_point(aes(V1, y=chdsB.econ4, color='S(12)')) + 
  geom_point(aes(V1, y=chdsB.econ5, color='S(11)')) + 
  geom_point(aes(V1, y=chdsB.econ6, color='S(10)')) + 
  ggtitle('CEG Parent Child Monitor') + 
  xlab('Relevant Sample Size') + ylab('Cumulative Log Penalty') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/chdsStageMonitorE.jpeg')

possible.colorings <- listParts(dim(stage.key[[which.cut]])[2])##removin the one where no one gets a color
chds.life <- one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 15)
chds.life2 <- one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df,which.cut=3,stage.key=cega.stage.key, n.monitor=200, crrnt.stg = 6)
chds.lifeC <- one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df[,c(2,1,3,4)],which.cut=3,stage.key=cegc.stage.key, n.monitor=200, crrnt.stg = 15)
chds.lifeB <- one.step.forecast(rho=0.8, epsilon=1.2, df_cut=df[,c(1,2,3,4)],which.cut=3,stage.key=cegb.stage.key, n.monitor=200, crrnt.stg = 10)

pass.message(df,cega.stage.key,evidence = df[55,c(1,3)],post.mean,cega.prior)
prob.w7 <- c();prob.w8 <- c();prob.w9 <- c()
prob.w1 <- c(); prob.w2 <- c();
prob.w3 <- c(); prob.w4 <- c();prob.w5 <- c();

function(df=df,stage.key=cega.stage.key,ev,post.mean,ceg.prior=cega.prior,n=20,stg.idx=3){
  brier <- c(); 
  for (i in 1:n){
    
    #filter so that we're just looking at the added in paths 
    updated.ev <- pass.message(df, ceg.stage.key,ev,post.mean,cega.prior)
    prob=updated.ev[[stg.idx]]
    actual <- rep(0,length(updated.ev[[stg.idx]]))
    actual[as.numeric(as.character(df[i,]))[stg.idx]] <- 1
    brier <- c(brier,sum(prob-actual)^2)
  }
  return(sum(brier))
}
brier<-c();
for (i in 1:500){
    if(df[i,1]!="High"){next}
  if(df[i,2]!="High"){next}
  updated.ev <- pass.message(df,cega.stage.key,evidence = df[i,c(1:2)],post.mean,cega.prior)
  prob <- updated.ev.2[[4]]
  actual <- rep(0,length(updated.ev[[5]]))#; df_sub <-filter(df[1:500,],Social=="High")
  actual[as.numeric(as.character(df[i,]))[3]] <- 1
  brier <- c(brier, sum((prob-actual)^2))
}

hist(prob.w3)
hist(brier)
df[1:500,] %>% 
  filter(Social=="High")

actualw1 <- rep(0,500)
actualw1[which(df[1:500,1]==levels(df[,2])[1])]<- 1
actualw2 <- rep(0,500)
actualw2[which(df[1:500,1]==levels(df[,2])[1])]<- 1


sum((prob.w1-actual)^2)
