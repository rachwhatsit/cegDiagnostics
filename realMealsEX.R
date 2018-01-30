library(ceg);library(deal)
library(tidyr);library(tidyverse)
library(rlang);library(partitions)

#the data
df <- read.csv("df.net.w.names.csv")

colnames(df)

df <- df[-which(is.na(df$Total.Meals)==T),c(9,11,18,19)]
df <- df[-which(is.na(df$Public.Transport)==T),]
df$Total.Meals[which(df$Total.Meals=="Med")] <- "High"
df$Total.Meals <- factor(df$Total.Meals)
summary(df)

#find the BN
fit <- network(df)
bn.prior <- jointprior(fit,600)
getnetwork(learn(fit,df,bn.prior))->bn.fit
bn.hisc <- autosearch(bn.fit,df,bn.prior,trace=F,removecycles = T)
plot(getnetwork(bn.hisc))

#change the
bn.adj <- getnetwork(remover(bn.hisc,2,1,bn.hisc,bn.prior))
bn.adj <- getnetwork(insert(bn.adj,1,2,bn.hisc,bn.prior))
rats.nw3 <- getnetwork(insert(rats.nw2,1,2,rats,rats.prior))

#find the CEG
df <- CheckAndCleanData(df)
ceg:::ContingencyTable(stratified.event.tree = set, data = df)
set <- Stratified.event.tree(df)
plot(set)
prior <- ceg:::PriorDistribution(set,3)

sst <- Stratified.staged.tree(df)
plot(sst)

meals.ceg <- ceg:::Stratified.ceg.model(stratified.staged.tree = sst)
plot(meals.ceg)

#find the diagnostics of the fit structure
#first desribe the structure of the CEG 
cega.stage.key <- list()
count(df) -> cega.stage.key[[1]]
df %>% count(SFSP.Kitchens) -> cega.stage.key[[2]]
df %>% count(SFSP.Kitchens, Media) -> cega.stage.key[[3]]
df %>% count(SFSP.Kitchens, Media, Public.Transport) -> cega.stage.key[[4]]
df %>% count(SFSP.Kitchens, Media, Public.Transport, Total.Meals) -> cega.stage.key[[5]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
cega.stage.key[[1]]$stage <- c("cega.w0")
cega.stage.key[[2]]$stage <- c("cega.w1", "cega.w2")
cega.stage.key[[3]]$stage <- c("cega.w3", "cega.w4", "cega.w5", "cega.w5")
cega.stage.key[[4]]$stage <- c("cega.w6", "cega.w7", "cega.w7", "cega.w8", "cega.w6", "cega.w6", "cega.w6")#this contains the structure
cega.stage.key[[5]]$stage <- rep("cega.winf", length(cega.stage.key[[5]]$n))

cega.stages <- list("cega.w0", "cega.w1", "cega.w2", "cega.w3", "cega.w4", "cega.w5", "cega.w6", "cega.w7", "cega.w8")

cega.w0 <- df %>% count(SFSP.Kitchens)
cega.w1 <- df %>% filter(SFSP.Kitchens=="No") %>% count(Media)
cega.w2 <- df %>% filter(SFSP.Kitchens=="Yes") %>% count(Media)
cega.w3 <- df %>% filter(SFSP.Kitchens=="No", Media=="Low") %>% count(Public.Transport)
cega.w4 <- df %>% filter(SFSP.Kitchens=="No", Media=="High") %>% count(Public.Transport)
cega.w5 <- df %>% filter((SFSP.Kitchens=="Yes"& Media=="High") | (SFSP.Kitchens=="Yes"& Media=="Low")) %>% count(Public.Transport)
cega.w6 <- df %>% filter((SFSP.Kitchens=="No"& Media=="High" & Public.Transport=="No") |
                           (SFSP.Kitchens=="Yes"& Media=="High" & Public.Transport=="No") |
                           (SFSP.Kitchens=="Yes"& Media=="Low" & Public.Transport=="No") |
                           (SFSP.Kitchens=="Yes"& Media=="Low" & Public.Transport=="Yes") ) %>% count(Total.Meals)
cega.w7 <- df %>% filter((SFSP.Kitchens=="No" & Media=="High"  & Public.Transport=="Yes") |  
                           (SFSP.Kitchens=="No" & Media=="Low"  & Public.Transport=="No")) %>% count(Total.Meals)
cega.w8 <- df %>% filter((SFSP.Kitchens=="No" & Media=="Low"  & Public.Transport=="Yes")) %>% count(Total.Meals)
cega.struct <- list(cega.w0, cega.w1, cega.w2, cega.w3, cega.w4, cega.w5, cega.w6, cega.w7, cega.w8)#this is the observed values for each of the stages

##now run the diagnostics
#show the learning in the dots
ceg.child.parent.monitor(df,"cega.w1",2,"cega.w0",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach01;plot(pach01[,1])
ceg.child.parent.monitor(df,"cega.w1",2,"cega.w0",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach01;lines(pach01[,1])
ceg.child.parent.monitor(df,"cega.w2",2,"cega.w0",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach02;plot(pach02[,1])
ceg.child.parent.monitor(df,"cega.w2",2,"cega.w0",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach02;lines(pach02[,1])
ceg.child.parent.monitor(df,"cega.w3",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach13;plot(pach13[,1])
ceg.child.parent.monitor(df,"cega.w3",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach13;lines(pach13[,1])
ceg.child.parent.monitor(df,"cega.w4",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach14;plot(pach14[,1])
ceg.child.parent.monitor(df,"cega.w4",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach14;lines(pach14[,1])
ceg.child.parent.monitor(df,"cega.w5",3,"cega.w2",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach25;plot(pach25[,1])
ceg.child.parent.monitor(df,"cega.w5",3,"cega.w2",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach25;lines(pach25[,1])
ceg.child.parent.monitor(df,"cega.w7",4,"cega.w3",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach37;plot(pach25[,1])
ceg.child.parent.monitor(df,"cega.w7",4,"cega.w3",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach37;lines(pach25[,1])
ceg.child.parent.monitor(df,"cega.w8",4,"cega.w4",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach48
plot(pach48[,1])

#help for troubleshooting
stages=cega.stages; struct=cega.struct; stage.key=cega.stage.key;target.stage="cega.w8";target.cut=4;condtnl.stage="cega.w4"

#
bnTl <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media","Public.Transport"),parent.values = c("No","Low","Yes"),child = "Public.Transport",n=500,learn=F)
plot(bnTl[,1])
bnT <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media","Public.Transport"),parent.values = c("No","High","Yes"),child = "Public.Transport",n=500,learn=T)
lines(bnT[,1])

#run the one step ahead prediction forecasts 
which.cut=4
possible.colorings <- listParts(dim(stage.key[[which.cut]])[1])##removin the one where no one gets a color#to get current stage,
xM <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 2,stage.key = cega.stage.key,n.monitor = 50,crrnt.stg = 2);plot(xM)
xT <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 3,stage.key = cega.stage.key,n.monitor = 50,crrnt.stg = 14);plot(xT)
#to do: figure out what staging is the right one when we have lots of partitions to search 
xmls <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 4,stage.key = cega.stage.key,n.monitor = 50,crrnt.stg = 14);plot(xmls)
