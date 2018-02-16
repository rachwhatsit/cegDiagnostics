library(ceg);library(deal)
library(tidyr);library(tidyverse)
library(rlang);library(partitions)

#the data
df <- read.csv("df.net.w.names.csv")

colnames(df)

df <-df[-which(is.na(df$Total.Meals)==T),c(9,11,18,19)]
df <- df[-which(is.na(df$Public.Transport)==T),]
df$Total.Meals[which(df$Total.Meals=="Med")] <- "High"
df$Total.Meals <- factor(df$Total.Meals)
summary(df)

#find the BN
fit <- network(df)
bn.prior <- jointprior(fit,600)
getnetwork(learn(fit,df,bn.prior))->bn.fit
bn.hisc <- autosearch(bn.fit,df,bn.prior,trace=F,removecycles = T)
bn <- getnetwork(bn.hisc)
plot(bn)


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
ceg.child.parent.monitor(df,"cega.w3",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach13l;plot(pach13l[,1])
ceg.child.parent.monitor(df,"cega.w3",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach13;lines(pach13[,1])
ceg.child.parent.monitor(df,"cega.w4",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach14;plot(pach14[,1])
ceg.child.parent.monitor(df,"cega.w4",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach14;lines(pach14[,1])
ceg.child.parent.monitor(df,"cega.w5",3,"cega.w2",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach25;plot(pach25[,1])
ceg.child.parent.monitor(df,"cega.w5",3,"cega.w2",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach25;lines(pach25[,1])
ceg.child.parent.monitor(df,"cega.w7",4,"cega.w3",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach37l;plot(pach37l[,1])
ceg.child.parent.monitor(df,"cega.w7",4,"cega.w3",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach37;lines(pach37[,1])
ceg.child.parent.monitor(df,"cega.w8",4,"cega.w4",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach48;plot(pach48[,1])
ceg.child.parent.monitor(df,"cega.w8",4,"cega.w4",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach48;lines(pach48[,1])

library(ggplot2)
ggplot(pach37l, aes(x=1:500,y=Sm,color='With Learning')) + 
  geom_line() + 
  geom_line(data= pach37, aes(color= 'Without Learning')) +
  ggtitle('Floret Monitor for stage w7 | w3') + 
  xlab('') + ylab('Cumulative Log Penalty') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/pachw73.jpeg')

pach37l$Em <- exp(-pach37l$Sm)*pach37l$Sm
pach37l$Vm <- exp(-pach37l$Sm)*(pach37l$Sm)^2 -(pach37l$Em)^2
pach37l$Zm <- (pach37l$Sm - pach37l$Em )/sqrt(pach37l$Vm)

pach37$Em <- exp(-pach37$Sm)*pach37$Sm
pach37$Vm <- exp(-pach37$Sm)*(pach37$Sm)^2 - (pach37$Em)^2
pach37$Zm <- (pach37$Sm - pach37$Em )/sqrt(pach37$Vm)

ggplot(pach37l[1:150,], aes(x=1:150,y=Zm,color='With Learning')) + 
  geom_line() + 
  geom_line(data= pach37[1:150,], aes(color= 'Without Learning')) +
  ggtitle('Floret Monitor for stage w7 | w3') + 
  xlab('') + ylab('Standardized Test Statistic') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/pachw73z.jpeg')


bnTl <-bn.parent.child.monitor(df,parents=c("Public.Transport"),parent.values = c("No"),child = "Total.Meals",n=500,learn=T)
bnT <-bn.parent.child.monitor(df,parents=c("Public.Transport"),parent.values = c("No"),child = "Total.Meals",n=500,learn=F)

ggplot(bnTl[1:150,],aes(1:150,y=Sm,color='With Learning'))+
  geom_line()+ 
  geom_line(data=bnT[1:150,], aes(color='Without Learning')) + 
  ggtitle('BN Parent Child Monitor') + 
  xlab('') + ylab('Cumulative Log Penalty') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/BNT.jpeg')
#help for troubleshooting
stages=cega.stages; struct=cega.struct; stage.key=cega.stage.key;target.stage="cega.w8";target.cut=4;condtnl.stage="cega.w4"
#line for troubleshooting
parents=c("SFSP.Kitchens","Media","Public.Transport");parent.values = c("No","Low","Yes");child = "Public.Transport"

#copmarison of w8 | w4
ceg.child.parent.monitor(df,"cega.w8",4,"cega.w4",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach48l
ceg.child.parent.monitor(df,"cega.w8",4,"cega.w4",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach48;
bnTl <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media","Public.Transport"),parent.values = c("No","Low","Yes"),child = "Total.Meals",n=500,learn=F)
bnT <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media","Public.Transport"),parent.values = c("No","Low","Yes"),child = "Total.Meals",n=500,learn=T)
#plottin
plot(pach48[,1],col='red',pch=18)
lines(pach48l[,1], col='red',pch=19)
lines(bnTl[,1],col='blue',pch=18)
plot(bnT[,1],col='blue',pch=19)


#copmarison of w7 | w4
ceg.child.parent.monitor(df,"cega.w7",4,"cega.w4",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach47l
ceg.child.parent.monitor(df,"cega.w7",4,"cega.w4",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach47;
bnTl <-bn.parent.child.monitor(df,parents=c("Public.Transport"),parent.values = c("Yes"),child = "Total.Meals",n=500,learn=T)
bnT <-bn.parent.child.monitor(df,parents=c("Public.Transport"),parent.values = c("Yes"),child = "Total.Meals",n=500,learn=F)
#plottin

#with learning
plot(bnTl[,1],col='blue',pch=18,type='b')
lines(pach47l[,1], col='red',pch=19,type='b')
legend("topleft", legend=c("CEG", "BN"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)
#without learning
plot(bnT[,1],col='blue',pch=19)
lines(pach47[,1],col='red',pch=18, type='b')
legend("topleft", legend=c("CEG", "BN"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

#comparison of w3| w1 
ceg.child.parent.monitor(df,"cega.w3",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach13;plot(pach13[,1])
ceg.child.parent.monitor(df,"cega.w3",3,"cega.w1",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach13;lines(pach13[,1])
bnTl <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media"),parent.values = c("No","High"),child = "Public.Transport",n=500,learn=F)
plot(bnTl[,1])
bnT <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media"),parent.values = c("No","High"),child = "Public.Transport",n=500,learn=T)
lines(bnT[,1])
#line for troubleshooting
parents=c("SFSP.Kitchens","Media");parent.values = c("No","Low");child = "Public.Transport"

#comparison of w5|w2
ceg.child.parent.monitor(df,"cega.w5",3,"cega.w2",cega.stages,cega.stage.key,cega.struct, n=500,learn=T) -> pach25l;plot(pach25l[,1])
ceg.child.parent.monitor(df,"cega.w5",3,"cega.w2",cega.stages,cega.stage.key,cega.struct, n=500,learn=F) -> pach25;lines(pach25[,1])
bnTl <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media"),parent.values = c("Yes","High"),child = "Public.Transport",n=500,learn=F)
plot(bnTl[,1])
bnT <-bn.parent.child.monitor(df,parents=c("SFSP.Kitchens","Media"),parent.values = c("Yes","High"),child = "Public.Transport",n=500,learn=T)
lines(bnT[,1])

#run the one step ahead prediction forecasts 
which.cut=3
possible.colorings <- listParts(dim(stage.key[[which.cut]])[1])##removin the one where no one gets a color#to get current stage,
#troubleshoot
rho=0.8; epsilon=1.2;which.cut=3;stage.key=cega.stage.key;n.monitor=50;crrnt.stg=14;
xM <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 2,stage.key = cega.stage.key,n.monitor = 500,crrnt.stg = 1);plot(-log(xM))
xM2 <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 2,stage.key = cega.stage.key,n.monitor = 500,crrnt.stg = 2);plot(-log(xM2))

xT <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 3,stage.key = cega.stage.key,n.monitor = 500,crrnt.stg = 14);plot(-log(xT))
xT2 <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 3,stage.key = cega.stage.key,n.monitor = 500,crrnt.stg = 15);plot(-log(xT2))
xT3 <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 3,stage.key = cega.stage.key,n.monitor = 500,crrnt.stg = 5);plot(-log(xT3))
xT4 <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 3,stage.key = cega.stage.key,n.monitor = 500,crrnt.stg = 11);plot(-log(xT4))

#to do: figure out what staging is the right one when we have lots of partitions to search 
xmls <- one.step.forecast(rho = 0.8,epsilon = 1.2,df,which.cut = 4,stage.key = cega.stage.key,n.monitor = 500,crrnt.stg = 14);plot(-log(xmls))

media.forecast <-data.frame(cbind(1:500,-log(xM),-log(xM2)))
trans.forecast <- data.frame(cbind(1:500,-log(xT),-log(xT2),-log(xT3),-log(xT4))) 
meals.forecasts <- as.data.frame(cbind(1:500,-log(xM),-log(xT),-log(xmls)))

#load(theOneWithTheLongPartitionsRun.Rdata)
ggplot(media.forecast[-(1:4),], aes(x=X1,y=X2,color='S(1)')) + 
  geom_line()+
  geom_line(data=media.forecast[-(1:4),], aes(y=X3,color='S(2)'))+
  ggtitle("Forecast for Media") + ylab("Cumulative Log Penalty")+xlab('Relevant Sample Size')+
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/oneStepFrcstMedia.jpeg')

ggplot(trans.forecast[-(1:4),], aes(x=X1,y=X2,color='S(14)')) + 
  geom_line()+
  geom_line(data=trans.forecast[-(1:4),], aes(y=X3,color='S(15)')) +
  geom_line(data=trans.forecast[-(1:4),], aes(y=X4,color='S(5)')) + 
  geom_line(data=trans.forecast[-(1:4),], aes(y=X5,color='S(11)')) + 
ggtitle("Forecast for Transport") + ylab("Cumulative Log Penalty")+xlab('Relevant Sample Size')+
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/oneStepFrcstTrans.jpeg')


ggplot(meals.forecasts[-(1:4),], aes(x=V1,y=V3)) + 
  geom_line()+
  ggtitle("Forecast for Transport") + ylab("Cumulative Log Penalty")+xlab('')+
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/oneStepFrcstTransport.jpeg')

ggplot(meals.forecasts[-(1:4),], aes(x=V1,y=V4)) + 
  geom_line()+
  ggtitle("Forecast for Meals") + ylab("Cumulative Log Penalty")+xlab('')+
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/oneStepFrcstMeals.jpeg')


ggplot(pach13l, aes(x=1:500,y=Sm,color='With Learning')) + 
  geom_line() + 
  geom_line(data= pach13, aes(color= 'Without Learning')) +
  ggtitle('Floret Monitor for stage w3 | w1') + 
  xlab('') + ylab('Cumulative Log Penalty') +
  theme(panel.background = element_blank())
ggsave('C://Users/rachel/Documents/diagpaper/pachw31.jpeg')


#find the BF of the model
stages=cega.stages; prior=ref.prior()
N <- list(c(374, 79),
          c(335,39),
          c(66,13),
          c(37,2),
          c(314,21),
          c(76,3),
          c(71,45),
          c(2,28),
          c(0,21))
bf <- c()
for (i in 1:length(stages)){
  prior.vec <-unlist(prior[i])
  counts <- unlist(N[i])
  bf[i] <-lgamma(sum(prior.vec) + sum(lgamma(prior.vec+counts)) - (lgamma(sum(prior.vec)+sum(counts)) + sum(lgamma(prior.vec))))
}
##create a new CEG with the cuts combined

#create a new variable called access
df.b <-df
df.b$Access <- rep("Low", length(df.b$SFSP.Kitchens))
df.b$Access[which(df.b$SFSP.Kitchens=="No" & df.b$Media=="High")] <- "Medium"
df.b$Access[which(df.b$SFSP.Kitchens=="Yes")] <- "High"
df.b$Access=factor(df.b$Access)
df.b <- df.b[,c(5,3,4)]
summary(df.b)

cegb.stage.key <- list()
count(df.b) -> cegb.stage.key[[1]]
df.b %>% count(Access) -> cegb.stage.key[[2]]
df.b %>% count(Access, Public.Transport) -> cegb.stage.key[[3]]
df.b %>% count(Access, Public.Transport,Total.Meals ) -> cegb.stage.key[[4]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
cegb.stage.key[[1]]$stage <- c("cegb.w0")
cegb.stage.key[[2]]$stage <- c("cegb.w1", "cegb.w2", "cegb.w3")
cegb.stage.key[[3]]$stage <- c("cegb.w6", "cegb.w6", "cegb.w5", "cegb.w4", "cegb.w6", "cegb.w5")
cegb.stage.key[[4]]$stage <- rep("cegb.winf", length(cegb.stage.key[[5]]$n))

cegb.stages <- list("cegb.w0", "cegb.w1", "cegb.w2", "cegb.w3", "cegb.w4", "cegb.w5", "cegb.w6", "cegb.w7", "cegb.w8")

cegb.w0 <- df.b %>% count(SFSP.Kitchens)
cegb.w1 <- df.b %>% filter(SFSP.Kitchens=="No") %>% count(Media)
cegb.w2 <- df.b %>% filter(SFSP.Kitchens=="Yes") %>% count(Media)
cegb.w3 <- df.b %>% filter(SFSP.Kitchens=="No", Media=="Low") %>% count(Public.Transport)
cegb.w4 <- df.b %>% filter(SFSP.Kitchens=="No", Media=="High") %>% count(Public.Transport)
cegb.w5 <- df.b %>% filter((SFSP.Kitchens=="Yes"& Media=="High") | (SFSP.Kitchens=="Yes"& Media=="Low")) %>% count(Public.Transport)
cegb.w6 <- df.b %>% filter((SFSP.Kitchens=="No"& Media=="High" & Public.Transport=="No") |
                           (SFSP.Kitchens=="Yes"& Media=="High" & Public.Transport=="No") |
                           (SFSP.Kitchens=="Yes"& Media=="Low" & Public.Transport=="No") |
                           (SFSP.Kitchens=="Yes"& Media=="Low" & Public.Transport=="Yes") ) %>% count(Total.Meals)
cegb.w7 <- df.b %>% filter((SFSP.Kitchens=="No" & Media=="High"  & Public.Transport=="Yes") |  
                           (SFSP.Kitchens=="No" & Media=="Low"  & Public.Transport=="No")) %>% count(Total.Meals)
cegb.w8 <- df.b %>% filter((SFSP.Kitchens=="No" & Media=="Low"  & Public.Transport=="Yes")) %>% count(Total.Meals)
cegb.struct <- list(cegb.w0, cegb.w1, cegb.w2, cegb.w3, cegb.w4, cegb.w5, cegb.w6, cegb.w7, cegb.w8)#this is the observed values for each of the stages

bf <- function(prior, counts){
  lgamma(sum(prior))+sum(lgamma(prior+counts))-(lgamma(sum(prior+counts)) + sum(lgamma(prior)))
}
prior <- c(3,3);counts <- c(5,5);prior.ui <- bf(prior,counts)
prior <- c(2,2,2);counts <- c(2,4,6);prior.uj <- bf(prior,counts)
prior <- c(1,1,1);counts <- c(2,4,6);prior.uk <- bf(prior,counts)
prior.ui + prior.uj
prior.uk
prior.uj
