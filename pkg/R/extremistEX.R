library(ceg);library(deal)
library(tidyr);library(tidyverse)
library(rlang);library(partitions)

#the data
df <- read.csv("data1.csv")

colnames(df)

#find the BN
fit <- network(df)
bn.prior <- jointprior(fit,600)
getnetwork(learn(fit,df,bn.prior))->bn.fit
bn.hisc <- autosearch(bn.fit,df,bn.prior,trace=F,removecycles = T)
bn <- getnetwork(bn.hisc)
plot(bn)

#find the CEG
df <- CheckAndCleanData(df)
ct <- ceg:::ContingencyTable(stratified.event.tree = set, data = df)
set <- Stratified.event.tree(df)
plot(set)
prior <- ceg:::PriorDistribution(set,5)

l1<-ceg:::OAHC(1,prior,ct, set)
l2<-ceg:::OAHC(2,prior,ct, set)
l3<-ceg:::OAHC(3,prior,ct, set)
l4<-ceg:::OAHC(4,prior,ct, set)#throws an error b/c of rodriguo's code
l5<-ceg:::OAHC(5,prior,ct, set)
l6<-ceg:::OAHC(6,prior,ct, set)
l7<-ceg:::OAHC(7,prior,ct, set)
l8<-ceg:::OAHC(8,prior,ct, set)

getcolors <- function(l=l3,start=3){
  flat <- unlist(l@cluster)[-which(is.na(unlist(l@cluster))==T)]
  num <- lengths(l@cluster)[!is.na(l@cluster)]
  labels <- c()
  for (i in 1:length(num)){
    start=start+1
    labels <- c(labels, rep(paste0('cegw',as.character(start)),num[i]))
  }
  return(labels)
}

#find the diagnostics of the fit structure
#first desribe the structure of the CEG 

  
ceg.stage.key <- list() 
df %>% count(Sex) -> ceg.stage.key[[1]]
df %>% count(Sex,Religion) -> ceg.stage.key[[2]]
df %>% count(Sex,Religion,Age) -> ceg.stage.key[[3]]
df %>% count(Sex,Religion,Age, Offence) -> ceg.stage.key[[4]]
df %>% complete(Sex,Religion,Age, Offence, Nationality)  %>% count(Sex,Religion,Age, Offence, Nationality) -> ceg.stage.key[[5]]
df %>% complete(Sex,Religion,Age, Offence, Nationality, Network)  %>% count(Sex,Religion,Age, Offence, Nationality, Network)-> ceg.stage.key[[6]]
df %>% count(Sex,Religion,Age, Offence, Nationality, Network, Engagement) -> ceg.stage.key[[7]]

ceg.stage.key[[1]]$stage <- c("cegw1","cegw2")
ceg.stage.key[[2]]$stage <- getcolors(l3,2)
ceg.stage.key[[3]]$stage <- c("cegw8","cegw9","cegw10","cegw11","cegw12","cegw13","cegw8","cegw9","cegw10","cegw11","cegw12","cegw13","cegw8","cegw9","cegw10","cegw11","cegw12","cegw13")
ceg.stage.key[[4]]$stage <- getcolors(l5,13)
ceg.stage.key[[5]]$stage <- getcolors(l6,21)
ceg.stage.key[[6]]$stage <- getcolors(l7,32)
ceg.stage.key[[7]]$stage <- rep("cegwinf",length(ceg.stage.key[[7]]$n))

cuts <- colnames(df)


ceg.stage.key->stage.key
stage.key[[1]]$fromstage <-rep("cegw0",2)
stage.key[[2]]$fromstage <- c(rep("cegw1",3),rep("cegw2",3))
for(i in 3:length(cuts)){##FIX THIS: need all previous labels for the pathway search applying recursive formulas in R
  from.ceg.idx <- match(interaction(get.interactions(stage.key,i,i)),interaction(get.interactions(stage.key,(i-1),i)))
  stage.key[[i]]$fromstage <- stage.key[[i-1]]$stage[from.ceg.idx]#paste all of the possibilities here
}
stages <- unique(c(stage.key[[1]]$fromstage,stage.key[[2]]$fromstage,stage.key[[3]]$fromstage,stage.key[[4]]$fromstage,stage.key[[5]]$fromstage,stage.key[[6]]$fromstage,stage.key[[7]]$fromstage))


get.interactions <- function(stage.key, which.key, intgr){#idiotic script to be able to use the interaction function. for use with the for loop above to go from RC's code to mine.
  dff = data.frame(matrix(vector(), dim(stage.key[[which.key]])[1], intgr-1))
  for (ii in 1:(intgr-1)){
    dff[,ii] <- unlist(stage.key[[which.key]][cuts[ii]])
  }
  return(dff)
  }

##now want to create a struct YAS WE MADE A STRUCT. TAKE THAT.
get.struct <- function(stages, stage.key){
  cut.idx <- 1
  struct <- list()
  cuts=colnames(df)
  for (i in 1:length(stages)){
    stg <- stages[i]
    if(stg %in% stage.key[[cut.idx]]$fromstage==F){cut.idx <- cut.idx+1}
    stage.key[[cut.idx]][which(ceg.stage.key[[cut.idx]]$stage==stg),]
    struct[[i]] <- filter(stage.key[[cut.idx]], fromstage==stg) %>% select((cuts[[cut.idx]]),n) %>% group_by_(UQ(cuts[[cut.idx]])) %>% summarise(tot.edge=sum(n))
  }
}

prior <- list()
for (i in 1:length(struct)){
  prior[[i]] <- struct[[i]]$tot.edge / sum(struct[[i]]$tot.edge)
}
  
posterior <- rep(NA, length(prior))
for (i in (1:length(prior))){posterior[i] <- list(unlist(prior[i])+unlist(struct[[i]]$tot.edge))}
post.mean <- rep(NA, length(prior))
for (i in (1:length(prior))){post.mean[i] <- list(unlist(posterior[i])/sum(unlist(posterior[i])))}


#TO RUN YOU WANT TO DELET THE ROOT AND SINK NODE IN THE STAGES
#w38monitor <- c()

n.stages <- length(unique(stage.key[[7]]$fromstage))
stages.in.question <- unique(stage.key[[7]]$fromstage)
stgs <- c(34:38)
n.stages <- length(stgs)
n.dim <- dim(df)[1]

brier<-as.data.frame(matrix(rep(0,n.dim*n.stages), nrow = n.dim, ncol=n.stages))
prob.yes<-as.data.frame(matrix(rep(0,n.dim*n.stages), nrow = n.dim, ncol=n.stages))
prob.no<-as.data.frame(matrix(rep(0,n.dim*n.stages), nrow = n.dim, ncol=n.stages))

#toolk 3.5 hrs last time.
  for (i in 1:n.dim){
    pass.message(df,stage.key,evidence=df[1:10,1:6],post.mean,prior,stages[-c(1,41)]) -> updated.ev
    #w38monitor <- c(w38monitor,updated.ev[[36]][1])
    for (j in 1:n.stages){
    prob <- updated.ev[[stgs[j]]]
    prob.no[i,j] <- updated.ev[[stgs[j]]][1]
    prob.yes[i,j] <- updated.ev[[stgs[j]]][2]
    actual <- rep(0,length(updated.ev[[stgs[j]]]))#; df_sub <-filter(df[1:500,],Social=="High")
    actual[as.numeric(as.character(df[i,]))[7]] <- 1
    brier[i,j] <- sum((prob-actual)^2)
    
    }
  }

df.v <-cbind(df$Engagement, prob.yes)
w33 <-df.v[which(df.v[,2]!=.5),c(1,2)]
colnames(w33)[1]<-"Engagement"
w33 %>% group_by(Engagement) %>% count(V1)

w34 <-df.v[which(df.v[,3]!=.5),c(1,3)]
colnames(w34)[1]<-"Engagement"
w34 %>% group_by(Engagement) %>% count(V2)

w35 <-df.v[which(df.v[,4]!=.5),c(1,4)]
colnames(w35)[1]<-"Engagement"
w35 %>% group_by(Engagement) %>% count(V3)

w36 <-df.v[which(df.v[,5]!=.5),c(1,5)]
colnames(w36)[1]<-"Engagement"
w36 %>% group_by(Engagement) %>% count(V4)


df.v2 <-cbind(df$Engagement, prob.no)
w33 <-df.v2[which(df.v2[,2]!=.5),c(1,2)]
colnames(w33)[1]<-"Engagement"
w33 %>% group_by(Engagement) %>% count(V1)

w34 <-df.v2[which(df.v2[,3]!=.5),c(1,3)]
colnames(w34)[1]<-"Engagement"
w34 %>% group_by(Engagement) %>% count(V2)

w35 <-df.v2[which(df.v2[,4]!=.5),c(1,4)]
colnames(w35)[1]<-"Engagement"
w35 %>% group_by(Engagement) %>% count(V3)

w36 <-df.v2[which(df.v2[,5]!=.5),c(1,5)]
colnames(w36)[1]<-"Engagement"
w36 %>% group_by(Engagement) %>% count(V4)

summary(as.factor(prob.no[(which(prob.no$V1 != 0.5)),1]))
summary(as.factor(prob.yes[(which(prob.yes$V1 != 0.5)),1]))

summary(as.factor(df[(which(prob.no$V1 != 0.5)),7]))

summary(as.factor(w38monitor))
hist(w38monitor)

test$phi
brier<-c();
for (i in 1:500){
  #if(df[i,1]!="High"){next}
  #if(df[i,2]!="High"){next}
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


sum((prob.w1-actual)^2)stage.key=ceg.stage.key;evidence=df[1,(-7)]
render.ceg(cuts, stage.key=ceg.stage.key,stages)
