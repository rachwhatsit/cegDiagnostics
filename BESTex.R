library(ceg)
#df.orig <- read.csv("180112_BEST_EQ5D3L_ALLTIMEPOINTS.csv",stringsAsFactors = F)
df.orig <- read.csv("180112_BEST_EQ5D3L_ALLTIMEPOINTS_TX.csv", stringsAsFactors = F)


df <- df.orig[-which(df.orig==-99,arr.in=TRUE),]#remove 99s
df <- df[-which(is.na(df)==TRUE),]#remove incompletes
df[which(df==3,arr.ind = TRUE)]<- 2 #binned into any problems
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


#import Jane's code here
df0.am.sst <- jCEG.AHC(df0.am,priorN = 2)
df0.amcbt.sst <- jCEG.AHC(df0.amcbt)
df3.am.sst <- jCEG.AHC(df3.am)
df3.amcbt.sst <- jCEG.AHC(df3.amcbt)
df6.am.sst <- jCEG.AHC(df6.am)
df6.amcbt.sst <- jCEG.AHC(df6.amcbt)
df12.am.sst <- jCEG.AHC(df12.am)
df12.amcbt.sst <- jCEG.AHC(df12.amcbt)

df0.am.sst$lik  
df3.am.sst$lik  
df6.am.sst$lik  
df12.am.sst$lik  #change in BF here

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

#component scores for each time series
am.bf <- as.data.frame(cbind(unlist(df0.am.sst$pach), unlist(df3.am.sst$pach), unlist(df6.am.sst$pach), unlist(df12.am.sst$pach)))
amcbt.bf <- as.data.frame(cbind(unlist(df0.amcbt.sst$pach), unlist(df3.amcbt.sst$pach), unlist(df6.amcbt.sst$pach), unlist(df12.amcbt.sst$pach)))



