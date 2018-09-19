library(tidyverse); library(rlang)

radical.df<- read.csv(file = "data1.csv")
sst <- jCEG.AHC(chds.df)
radical.sst <- jCEG.AHC(radical.df) 
sst$result
sst$merged

#sst is the result of jAHC.R 
#the result skey is the list of length same number of variables
#can match all of the possibilities to what size it is in. 
#df is the data frame of interest

tostagekey <- function(df, sst){skey <- list()#list of size 4

sst$comparisonset
num.cuts <- length(sst$comparisonset)#gives the number of cuts in the stratified staged tree excluding root
stage.num<-do.call(rbind, lapply(sst$comparisonset, length)) #gives the number of stages (except the root node)
sst$mergedlist[unlist(sst$comparisonset)]#not NA parts of the merged list, shows the paths leading into each stage
sum(stage.num) == length(sst$mergedlist[unlist(sst$comparisonset)]) #should be T, if not, panic

#initalize the stage tree 
skey[[1]] <- count(df) #this is the root node 
for (i in 2:(num.cuts+1)){
  cols <- syms(colnames(df)[1:(i-1)])
  skey[[i]] <- count(df, !!!cols) 
} #this initializes the stage key for the dataset.
#skey gives us all the unique combinations of variable levels represented in the dataset. 
#it does NOT give us all possible combos of variable levels. 
 
#now we need to add the stage to each of the skey lists 
skey[[1]]$stage <- "w0" #specify the root situation in the stratified tree 
w<-0
for (i in 2:length(skey)){ #add a stage column
  skey[[i]]$stage <- rep(NA, dim(skey[[i]])[1]) #initialized
  test <-sst$mergedlist[unlist(sst$comparisonset[[(i-1)]])]
  #find the corresponding merged list 
  skey1 <- skey[[i]]
  #in.paths$key <- apply( in.paths[ , 1:dim.x ] , 1 , paste , collapse = "" )
  cols <- syms(colnames(df)[1:(i-1)])
  unite(skey1, key, !!!(cols), sep="", remove=F) -> skey1
  for(j in 1:length(test)){
    w <- w+1
    in.paths <- as_tibble(t(data.frame(test[j])))
    in.path.cols <- syms(colnames(in.paths)[1:(i-1)])
    unite(in.paths, key, !!!in.path.cols, sep="", remove=F) -> in.paths #will need to check this for other data frame
    skey1$stage[which(skey1$key %in% in.paths$key)] <- paste0('w',as.character(w)) #need to change i to the mapping for the stage numberings
  }
  #for each row test to see which one it is in
  #match(skey1[,1]l, test[,1])
  skey[[i]]<-skey1
}
return(skey)
}
# count(df) #w0
# skey1<- count(df,df[[1]], df[[2]],df[[3]])
# skey1$stage <- rep(NA, length(skey1[[1]]))
# test <-sst$mergedlist[unlist(sst$comparisonset[[3]])]
# 
# unite(skey1, key, starts_with("df"),sep="",remove = F) ->skey1 #will also need to check this for other data frames
# for(i in 1:length(test)){
#   in.paths <- as_tibble(t(data.frame(test[i])))
#   unite(in.paths, key, starts_with("V"), sep="", remove=F) -> in.paths #will need to check this for other data frames
#   skey1$stage[which(skey1$key %in% in.paths$key)] <- paste0('w',as.character(i)) #need to change i to the mapping for the stage numberings
# }
# #for each row test to see which one it is in
# match(skey1[,1]l, test[,1])
