library(tidyverse); library(rlang)

radical.df<- read.csv(file = "data1.csv")
sst <- jCEG.AHC(chds.df)
radical.sst <- jCEG.AHC(radical.df) 
sst$result
sst$merged

#sst is the result of CEGAHC.R 
#the result skey is the list of length same number of variables
#can match all of the possibilities to what size it is in. 
#df is the data frame of interest

tostagekey <- function(df, sst) {
  sst$comparisonset
  num.cuts <-
    length(sst$comparisonset)#gives the number of cuts in the stratified staged tree excluding root
  stage.num <-
    do.call(rbind, lapply(sst$comparisonset, length)) #gives the number of stages (except the root node)
  sst$mergedlist[unlist(sst$comparisonset)]#not NA parts of the merged list, shows the paths leading into each stage
  sum(stage.num) == length(sst$mergedlist[unlist(sst$comparisonset)]) #should be T, if not, panic
  
  #initalize the stage tree
  skey <- list()
  skey[[1]] <- count(df) #this is the root node
  for (i in 2:(num.cuts + 1)) {
    cols <- syms(colnames(df)[1:(i - 1)])
    skey[[i]] <- count(df,!!!cols)
  } #this initializes the stage key for the dataset.
  #skey gives us all the unique combinations of variable levels represented in the dataset.
  #it does NOT give us all possible combos of variable levels.
  
  #now we need to add the stage to each of the skey lists
  skey[[1]]$stage <-
    "w0" #specify the root situation in the stratified tree
  w <- 0
  for (i in 2:length(skey)) {
    #add a stage column
    skey[[i]]$stage <- rep(NA, dim(skey[[i]])[1]) #initialized
    test <- sst$mergedlist[unlist(sst$comparisonset[[(i - 1)]])]
    #find the corresponding merged list
    skey1 <- skey[[i]]
    #in.paths$key <- apply( in.paths[ , 1:dim.x ] , 1 , paste , collapse = "" )
    cols <- syms(colnames(df)[1:(i - 1)])
    unite(skey1, key,!!!(cols), sep = "", remove = F) -> skey1
    for (j in 1:length(test)) {
      w <- w + 1
      in.paths <- as_tibble(t(data.frame(test[j])))
      in.path.cols <- syms(colnames(in.paths)[1:(i - 1)])
      unite(in.paths,
            key,
            !!!in.path.cols,
            sep = "",
            remove = F) -> in.paths 
      ####FIX THE COMPLETE AND UTTER LACK OF POSITION INFORMATION IN THE TREE DAMMIT.
      # #check that the same paths also appear in the subsequen stage
      
      test2 <- sst$mergedlist[unlist(sst$comparisonset[[i]])]
      pos <- 1 #original position
      pstns <- c()
      test.for.pos <- list()
      for(k in 1:dim(test[[j]])[2]){
        tgt.pos <- test[[j]][,k][(test[[j]][,k]!="NA")] #labels to test, ex: 'High High'
        nxt.cut <- lapply(test2, data.frame)
        
        test.for.pos[[k]] <- find.idx(tgt.pos,nxt.cut)

      }
      
      find.idx <- function(tgt.pos, nxt.cut){
        tgt.pos.idx <- c()####ONLY WORKS FOR TWO DEEP DAMMIT
        for (l in 1:length(nxt.cut)){
          tgt.pos.idx[l] <- ifelse(any(test2[[l]][1,]==tgt.pos[1] & test2[[l]][2,]==tgt.pos[2]),l, 0)
        }
        return(tgt.pos.idx)#the indices of where the tgt.pos are
      }
      # ##END THE NEW GARBAAAGE######################################
      
      skey1$stage[which(skey1$key %in% in.paths$key)] <-
        paste0('w', as.character(w)) #need to change i to the mapping for the stage numberings
    }
    #for each row test to see which one it is in
    #match(skey1[,1]l, test[,1])
    skey[[i]] <- skey1
  }
  return(list(skey, stage.num))
}


to.struct <- function(df, stage.key, sst) {
  idx <-
    which(!is.na(unlist(lapply(
      sst$data, '[[', 1
    ))) == TRUE)
  sst$data[idx] -> strct
  stage.num <-
    do.call(rbind, lapply(sst$comparisonset, length)) #gives the number of stages (except the root node)
  dimnames(strct[[1]]) <- NULL
  struct <- list()
  struct[[1]] <-
    as.tibble(cbind(levels((df[colnames(df)[1]])[, 1]), as.vector(as.numeric(strct[[1]]))))
  names(struct[[1]]) <- c(colnames(df)[1], "n")
  counter <- 1
  whichcut = 1
  for (i in 2:length(idx)) {
    #check about the weirdness that is the first entry
    if (counter <= stage.num[whichcut]) {
      counter <- counter + 1
    } else{
      counter <- 2
      whichcut <- whichcut + 1
    }
    
    struct[[i]] <-
      as.tibble(cbind(levels((df[colnames(df)[whichcut + 1]])[, 1]), as.vector(as.numeric(strct[[i]]))))
    names(struct[[i]]) <- c(colnames(df)[whichcut+1], "n")
  }
  return(struct)
}
