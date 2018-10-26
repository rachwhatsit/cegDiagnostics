library(tidyverse); library(rlang)

#sst is the result of CEGAHC.R 
#the result skey is the list of length same number of variables
#can match all of the possibilities to what size it is in. 
#df is the data frame of interest

find.cut <- function(x){
  if (is.null(dim(test[[x]])) == T) {
    no.nas <- length(which(test[[x]] != "NA"))
  }
  else{
    no.nas <- length(which(test[[x]][,1]!="NA"))}
  return(no.nas)}#used in tostagekey


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
  skey[[1]]$color <- 0
  w <- 0
  color <- 0
  for (i in 2:length(skey)) {
    #add a stage column
    skey[[i]]$stage <- rep(NA, dim(skey[[i]])[1]) #initialized
    skey[[i]]$color <- rep(NA, dim(skey[[i]])[1]) #initialized
    test <<- sst$result
    unlist(map(1:length(test), find.cut))->test.cut
    test.cut[1]<-0
    
    #pull out the tests that have the right idx
    test.cut.idx <- which(test.cut==(i-1))
    test.sbst <- test[test.cut.idx] #results in same cut
  
    #find the corresponding merged list
    skey1 <- skey[[i]]
    cols <- syms(colnames(df)[1:(i - 1)])
    unite(skey1, key,!!!(cols), sep = "", remove = F) -> skey1
    for (j in 1:length(test.sbst)) {
      
      in.paths <- as_tibble(t(data.frame(test.sbst[j])))
      in.path.cols <- syms(colnames(in.paths)[1:(i - 1)])
      unite(in.paths,
            key,
            !!!in.path.cols,
            sep = "",
            remove = F) -> in.paths
      same.color <- which(skey1$key %in% in.paths$key)####returns the indices of the rows that are in the same color
     ###check out the position 
      if (length(same.color)==1){#what happens if there is only one position of the same color
        w <- w+1 
        color <- color+1
        skey1$stage[same.color] <- paste0('w',w)
        skey1$color[same.color] <- color
      } else if(i==(num.cuts)+1){
        w <- w+1
        color <- color+1
        skey1$stage[same.color] <- paste0('w',w)
        skey1$color[same.color] <- color
        } else {
        #do all the other stuff below
        color <- color+1
        test2 <- sst$result[test.cut==i]  
        rows <- dim(test.sbst[[j]])[2] ;cols <- length(levels(as.factor(df[[colnames(df)[i]]])))
        same.pos.mat <- matrix(rep(NA, rows*cols),nrow = rows,ncol=cols)#initalize the matrix of same positioning
        for(m in 1:rows){
          for (n in 1:cols){
            test.sbst[[j]][,m] -> tgt.lst
            tgt.lst[i] <- levels(as.factor(df[[colnames(df)[i]]]))[n]
            lapply(test2, `==`, tgt.lst) ->checkme
            lapply(1:length(checkme), function(x) apply(checkme[[x]],2,all))->all.checkme
            which(lapply(1:length(checkme),function(x) any(all.checkme[[x]]))==TRUE)->checkme.pos
            same.pos.mat[m,n] <- checkme.pos
          }
        }
        pos.color <- rep(NA, dim(test.sbst[[j]])[2])
        for(p in 1:dim(unique(same.pos.mat))[1]){
          w <- w+1
          pos.color.idx <-which(apply(same.pos.mat, 1, function(x) identical(x[1:dim(same.pos.mat)[2]], unique(same.pos.mat)[p,])))
          pos.color[pos.color.idx] <- paste0('w',w)
        }                       
        skey1$stage[same.color] <- pos.color #same color may not be the right idx???  
        skey1$color[same.color] <- color
      }
    }
    #for each row test to see which one it is in
    #match(skey1[,1]l, test[,1])
    skey[[i]] <- skey1
  }
  return(skey)
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
