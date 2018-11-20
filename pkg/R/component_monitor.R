#' A function to compute the component monitor of a CEG
#'
#' @param df data in question
#' @param struct
#' @param stage.key
#' @param stages
#' @param which.cut
#' @keywords cut batch
#' @export
#' @examples

getdata <- function(df, stage.key) {
  cuts <- colnames(df)
  cols <- syms(colnames(df)[1:(length(colnames(df)))])#add the last stage key on
  add.on <- length(stage.key)+1
  stage.key[[add.on]] <- count(df, !!!cols)
  stage.key[[add.on]]$stage <- rep('winf', length(stage.key[[add.on]]$n))
  counter <- 1
  data.lst <- list()
  for (i in 3:(length(cuts)+1)){
    colnames(stage.key[[(i-1)]])[i] <- "from.stage"
    colnames(stage.key[[i]])[(i+1)] <- "to.stage"
    cols <- colnames(df)[1:(i - 2)]
    trgcut <- sym(colnames(df)[(i-1)])
    full_join(stage.key[[i]],stage.key[[(i-1)]], by=cols) %>%  
      group_by(!!trgcut, from.stage) %>% summarise(cnts=sum(n.x))->data.df
      
    stgs <- unique(data.df$from.stage)
    num.stages <- length(stgs)
    
    for (j in 1:num.stages){
      counter <- counter+1
      data.lst[[counter]] <- data.df$cnts[which(data.df$from.stage==stgs[j])]#here's the point where we take one out! 
    }
    colnames(stage.key[[(i-1)]])[i] <- "stage"
    colnames(stage.key[[i]])[(i+1)] <- "stage"
    
  }
  return(data.lst)
}



##TODO: change for the sparsity of the different functions... 
#' @param data from get.data fn
#' @param prior
#' @keywords BayesFactor
#' @export
#' @examples
#' 
component.monitor <- function(data, prior){###THIS IS
  components <- c()
  for (i in 1:length(prior)){
    alpha <- unlist(prior[i])
    N <- unlist(data[i])
    components[i]  <- sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) - lgamma(sum(alpha + N)))
  }
  return(components)
}

#' @param prior
#' @param which.stage
#' @param which.cut
#' @keywords BayesFactor
#' @export
#' @examples
#' 
expected.counts <- function(prior,which.stage){
  p <- lapply(chds.prior, function(x) (x/sum(x)))
  expct.cnts <- list()
  expct.cnts[[1]] <- 890*p[[1]]
  root.cut <- 1;crrnt.cut <-2
  for (i in 2:length(which.stage)){
    root.cut <- ifelse(crrnt.cut==which.stage[i], root.cut, root.cut+1)
    crrnt.cut <- ifelse(crrnt.cut==which.stage[i], crrnt.cut, crrnt.cut+1)
    expct.root <- expct.cnts[[which(which.stage==root.cut)[[1]]]][[1]]#counts we expect to see entering the stage
    expct.cnts[[i]] <- expct.root*p[[i]]
  }
  return(expct.cnts)
}
  

#code the leave one out method
one.out.getdata <- function(df, stage.key) { #returns mega list of one out situations
  cuts <- colnames(df)
  cols <- syms(colnames(df)[1:(length(colnames(df)))])#add the last stage key on
  add.on <- length(stage.key)+1
  stage.key[[add.on]] <- count(df, !!!cols)
  stage.key[[add.on]]$stage <- rep('winf', length(stage.key[[add.on]]$n))
  mega.data.lst <- list()
  for (i in 3:(length(cuts)+1)){
    colnames(stage.key[[(i-1)]])[i] <- "from.stage"
    if(i==length(cuts)+1){
      print('')
    } else { 
      colnames(stage.key[[i]])[(i+1)] <-"to.stage"}
    cols <- colnames(df)[1:(i - 2)]
    trgcut <- sym(colnames(df)[(i-1)])
    full_join(stage.key[[i]],stage.key[[(i-1)]], by=cols) ->anotherchunk
    
    for (k in 1:dim(anotherchunk)[1]){
      counter <- 1
      data.lst <- list()
      anotherchunk %>% filter(!row_number()==1) %>% group_by(!!trgcut, from.stage) %>% summarise(cnts=sum(n.x))->data.df
      
      stgs <- unique(data.df$from.stage)
      num.stages <- length(stgs)
      
      for (j in 1:num.stages){
        counter <- counter+1
        data.lst[[counter]] <- data.df$cnts[which(data.df$from.stage==stgs[j])]
      }
      mega.data.lst[[k]] <- data.lst
    }
    
    colnames(stage.key[[(i-1)]])[i] <- "stage"
    if(i==length(cuts)+1){
      print('')
    } else { 
      colnames(stage.key[[i]])[(i+1)] <- "stage"}
    
    
  }
  return(mega.data.lst)
}

