#' A function to compute the component monitor of a CEG
#'
#' @param df data in question
#' @param struct ceg.struct
#' @param stage.key ceg stage leu 
#' @param stages list of stages
#' @param which.cut which cut to examine
#' @keywords leave one out prequential 
#' @export
#' @examples getdata(radical.df, radical.sk.nocol) -> radical.data

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


#' A function that returns the component monitors
#' 
#' @param data from get.data fn
#' @param prior effective sample size over number of edges 
#' @keywords BayesFactor
#' @export
#' @examples expct.cnts <- expected.counts(chds.prior, chds.which.stage)
#' expctBF <-component.monitor(expct.cnts,chds.prior)
#'
#'  
component.monitor <- function(data, prior){###THIS IS
  components <- c()
#  for (i in 1:length(prior)){
  for (i in 1:3){
    alpha <- unlist(prior[i])
    N <- unlist(data[i])
    components[i]  <- sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) - lgamma(sum(alpha + N)))
  }
  return(components)
}

#' Returns the expected counts
#' 
#' @param prior effective sample size divided by number of outgoing edges
#' @param which.stage stage under consideration
#' @param which.cut cut containing the stage in question 
#' @keywords BayesFactor
#' @export
#' @examples
#' chds.which.stage <- c(1,2,2,3,3,4,4,4)
#'expct.cnts <- expected.counts(chds.prior, chds.which.stage)

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
  

#probably the least efficient way to code leave one out cross validation ever.
#use the nocol vers of the stage.key
#'returns the leave one out data
one.out.getdata <- function(df, stage.key) { #returns mega list of one out situations
  cuts <- colnames(df)
  cols <- syms(colnames(df)[1:(length(colnames(df)))])#add the last stage key on
  add.on <- length(stage.key)+1
  stage.key[[add.on]] <- count(df, !!!cols)
  stage.key[[add.on]]$stage <- rep('winf', length(stage.key[[add.on]]$n))
  mega.data.lst <- list()
  another.counter <- 0
  for (i in 3:(length(cuts)+1)){#loop over each cut in the stratified tree
    colnames(stage.key[[(i-1)]])[i] <- "from.stage"
    if(i==length(cuts)+1){
      print('')
    } else { 
    colnames(stage.key[[i]])[(i+1)] <-"to.stage"}
    cols <- colnames(df)[1:(i - 2)]
    trgcut <- sym(colnames(df)[(i-1)])
    full_join(stage.key[[i]],stage.key[[(i-1)]], by=cols) %>% group_by(!!trgcut, from.stage) %>% summarise(cnts=sum(n.x))->data.df
    full_join(stage.key[[i]],stage.key[[(i-1)]], by=cols) %>% group_by(from.stage) %>% count(from.stage) %>% mutate(cntrbt.sts = n/length(levels(as.factor(df[[trgcut]])))) ->numsits
    stgs <- unique(data.df$from.stage)
    num.stages <- length(stgs)
    for (j in 1:num.stages){
      counter <- 0
      another.counter <- another.counter+1
      data.lst <- list()
        if(numsits$cntrbt.sts[which(numsits$from.stage==stgs[j])]==1){#if there's only one contributing situation, proceed as normal
          counter <- counter+1
          data.lst[[counter]] <- data.df$cnts[which(data.df$from.stage==stgs[j])]
        } else { #otherwise take each one out
          for(k in 1:numsits$cntrbt.sts[which(numsits$from.stage==stgs[j])]){#for each of the possible situations
            full_join(stage.key[[i]],stage.key[[(i-1)]], by=cols) %>% group_by(!!trgcut, from.stage) ->all.df #filter the data frame
            cntrb.sits.idx <- which(all.df$from.stage==stgs[j])
            num.edges <-length(levels(as.factor(df[[trgcut]])))
            LOOsits.idx <- (1:num.edges)+num.edges*(k-1)#which indices to leave out
            all.df[cntrb.sits.idx,] -> df2 
            df2[-LOOsits.idx,] %>%summarise(cnts=sum(n.x))->sumthisone.df
            counter <- counter+1
            data.lst[[counter]] <- sumthisone.df$cnts
            
          }
        }
      mega.data.lst[[another.counter]] <- data.lst
    }
    colnames(stage.key[[(i-1)]])[i] <- "stage"
    if(i==length(cuts)+1){
      print('')
    } else { 
      colnames(stage.key[[i]])[(i+1)] <- "stage"
    }
  }  
  return(mega.data.lst)
}

