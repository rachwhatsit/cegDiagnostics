#' A function to compute the node monitors of the BN
#'
#' @param df data frame
#' @param col_name string with the node in question
#' @param prior vector prior set by modeller
#' @param n integer number of records in the dataset to consider
#' @param learn logical to include learning of prior or not
#' @keywords bn node monitor unconditional
#' @export
#' @examples###############################################################
##UNCONDITIONAL NODE MONITOR FOR BNs 
#df is the data in question, col_name is the stage in question, prior is the set prior (must have right number of iterations), n is the max sample size we wish to consider.
#df should be filtered first
bn.uncondtnl.node.monitor <- function(df, col_name="Events", prior, n=50, learn=FALSE) {#dataframes should also be added for the counts
  #add checks to make sure that prior has same number of items as counts in dataframe
  Zm <- rep(NA, n)
  Sm <- rep(NA, n)
  Em <- rep(NA, n)
  Vm <- rep(NA, n)
  p <- rep(NA, n)
  
  df %>% group_by_(col_name) %>% tally() -> empty
  empty$n <- rep(0,length(empty$n))
  if(learn==FALSE){
    for (i in 2:n){
      df_cut <- df[2:i,] 
      df_cut %>%
        group_by_(col_name) %>% #groups by the stage of interest
        tally() -> u1 #stage1
      empty$n[which(empty[[col_name]] %in% u1[[col_name]])] <- u1$n
      counts = empty$n 
      p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#logprobability
      #compute the z statistics
      Sm[i]=-p[i]
      Em[i]=sum((prior/sum(prior))*sum(counts))
      
      Vm[i] = (sum(counts)*((sum(counts)+sum(prior))/(1+sum(prior))))*(prior/sum(prior))*(1-(prior/sum(prior)))
      #Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
    }
    Zm = Sm-Em /sqrt(Vm)
  }
  else{
    for (i in 2:n){
      df_cut <- df[2:i,] 
      df_cut %>%
        group_by_(col_name) %>% #groups by the stage of interest
        tally() -> u1 #stage1
      empty$n[which(empty[[col_name]] %in% u1[[col_name]])] <- u1$n
      counts = empty$n 
      p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#log probability
      #compute the z statistics
      Sm[i]=-p[i]
      Em[i]=sum(exp(na.omit(p[i]))*na.omit(Sm[i]))
      Vm[i]=sum(exp(na.omit(p[i]))*(na.omit(p[i])^2) - na.omit(Em[i])^2)          
      #Vm[i] = (sum(counts)*((sum(counts)+sum(prior))/(1+sum(prior))))*(prior/sum(prior))*(1-(prior/sum(prior)))
      prior <- (prior+counts)/sum(counts+1)
    }
    Zm = Sm-Em /sqrt(Vm)  
  }
  results <-data.frame(cbind(Sm, Zm, Em, Vm))
  colnames(results) <- c("Sm", "Zm", "Em", "Vm")
  return((results))
}


bn.parent.child.monitor <- function(df, parents, parent.values, child, n=50, learn=FALSE) {#dataframes should also be added for the counts
  #add checks to make sure that prior has same number of items as counts in dataframe
  
  #passing col names to the filtering bit
  #p.sym <- sym(parents)
  #p.sym <- lapply(parents, sym)
  c.sym <- sym(child)
  alpha.bar <- max(apply(df, 2, function(x){length(levels(as.factor(x)))})) #max number of categories at each level in the dataset 
  prior <- rep(alpha.bar, length(levels(df[[child]])))/length(levels(df[[child]]))
  
  #initialize log penalty scores
  Zm <- rep(NA, n)
  Sm <- rep(NA, n)
  Em <- rep(NA, n)
  Vm <- rep(NA, n)
  p <- rep(NA, n)
  if(learn==FALSE){
    for (i in 1:n){
      
      df_cut <- df[1:i,] 
      #for each parent, filter it off 
      for (j in 1: length(parents)){
        df_cut <- filter(df_cut, UQ(sym(parents[j])) == parent.values[j])   
      }
      
      df_cut %>% count(!!c.sym) -> counts.tbl
      counts = counts.tbl$n 
      counts <- rep(0,length(prior))
      counts[as.numeric(rownames(counts.tbl))] <-counts.tbl$n
      
      p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#logprobability
      #compute the z statistics
      Sm[i]=-p[i]
      Em[i]=sum((prior/sum(prior))*sum(counts))#expected value
      Vm[i] = (sum(counts)*((sum(counts)+sum(prior))/(1+sum(prior))))*(prior/sum(prior))*(1-(prior/sum(prior)))
      #Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
    }
  }
  
  else{
    for (i in 1:n){
      
      df_cut <- df[1:i,] 
      #for each parent, filter it off 
      for (j in 1: length(parents)){
        df_cut <- filter(df_cut, UQ(sym(parents[j])) == parent.values[j])   
      }
      
      df_cut %>% count(!!c.sym) -> counts.tbl
      counts = counts.tbl$n 
      counts <- rep(0,length(prior))
      counts[as.numeric(rownames(counts.tbl))] <-counts.tbl$n
      
      p[i] = (lgamma(sum(prior)) + sum(lgamma(prior+counts)) - (sum(lgamma(prior)) + lgamma(sum(prior)+sum(counts))))#logprobability
      #compute the z statistics
      Sm[i]=-p[i]
      Em[i]=sum((prior/sum(prior))*sum(counts))#expected value
      Vm[i] = (sum(counts)*((sum(counts)+sum(prior))/(1+sum(prior))))*(prior/sum(prior))*(1-(prior/sum(prior)))
      #Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
      prior=prior+counts
    }    
  }
  
  Zm = Sm-Em /sqrt(Vm)
  results <-data.frame(cbind(Sm, Zm, Em, Vm))
  colnames(results) <- c("Sm", "Zm", "Em", "Vm")
  return((results))
}
