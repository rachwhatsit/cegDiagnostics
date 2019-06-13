#' A function to compute the batch monitor of a BN according to the form given in Cowell, et. al. 2007
#'
#' @param df data frame 
#' @param child string with the name of the child node
#' @param parents string with the parent node
#' @param parentvalues string with the values of the parent node
#' @param prior prior set by modeller 
#' @keywords chisquare, cuts
#' @export
#' @examples bn.batch.monitor(df, Economic, Social, High, chds.prior)

bn.batch.monitor <- function(df, child, parents, parent.values,prior){#returns pearson chi-square of diff between observed and expected values
  #determine what the observed counts are
  df_cut <- df
  for (j in 1: length(parents)){
    df_cut <- filter(df_cut, UQ(sym(parents[j])) == parent.values[j])
  }
  count(df_cut, UQ(sym(child))) -> obsv.df
  obsv <- unlist(obsv.df$n)
  ref.prior <- rep(1/length(obsv.df$n), length(obsv.df$n))
  expct <- ref.prior*sum(obsv.df$n)
  pearson <- sum((obsv-expct)^2/expct)
  
  dscnt.fctr <- (sum(prior)+1)/(sum(obsv)+sum(prior))
  adjst.prsn <- dscnt.fctr*pearson
  d.free <- length(prior)-1
  p.val <- pchisq(adjst.prsn, d.free) 
  results <- list(adjst.prsn,d.free,p.val)
  return(results)
}

