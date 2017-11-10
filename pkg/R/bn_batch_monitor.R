#' A function to compute the batch monitor of a BN
#'
#' @param df data in question
#' @param child
#' @param parents
#' @param parentvalues
#' @keywords cut batch
#' @export
#' @examples
#'  cegb.prior <- get.ref.prior(df, cegb.struct, cuts, cegb.stage.key, cegb.stages)
#'  bn.batch.monitor()

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

