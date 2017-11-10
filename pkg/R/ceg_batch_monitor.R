#' A function to compute the batch monitor of a CEG
#'
#' @param df data in question
#' @param struct
#' @param stage.key
#' @param stages
#' @param which.cut
#' @keywords cut batch
#' @export
#' @examples
#'  cegb.prior <- get.ref.prior(df, cegb.struct, cuts, cegb.stage.key, cegb.stages)
#'  bn.batch.monitor()
ceg.batch.monitor <- function(df, struct, stage.key, stages, which.cut){
  #which.cut is an integer that determines the cut for which we want to compute the batch monitors of two CEGs.
  colnames(df) -> cuts
  obsv <- stage.key[[which.cut]]$n #how many observed in each pathway through cut
  prior <- get.ref.prior(df, struct, cuts, stage.key, stages) #compute the prior
  which.to.add <- unlist(lapply(stage.key[[which.cut]]$stage, function(x){as.numeric(substr(x,nchar(x),nchar(x)))}))
  expct.cut <- unlist(prior[unlist(lapply(unique(stage.key[[which.cut-1]]$stage), function(x){as.numeric(substr(x,nchar(x),nchar(x)))+1}))])

  prior.vec <- rep(0, length(unique(which.to.add)))
  obsv.vec <- rep(0, length(unique(which.to.add)))
  for (i in 1:length(unique(which.to.add))){
    target.stage <- unique(which.to.add)[i]
    prior.vec[i] <- sum(expct.cut[which(which.to.add==which.to.add[i])])
    obsv.vec[i] <- sum(obsv[which(which.to.add==which.to.add[i])])
  }#this can be done with applys, but my head hurts

  expct.vec <- (prior.vec/n)*dim(df)[1]#scale the expected vec
  pearson <- sum((obsv.vec-expct.vec)^2/expct.vec)
  dscnt.fctr <- (sum(prior)+1)/(sum(prior)+sum(expct.vec))
  adjst.prsn <- pearson*dscnt.fctr
  d.free <- length(prior)-1
  p.val <- pchisq(adjst.prsn,d.free,lower.tail = F)
  results <- list(adjst.prsn,d.free,p.val)
  return(results) #returns the pearson coefficient for the pathways which can be used to find the p-value
  #go back in and add the discount factor
}
