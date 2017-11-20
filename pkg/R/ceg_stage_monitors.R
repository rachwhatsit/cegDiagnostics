#' A function to compute the node monitors of the BN
#'
#' @param df data in question
#' @param target.stage
#' @param target.cut
#' @param stages 
#' @param stage.key
#' @param struct
#' @param n number of iterations
#' @param learn binary to control learning 
#' @keywords bn node monitor
#' @export
#' @examples###############################################################
#no conditional parent monitor here. 
prior <- get.ref.prior(df, struct, cuts, stage.key, stages)
posterior <- rep(NA, length(prior))
for (i in (1:length(prior))){posterior[i] <- list(unlist(prior[i])+unlist(struct[[i]]$n))}
pi <- rep(NA, length(prior))
for (i in (1:length(prior))){pi[i] <- list(unlist(posterior[i])/sum(unlist(posterior[i])))}


ceg.condtnl.stage.monitor <- function(df, target.stage, stages, stage.key.struct, n, learn){
  Sm <- rep(NA, 1:n)
  #without learning,
  for (i in 1:n){
    ev <- df[1:n]
    pihat <- pass.message(df,stage.key, evidence=ev, post.mean=pi)
    Sm[i] <- lgamma()
    
    #with learning, update the prior each time 
    
  }
  
}