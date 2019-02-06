#' @param u
#' @export
#' @examples
surprise.score <- function(u){#guessing this is what jim calls the bayes p-value
  score <- rep(NA, length(chds.loo.counts[[u]]))
  counts <- rep(0, length(unlist(chds.loo.counts[[u]][i])))
  for (i in 2:length(chds.loo.counts[[u]])){
    new.counts <- unlist(chds.data[[u]])-unlist(chds.loo.counts[[u]][i-1])
    counts <- counts + new.counts
    a1 <- counts[1]
    b1 <- sum(counts[-1])
    stage.counts <- unlist(chds.data[[u]])-unlist(chds.loo.counts[[u]][i])#current situation counts
    n1 <- sum(stage.counts)
    y1 <- stage.counts[1]
    score[i] <-exp(lgamma(a1 + b1) - lgamma(a1) - lgamma(b1) +
                     log(choose(n1,y1)) +
                     lgamma(a1 + y1) + lgamma(b1+n1-y1) - lgamma(a1+b1+n1))#level of surprise at seeing that ratio

  }
  return(score)
}

#' @param u
#' @param level
#' @keywords BayesFactor
#' @export
#' @examples
sit.resids <- function(u,level){
  expct <- rep(NA, length(chds.loo.counts[[u]]))
  obsv <- rep(NA, length(chds.loo.counts[[u]]))
  for (i in 1:length(chds.loo.counts[[u]])){
    counts <- unlist(chds.loo.counts[[u]][i])
    stage.counts <- unlist(chds.data[[u]])-counts
    post <- unlist(chds.prior[[u]])+counts
    obsv[i] <- stage.counts[level]/sum(stage.counts)
    expct[i] <- post[level]/sum(post)
  }
  df <- as.data.frame(cbind(1:length(chds.loo.counts[[u]]),obsv,expct))

  df %>% gather(key, value, -V1) %>%
    ggplot(aes(x=V1, y=value, colour=key,shape=key)) +
    geom_point(size=4) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks= 1:length(chds.loo.counts[[u]])) +
    theme(legend.position = "none") +
    xlab(expression(v[j])) + ylab(TeX("$E(\\alpha^+_{-k'}, \\beta^+_{-k'})$"))  + theme_minimal()

}



###########FUNCTIONIZE MEEEEE
#' @param u
#' @param level
#' @param data
#' @param prior
#' @keywords BayesFactor
#' @export
#' @examples
sit.resids <- function(u,level, loo.counts, data, prior){
  expct <- rep(NA, length(loo.counts[[u]]))
  obsv <- rep(NA, length(loo.counts[[u]]))
  for (i in 1:length(loo.counts[[u]])){
    counts <- unlist(loo.counts[[u]][i])
    stage.counts <- unlist(data[[u]])-counts
    post <- unlist(prior[[u]])+counts
    obsv[i] <- stage.counts[level]/sum(stage.counts)
    expct[i] <- post[level]/sum(post)
  }
  df <- as.data.frame(cbind(1:length(loo.counts[[u]]),obsv,expct))
  var <- (df$expct * (1-df$expct)) -expct^2
  n <- unlist(lapply(lapply(loo.counts[[u]], unlist), sum))
  df$upper <- df$expct +sqrt(var)/sqrt(n)
  df$lower <- df$expct -sqrt(var)/sqrt(n)


  ggplot() +
    geom_pointrange(df, mapping=aes(x=V1, y=expct, ymin=upper, ymax=lower), size=1,color='red') +
    geom_point(df, mapping=aes(x=V1, y=obsv), size=5, color='blue',shape=17) +
    theme_minimal()+
    xlab(expression(v[j])) + ylab(expression(E(X[e])))


}

#########for the radical example
#######do the surprise score
#' @param u
#' @param loo.counts
#' @param data
#' @param prior
#' @keywords
#' @export
#' @examples
surprise.score <- function(u, loo.counts, data, prior){#guessing this is what jim calls the bayes p-value
  score <- rep(NA, length(loo.counts[[u]]))
  counts <- rep(0, length(unlist(loo.counts[[u]][i])))
  for (i in 2:length(loo.counts[[u]])){
    new.counts <- unlist(data[[u]])-unlist(loo.counts[[u]][i-1])
    counts <- counts + new.counts
    a1 <- counts[1]
    b1 <- sum(counts[-1])
    stage.counts <- unlist(data[[u]])-unlist(loo.counts[[u]][i])#current situation counts
    n1 <- sum(stage.counts)
    y1 <- stage.counts[1]
    score[i] <-exp(lgamma(a1 + b1) - lgamma(a1) - lgamma(b1) +
                     log(choose(n1,y1)) +
                     lgamma(a1 + y1) + lgamma(b1+n1-y1) - lgamma(a1+b1+n1))#level of surprise at seeing that ratio

  }
  return(score)
}
