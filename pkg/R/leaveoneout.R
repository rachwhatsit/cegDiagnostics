pi <- Vectorize(function(theta)  dbeta(theta,104.25,11.25))
pi2 <- Vectorize(function(theta)  dbeta(theta,213.25,21.25))
curve(pi, xlab=~theta, ylab="Density", main="Beta prior: a=104.25, b=11.25",lwd=2)
curve(pi, xlab=~theta, ylab="Density", main="Beta prior: a=213.25, b=21.25",lwd=2)
curve(pi2, xlab=~theta, ylab="Density", main="Beta prior: a=109.25, b=10.25",add=T, col=2,lwd=2)

u <- 4
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
surprise.score(8)
map(c(4,6:8),surprise.score)

u <- 4
level <- 2 #idx corresponding to 'worst' level
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

sit.resids(4,2)


###########FUNCTIONIZE MEEEEE

u <- 4
level <- 2 #idx corresponding to 'worst' level
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
    xlab(expression(v[j])) + ylab(expression(p(X[h])))

  # df %>% gather(key, value, -V1) %>%
  #   ggplot(aes(x=V1, y=value, colour=key,shape=key,size=0.75)) + 
  #   geom_point() +
  #   #scale_y_continuous(limits = c(0, 1)) +
  #   scale_x_continuous(breaks= 1:length(loo.counts[[u]])) +
  #   theme(legend.position = "none") + 
  #   xlab(expression(v[j])) + ylab(expression(mu))
  
}

#########for the radical example

sit.resids(8,2,chds.loo.counts, chds.data,chds.prior)
sit.resids(7,2,chds.loo.counts, chds.data,chds.prior)
sit.resids(6,2,chds.loo.counts, chds.data,chds.prior)
sit.resids(4,2,chds.loo.counts, chds.data,chds.prior)

sit.resids(32,2,radical.loo.counts, radical.data,radical.prior)
sit.resids(33,2,radical.loo.counts, radical.data,radical.prior)
sit.resids(34,2,radical.loo.counts, radical.data,radical.prior)
sit.resids(35,2,radical.loo.counts, radical.data,radical.prior)
sit.resids(36,2,radical.loo.counts, radical.data,radical.prior)
sit.resids(37,2,radical.loo.counts, radical.data,radical.prior)

#######do the surprise score
u <- 4
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
surprise.score(8, chds.loo.counts, chds.data, chds.prior)
map(c(4,6:8),surprise.score)

map(1:37, function(x) length(radical.loo.counts[[x]])) %>% unlist ->num.sits

map(which(num.sits!=1), function(x) surprise.score(x, radical.loo.counts, radical.data,radical.prior))

surprise.score(32,radical.loo.counts,radical.data,radical.prior)
    