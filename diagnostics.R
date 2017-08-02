library(dplyr);library(ggplot2)
setwd("/home/rachel/Documents/diagnostics/")
df<-read.csv(file = "CHDS.latentexample1.csv")

 df %>%
   group_by(Social, Economic, Events, Admission) %>%
   tally() -> counts #counts for all pathways in the CEG
 #write.csv(counts,'counts.csv')

get.zhed <- function(df, col_name, prior,n=50) {#dataframes should also be added for the counts
  #add checks to make sure that prior has same number of items as counts in dataframe
  Zm <- rep(NA, n)
  Sm <- rep(NA, n)
  Em <- rep(NA, n)
  Vm <- rep(NA, n)
  p <- rep(NA, n)
  for (i in 5:n){
    df_cut <- df[1:i,] 
    df_cut %>%
      group_by_(col_name) %>% #groups by the stage of interest
      tally() -> u1 #stage1
    counts = u1$n 
    p[i] = sum(gamma(prior))*prod(gamma(prior+counts))/(prod(gamma(prior)) * gamma(sum(prior)+sum(counts)))#prob of observing the sequence
    #p[i] = sum(gamma(prior))* gamma(prior+counts)/(prod(gamma(prior)) * gamma(sum(prior)+sum(counts)))#prob of observing each iteration
    #compute the z statistics
   Sm[i]=-log(p[i])
   Em[i]=-p[i]*log(p[i])
   Vm[i]=p[i]*(log(p[i])^2) - (Em[i])^2
   Zm[i]=sum(na.omit(Sm)) - sum(na.omit(Em)) / sqrt(sum(na.omit(Vm)))
  }
  return(list(Sm,Zm))
}

mod1 <- get.zhed(col_name = "Social", prior=c(10,1),n=10)
mod2 <- get.zhed(prior=c(.5,.5),n=10)

plot(mod2[[2]],xlab='Relevant sample size', ylab = 'Cumulative logarithmic penalty')
lines(mod1[[2]])
legend(2,25,c("Dir(1,10)", "Dir(6,4)"))
title("Diagnostics for u1")

#how do we store positions of the CEG in the dplyr function? 
df %>% group_by(Social) %>% tally() #w1
df %>% filter(Social=="High") %>% group_by(Economic) %>% tally() #w2
df %>% filter(Social=="Low") %>% group_by(Economic) %>% tally() #w3
df %>% filter(Social=="High") %>% group_by(Events) %>% tally() #w4
df %>% filter(Social=="Low", Economic=="High") %>% group_by(Events) %>% tally() #w5
df %>% filter(Social=="Low", Economic=="Low") %>% group_by(Events) %>% tally() #w6
df %>% filter(Social=="High",Events=="Low") %>% group_by(Admission) %>% tally() #w7


