#simulates a summer meals example
library(bnlearn)

meals_g <- empty.graph(c('s','a','t','m'))

n <- 1000
s <- sample(c('Yes','No'),n, replace = T)
a <- sample(c('High','Low'),n, replace = T)
t <- sample(c('Yes','No'),n, replace = T)
m <- sample(c('Yes','No'),n, replace = T)
meals <- data.frame(cbind(s,a,t,m))
colnames(meals) <- c('s','a','t','m')

arcs(meals_g) = matrix(c('s','a','s','m','a','t','t','m'), ncol=2,byrow=T,dimnames=list(c(),c('from','to')))

wrong = gs(meals)
wrong = set.arc(wrong, 's','a')
wrong = set.arc(wrong, 's','m')
wrong = set.arc(wrong, 'a','t')
wrong = set.arc(wrong, 't','m')

fitted = bn.fit(wrong, meals)

wrong2 <- wrong
wrong2 <- set.arc(wrong2,'s','t')

fitted2 =bn.fit(wrong2,meals)

#overwrite all the wrong c
new.cpt.s = as.table(matrix(c(0.3,.7), byrow=T, ncol=2,dimnames=list(B=c(), a=c('No','Yes'))))
new.cpt.a = as.table(matrix(c(.3,.8,.7,.2),byrow = T, ncol=2,dimnames = list(a=c('High','Low'), s=c('No','Yes'))))
new.cpt.t = as.table(matrix(c(.5,.8,.5,.2),byrow = T, ncol=2,dimnames = list(t=c('No','Yes'), a=c('High','Low'))))
#new.cpt.m = list(as.table(matrix(c(.7,.3,.3,.7),byrow = T, ncol=2,dimnames = list(M=c('No','Yes'), S=c('No','Yes')))),
#                          as.table(matrix(c(.3,.9,.7,.1),byrow = T, ncol=2,dimnames = list(M=c('No','Yes'), S=c('No','Yes')))))

new.cpt.m <- array(
  c(.7,.3,.3,.7,.3,.7,.1,.9), dim = c(2,2,2),
  dimnames = list(m=c('No','Yes'), s=c('No','Yes'), t=c('No','Yes')))

#cpts for wrong2
new.cpt.t2 <- array(c(.2,.8,.3,.7,.1,.9,.3,.7), dim = c(2,2,2),
  dimnames = list(t=c('No','Yes'), s=c('No','Yes'), a=c('High','Low')))

new.cpt.m2 <- array(
  c(.7,.3,.3,.7,.3,.7,.1,.9), dim = c(2,2,2),
  dimnames = list(m=c('No','Yes'), s=c('No','Yes'), t=c('No','Yes')))

fitted$s = new.cpt.s
fitted$a = new.cpt.a
fitted$t = new.cpt.t
fitted$m = new.cpt.m

sim=rbn(fitted,1000)

fitted2$s = new.cpt.s
fitted2$a = new.cpt.a
fitted2$t = new.cpt.t2
fitted2$m = new.cpt.m2

sim2=rbn(fitted2,1000)

simulated_data <- rbind(sim,sim2)
write.csv(simulated_data,'summermeals_sim.csv',sep=',')
