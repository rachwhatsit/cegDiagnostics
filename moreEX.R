
cegb.prior <- get.ref.prior(df, cegb.struct, cuts, cegb.stage.key, cegb.stages)
cega.prior <- get.ref.prior(df, cega.struct, cuts, cega.stage.key, cega.stages)

##BATCH MONITORS 
bn.batch.mod1 <- bn.batch.monitor(df, child = "Events", parents = c("Social", "Economic"), parent.values = c("Low", "Low"), prior=c(.5,.5)) 
bn.batch.mod2 <- bn.batch.monitor(df, child = "Events", parents = "Social", parent.values = "Low", prior=c(.5,.5)) 
bn.batch.mod3 <- bn.batch.monitor(df, child = "Social", parents = "Economic", parent.values = "Low",prior=c(.5,.5)) 
bn.batch.mod4 <- bn.batch.monitor(df, child = "Social", parents = "Events", parent.values = "Low",prior=c(1/3,1/3,1/3)) 
#priors are pretty much fine for all of this stuff. what does computing the CEGs give us? 

cega.batch.monitor <- ceg.batch.monitor(df, cega.struct, cega.stage.key, cega.stages,3)#slight evidence against the prior
cega.batch.monitor <- ceg.batch.monitor(df, cega.struct, cega.stage.key, cega.stages,2)
cega.batch.monitor <- ceg.batch.monitor(df, cega.struct, cega.stage.key, cega.stages,4)
cegb.batch.monitor <- ceg.batch.monitor(df, cegb.struct, cegb.stage.key, cegb.stages,3)#strong evidence against prior
cegb.batch.monitor <- ceg.batch.monitor(df, cegb.struct, cegb.stage.key, cegb.stages,2)




mod1 <- bn.uncondtnl.node.monitor(df,col_name = "Social", prior=c(10,1),n=25)
mod2 <- bn.uncondtnl.node.monitor(df,col_name = "Social", prior=c(5,5),n=25)

plot(mod2[[1]],xlab='Relevant sample size', ylab = 'Cumulative logarithmic penalty')
lines(mod1[[1]])
legend(2,25,c("Dir(1,10)", "Dir(6,4)"))
title("Diagnostics for u1")
plot(mod2[[1]],xlab='Relevant sample size', ylab = 'z statistic')
lines(mod1[[1]])

par(mfrow=c(1,2))
plot(mod1[[1]]); title("Social, prior=(10,1)")
plot(mod2[[1]]); title("Social, prior=(5,5)")

##EXAMPLES OF BNS corresponding to BNs in the CEG book
#testex
bn.mod1 <- bn.parent.child.monitor(df, parents = "Social", parent.values = "High", child = "Economic",n=50)
plot(bn.mod1[[2]])#plots the z-score

#conditional node montiors for Events
#BN-a 
bn.modA <- bn.parent.child.monitor(df, parents = c("Social", "Economic"), parent.values = c("Low","Low"), child = "Events",n=50)
plot(bn.modA[[1]])

bn.modA2<-bn.uncondtnl.node.monitor(df,"Events",prior=c(1,1,1), 50)
lines(bn.modA2[[1]])

#BN-b
bn.modB <- bn.parent.child.monitor(df, parents = "Social", parent.values = "Low", child = "Events",n=50)
plot(bn.modB[[1]]) 

#conditional node montior for Social 
#BN-c
bn.modC <- bn.parent.child.monitor(df, parents = "Economic", parent.values = "Low", child = "Social",n=50)
plot(bn.modC[[1]]) 
#BN-d
bn.modD <- bn.cndtl.node.monitor(df, parents = "Events", parent.values = "Low", child = "Social",n=50)
plot(bn.modD[[1]]) 

par(mfrow=c(2,2))
plot(bn.modA[[1]])
title("Economic | Social=High"); xlab("Log penalty")
plot(bn.modB[[1]]) 
title("Events | Social=Low"); xlab("Log penalty")
plot(bn.modC[[1]]) 
title("Social | Economic=Low"); xlab("Log penalty")
plot(bn.modD[[1]]) 
title("Social | Events=Low"); xlab("Log penalty")

cega.uncondtnl.stage.monitor <- ceg.uncondtnl.stage.monitor(df, target.stage="cega.w3",target.cut=3,stages=cega.stages,stage.key=cega.stage.key,struct=cega.struct,n=100)
cegb.uncondtnl.stage.monitor <- ceg.uncondtnl.stage.monitor(df, target.stage="cegb.w3",target.cut=3,stages=cegb.stages,stage.key=cegb.stage.key,struct=cegb.struct,n=100)
##warnings are because for low path numbers, we don't have observations at all factor levels... TODO

par(mfrow=c(1,2))
plot(cega.uncondtnl.stage.monitor[[1]]);title("Monitor for w3 in CEG A")
plot(cegb.uncondtnl.stage.monitor[[1]]); title("Monitor for w3 in CEG B")

####EXAMPLES
cega.condtnl.stage.monitor <- ceg.child.parent.monitor(df, target.stage="cega.w3",condtnl.stage = "cega.w1",
                                                        target.cut=3,stages=cega.stages,
                                                        stage.key=cega.stage.key,struct=cega.struct,n=100)
cegb.condtnl.stage.monitor <- ceg.child.parent.monitor(df, target.stage="cegb.w3",condtnl.stage = "cegb.w1",
                                                        target.cut=3,stages=cegb.stages,
                                                        stage.key=cegb.stage.key,struct=cegb.struct,n=100)

plot(cega.condtnl.stage.monitor[[1]])
lines(cegb.condtnl.stage.monitor[[1]]) #Thelog penalty is lower for w3 in CEG A. This kind of makes sense.



##test to see if Economic and Admission are still independent in BN_A
HEuncondtnl <- bn.uncondtnl.node.monitor(df, "Admission", prior=c(.8,.2), n=50) #to compute priors take proportions? 
HEcond <- bn.cndtl.node.monitor(df, "Economic", c("Low"), "Admission") #fair enough?
plot(HEuncondtnl$Sm)
lines(HEcond$Sm)

#test dependence of Life events in BN-A 

Luncondtnl <- bn.uncondtnl.node.monitor(df, "Events", c(.3,.3,.4),n=50,learn = TRUE)
LuncondtnlNOlearn <- bn.uncondtnl.node.monitor(df, "Events", c(.3,.3,.4),n=50,learn = FALSE)
LEScond <- bn.cndtl.node.monitor(df, parents= c("Social"), c("High"), "Events",n=30,learn = TRUE)
plot(Luncondtnl[[1]])
lines(LEScond[[1]])

plot(Luncondtnl[[1]])
lines(LuncondtnlNOlearn[[1]])  

##SUMMER MEALS EXAMPLE 
#testing for independence between M/A and T/S

meal <-read.csv('summermeals_sim.csv')
df_meal <- meal[c(1:50, 1000:1050),]
MA <- bn.uncondtnl.node.monitor(meal, col_name = 't',prior =  c(1,1), n=2000)
MAuncond <- bn.cndtl.node.monitor(meal, parents = 's','Yes','t',n=2000)
MAuncond2 <- bn.cndtl.node.monitor(meal, parents = 's','No','t',n=2000)
plot(MA[[1]])
lines(MAuncond[[1]])
lines(MAuncond2[[1]])

SM <- bn.uncondtnl.node.monitor(meal, 'm', c(1,1), n=2000,TRUE)
SMuncond <- bn.cndtl.node.monitor(meal, 's','Yes','m',n=2000)
SMuncond2 <- bn.cndtl.node.monitor(meal, 's','No','m',n=2000)
plot(SM[[1]])
lines(SMuncond[[1]])
lines(SMuncond2[[1]])
