library(dirmult)
cegb.prior <- get.ref.prior(df, cegb.struct, cuts, cegb.stage.key, cegb.stages)
cega.prior <- get.ref.prior(df, cega.struct, cuts, cega.stage.key, cega.stages)

bn.batch.mod1 <- bn.batch.monitor(df, child = "Events", parents = c("Social", "Economic"), parent.values = c("Low", "Low")) 
bn.batch.mod2 <- bn.batch.monitor(df, child = "Events", parents = "Social", parent.values = "Low") 
bn.batch.mod3 <- bn.batch.monitor(df, child = "Social", parents = "Economic", parent.values = "Low") 
bn.batch.mod4 <- bn.batch.monitor(df, child = "Social", parents = "Events", parent.values = "Low") 

cega.batch.monitor <- ceg.batch.monitor(df, cega.struct, cega.stage.key, cega.stages,3)
cegb.batch.monitor <- ceg.batch.monitor(df, cegb.struct, cegb.stage.key, cegb.stages,3)#substantial improvement for ceg-B

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
bn.mod1 <- bn.cndtl.node.monitor(df, parents = "Social", parent.values = "High", child = "Economic",n=50)
plot(bn.mod1[[2]])#plots the z-score

#conditional node montiors for Events
#BN-a 
bn.modA <- bn.cndtl.node.monitor(df, parents = c("Social", "Economic"), parent.values = c("Low","Low"), child = "Events",n=50)
plot(bn.modA[[1]])

bn.modA2<-bn.uncondtnl.node.monitor(df,"Events",prior=c(1,1,1), 50)
lines(bn.modA2[[1]])

#BN-b
bn.modB <- bn.cndtl.node.monitor(df, parents = "Social", parent.values = "Low", child = "Events",n=50)
plot(bn.modB[[1]]) 

#conditional node montior for Social 
#BN-c
bn.modC <- bn.cndtl.node.monitor(df, parents = "Economic", parent.values = "Low", child = "Social",n=50)
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
cega.condtnl.stage.monitor <- ceg.condtnl.stage.monitor(df, target.stage="cega.w3",condtnl.stage = "cega.w1","High",
                                                        target.cut=3,stages=cega.stages,
                                                        stage.key=cega.stage.key,struct=cega.struct,n=100)
cegb.condtnl.stage.monitor <- ceg.condtnl.stage.monitor(df, target.stage="cegb.w3",condtnl.stage = "cegb.w1",cndtnl.stage.val="High",
                                                        target.cut=3,stages=cegb.stages,
                                                        stage.key=cegb.stage.key,struct=cegb.struct,n=100)

par(mfrow=c(1,2))
plot(cega.condtnl.stage.monitor[[1]]); title("Monitor for w3 | w1=High in CEG A")
plot(cegb.condtnl.stage.monitor[[1]]); title("Monitor for w3 | w1=High in CEG B")


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
