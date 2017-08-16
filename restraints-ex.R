#just adding comments
#data cleaning of restraint dataset from ProPublica 
library(ggplot2); library(dplyr);library(tidyr)
rm(list=ls())
df<- read.csv("/home/rachel/Documents/data/restraints_seclusions.csv")

df[,9:17] <- apply(df[,9:17],2,as.numeric  )
df$dscpln <- rowSums(df[,9:17])


filter(df, dscpln > 5)  %>% 
  gather(key , value ,9:17) %>% 
  filter(value!=0)-> df_melt 

df_melt$restraint.type <- as.character(lapply(strsplit(as.character(df_melt$key), split="_"),tail, n=1))
df_melt$restraint.type[which(df_melt$key == "idea_mech_restraints" | df_melt$key == "section_504_mech_restraints" | df_melt$key == "no_dis_mech_restraints")] <- "mech_restraints"
df_melt$restraint.type[which(df_melt$key == "idea_phys_restraints" | df_melt$key == "section_504_phys_restraints" | df_melt$key == "no_dis_phys_restraints")] <- "phys_restraints"

df_melt$justice <- ifelse(df_melt$justice_facility=='', "NO", "YES")

df_melt[, c(9,17,14,16,13)] -> restraint_df

#tally up the counts for the dataset for the saturated tree
#list elicited stages by hand

get.priors <- function(df) {
  
  group_by(df,justice) %>% count() -> u0
  filter(df, justice=="NO") %>% group_by( justice, disability) %>% count() -> u1
  filter(df, justice=="YES") %>% group_by( justice, disability) %>% count() -> u2
  filter(df, justice=="NO", disability=="idea") %>% group_by( justice, restraint.type) %>% count() -> u3
  filter(df, justice=="NO", disability=="no") %>% group_by( justice, restraint.type) %>% count() -> u4
  filter(df, justice=="NO", disability=="section") %>% group_by( justice, restraint.type) %>% count() -> u5
  filter(df, justice=="YES", disability=="idea") %>% group_by( justice, restraint.type) %>% count() -> u6
  filter(df, justice=="YES", disability=="no") %>% group_by( justice, restraint.type) %>% count() -> u7
  filter(df, justice=="YES", disability=="section") %>% group_by( justice, restraint.type) %>% count() -> u8
  
  priors <- list(u0$n, u1$n, u2$n, u3$n, u4$n, u5$n, u6$n, u7$n, u8$n)

  return(priors)
}

#set the reference prior as the prior for the entire dataset 
#determine the difference for each of the states--does this hold for entire cohorots?

restraint_priors <- get.priors(restraint_df)
df_melt[, c(1,17,14,16,13)]-> all.state.restraint.df
restraint_priors_tx <- get.priors(filter(all.state.restraint.df, State=='TX'))
restraint_priors_ny <- get.priors(filter(all.state.restraint.df, State=='NY'))


ggplot(df_melt, aes(x=State,y=value))+ geom_bar('stat')


#determine the differnece the reference prior and the missing data 
restraint_priors_dirty <- get.priors(filter(restraint_df, propublica_dirty=='t'))

library(DiagrammeR)
#plot the elicited tree 
grViz("
digraph ET {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]

  node [shape = circle,
        fixedsize = true,
        width = 0.9] // sets as circles
  u0; u1; u2; u3; u4; u5; u6; u7; u8

  # several 'edge' statements
  u0->u1 [label = 'Justice']
  u0->u2 [label = 'School']
  u1->u3 [label = 'Disability']
  u1->u4 [label = 'Section 504']
  u1->u5 [label = 'No Disability']
  u2->u6 [label = 'Disability']
  u2->u7 [label = 'Section 504']
  u2->u8 [label = 'No Disability']
}
")
#effective sample size is 3
#18 leaves of the tree
#read in the source code 
source("/home/rachel/Documents/chain-event-graphs/jTree.r")
source("/home/rachel/Documents/chain-event-graphs/jAHC.ceg.r")
#cleaned up the columns, ready to find a CEG that fits

C0 <-  Tree(restraint_df,32) # first argument is the data, the second is the prior 'sample size'
# as there are 8 'leaves' to the tree, I used 16 in prior: default is uniform
C0$tdata # shows the vertices and numbers on the tree
C0$prior # shows the prior. 

ceg1 <- jCEG.AHC(restraint_df,32)  # fit a CEG for member weeks 14, and 26
# as there are 8 'leaves' to the tree, I used 16 in prior.
names(ceg1) # I will explain each of these in Data1.cegB.lst.
ceg1$stages # Which of the initial vertices have become stages.
ceg1$result # Shows the factors which have been combined.
ceg1$score  # the score (log likelihood) at each step
ceg1$lik    # the final log likelihood
ceg1$merged # table to show which stages are merged at each step.
ceg1$mergedlist # This is the full tree, with 'NA' where vertices have been merged.
ceg1$prior # WARNING this is the 'final prior', not the initial prior.
ceg1$data  # FINAL data at each stage









