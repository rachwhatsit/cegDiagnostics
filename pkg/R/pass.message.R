
#convert a CEG to a path structure


get.edge.path.key <- function(stage.key,cuts) {
stage.key[[length(stage.key)]] -> paths
mutate(paths, stage1 = rep(as.character(stages[1]), length(paths$n))) -> paths #initialized with first and last stages
for(i in 2:(length(stage.key)-1)){
  left_join(paths, stage.key[[i]], by=UQ(cuts[1:(i-1)])) -> paths} #this returns the paths in a wonky order, but they're there
dplyr::select(paths, c(1:(length(cuts)),starts_with("stage"))) -> edge.path.key
return(edge.path.key)}

#takes a well ordered CEG and C-copmatible information I 
#outputs: an uncolored CEG with pi_hat

evidence <- df[1:5,] #how much evidencd do you have at each time?? yo ne se.
posterior <- rep(NA, length(prior))
for (i in (1:length(prior))){posterior[i] <- list(unlist(prior[i])+unlist(struct[[i]]$n))}
post.mean <- rep(NA, length(prior))
for (i in (1:length(prior))){post.mean[i] <- list(unlist(posterior[i])/sum(unlist(posterior[i])))}


pass.message <- function(df, stage.key, evidence,post.mean,prior){#what's the most natural way to put the evidence into the system?
  #prior <- get.ref.prior(df, struct, cuts, stage.key, stages)
  tau <- c()
  for (i in 2:length(stage.key)){
    cuts <- colnames(df)
    stage.key[[i]]<-mutate(stage.key[[i]], pi = n/dim(df)[1])
    }#adds edge probabilties to each stage
  edge.path.key <- get.edge.path.key(stage.key,cuts) #determine what the edge path key is
  left_join(evidence,edge.path.key) -> ev.paths
  dplyr::select(ev.paths,-(1:length(cuts))) %>% as.list() %>% unlist() %>% unique() -> ev.stages
    
    
      sk.idx <- 1
    for (i in 1:length(prior)){
      if (!stages[[i]] %in% stage.key[[sk.idx]]$stage){sk.idx <- sk.idx+1}
      tau[[i]] <- rep(-1, length(prior[[i]]))#initialize tau
      #check to see that the evidence matches this stage
      if(! stages[[i]] %in% ev.paths[,(length(stage.key)+sk.idx)]){next}#if the stages is not in the evidence, pass on it.
      subset(ev.paths, ev.paths[,(length(stage.key)+sk.idx)]==stages[i]) -> ev.paths.stage
      
      idx <- which(unlist(struct[[i]][,1]) %in% ev.paths.stage[,sk.idx])
        tau[[i]][idx] <- post.mean[[i]][idx] #if the edge is in the evidence, then add the probability for each existing edge
        tau[[i]][-idx] <- 0
    }
    
    phi <- c()
    for (i in 1:length(tau)){phi[i] <- sum(unlist(tau[[i]]))}
    
    pi.hat <- c()
    for (i in 1:length(tau)){#GO BACK AND GET RID OF THESE FOR LOOPS LIKE A REAL CODER
      pi.hat[[i]] <- unlist(tau[[i]])/phi[i]
    }
    return(pi.hat)
}


newpi <- pass.message(df,stage.key,df[1:2,],post.mean)
newpi2 <- pass.message(df, stage.key,df[1:4,],post.mean)
newpi3 <- pass.message(df, stage.key,df[1:10,],post.mean)
newpi5 <- pass.message(df, stage.key,df[1:50,],post.mean)

#EVERYTHING IS GARBAGE BELOW HERE  
#   stages %in% ev.stages
#   
#   #find pi.hat, by taking the original probabilities of all paths in the
#   left_join(evidence,stage.key[[4]])
#   
#   #prior of CEGb 
#   #observed counts of CEGb
#   #posterior of CEGB
#   #posterior mean of CEGb for each stage
#   
#    
#   tau  <- c() #initalize tau
#   sk.idx <- as.integer(length(stage.key))
#   for (i in (1:6)){
#    #figure out what stages are in each cut
#     n = length(prior)-(i-1)#starting with the reverse orders 
#     if(!stages[n] %in% stage.key[[sk.idx]]$stage){sk.idx <- sk.idx-1}
#     left_join(stage.key[[sk.idx]],stage.key[[(sk.idx-1)]], cuts[(1:(sk.idx-2))]) %>% filter(stage.x==stages[n]) -> lookup
#     match(lookup$stage.y, stages) -> stage.idx
#     as.numeric(unlist(lookup[,sk.idx-1])) -> level.idx 
#     
#     tau[n] <- 0
#     for (i in 1:length(lookup$stage.x)){
#     tau[n] <- as.numeric(tau[n]) + as.numeric(unlist(post.mean[[stage.idx[i]]])[level.idx[i]])  
#     }
#   }
#   tau <- post.mean
#   tau[which(!stages %in% ev.stages)] <- 0 #zero out the ones that are not in the evidence
#   phi <- post.mean #initialize the potential
#   # for (i in (1:length(stages))){#stages does NOT include winf
#   #   if(sum(tau[[i]]))
#   #   n = length(stages)+1-i#do this in the reverse order, at w8 for CEG-B
#   #   #sum the post.means for the observed stages. 
#   #   if(sum(unlist(tau[[n]])==0)){tau[[n]]<-rep(-1, length(tau[[n]]))} #forget it if 
#   #   else{
#   #     left_join(stage.key[[5]],stage.key[[4]], by =cuts[1:3])
#   #     tau[n]=sum()
#   #     
#   #     
#   #     as.numeric(substr(condtnl.stage,nchar(condtnl.stage),nchar(condtnl.stage))) #figure out how the
#   #     phi[[i]] #beginning with the root node 
#   #     
#   #   }
#   #   
#   # }
#   
#   }
#   
# }
# 
# 
# tau <- c()
# for (i in 1:length(post.mean)){tau[i]<-sum(unlist(post.mean[[i]]))}
# 
# # tau.vals <- list()#sum the edge probabilites at each stage
# #  stage.key[[1]] %>% dplyr::select(-n)%>% mutate(sum = 1) -> tau.vals[[1]] #FIX THIS: only works for the root nodes
# # for (i in (2:length(cuts))){
# #   left_join(stage.key[[(i+1)]], stage.key[[i]], by=cuts[1:(i-1)]) %>% group_by(stage.y) %>% summarise(sum=sum(pi.x)) -> tau.vals[[i]]
# # }
# #   
# # tau <- c()
# # for(i in (1:length(tau.vals))){tau <- c(tau, unlist(tau.vals[[i]]$sum))}
# # 
# # phi.hat <- post.mean
# # for (i in 1:length(post.mean)){unlist(post.mean[[i]])/tau[i]}
