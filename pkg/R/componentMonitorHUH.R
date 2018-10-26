#create a function that takes the components of the bayes factor for the whole thing. 

component.monitor <- function(data, prior){
  components <- c()
  for (i in 1:length(prior)){
    alpha <- unlist(prior[i])
    N <- unlist(data[i])
    components[i]  <- sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) - lgamma(sum(alpha + N)))
  }
  return(components)
}

getdata <- function(df, stage.key) {
  cuts <- colnames(df)
  cols <- syms(colnames(df)[1:(length(colnames(df)))])#add the last stage key on
  add.on <- length(stage.key)+1
  stage.key[[add.on]] <- count(df, !!!cols)
  stage.key[[add.on]]$stage <- rep('winf', length(stage.key[[add.on]]$n))
  counter <- 1
  data.lst <- list()
  for (i in 3:(length(cuts)+1)){
    colnames(stage.key[[(i-1)]])[i] <- "from.stage"
    colnames(stage.key[[i]])[(i+1)] <- "to.stage"
    cols <- colnames(df)[1:(i - 2)]
    trgcut <- sym(colnames(df)[(i-1)])
    full_join(stage.key[[i]],stage.key[[(i-1)]], by=cols) %>%  
      group_by(!!trgcut, from.stage) %>% summarise(cnts=sum(n.x))->data.df
      
    stgs <- unique(data.df$from.stage)
    num.stages <- length(stgs)
    
    for (j in 1:num.stages){
      counter <- counter+1
      data.lst[[counter]] <- data.df$cnts[which(data.df$from.stage==stgs[j])]
    }
    colnames(stage.key[[(i-1)]])[i] <- "stage"
    colnames(stage.key[[i]])[(i+1)] <- "stage"
    
  }
  return(data.lst)
}

# components <- function(sst, prior) {
#   idx <-
#     which(!is.na(unlist(lapply(
#       sst$data, '[[', 1
#     ))) == TRUE)
#   sst$data[idx] -> NN
#   
#   components <- c()
#   for (i in 1:length(prior)){
#     components[i]  <- score(unlist(prior[i]), unlist(NN[i]))
#   }
# }

# score <- function(alpha, N){
# sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) - lgamma(sum(alpha + N)))->p
#   return(p)
# }

for (i in 1:length(stages)){
  
}

#component monitor returns the bayes factor for a chunk of the graph
component.monitor.ceg <-
  function(df,
           target.stage,
           target.cut,
           condtnl.stage,
           stages,
           stage.key,
           struct,
           prior,
           n = 50,
           learn = FALSE) {
    cuts <- colnames(df)
    target.stage.idx <-
      as.numeric(substr(target.stage, nchar(target.stage), nchar(target.stage))) + 1
    condtnl.stage.idx <-
      as.numeric(substr(condtnl.stage, nchar(condtnl.stage), nchar(condtnl.stage))) + 1
    target.prior <- unlist(prior[target.stage.idx])
    df_cut <- df
    #figure out what stages we're watching for the target (child) stage
    in.paths <-
      stage.key[[target.cut]][which(stage.key[[target.cut]]$stage == target.stage), ]#id the incoming pathways
    stages.of.interest <-
      merge(in.paths[, 1:(target.cut - 1)], stage.key[[(target.cut - 1)]][, c(1:(target.cut -
                                                                                   1), dim(stage.key[[(target.cut - 1)]])[2])])$stage
    ref.prior.idx <-
      unlist(lapply(stages.of.interest, function(x) {
        as.numeric(substr(x, nchar(x), nchar(x))) + 1
      }))#gives the stage number, because of weird indexing, want
    in.path.idx <-
      which(stage.key[[target.cut]]$stage == target.stage)
    #figure out what stages we're watching for the condtnl (parent) stage
    cnd.in.paths <-
      stage.key[[target.cut - 1]][which(stage.key[[target.cut - 1]]$stage == condtnl.stage), ]
    cnd.in.path.idx <-
      which(stage.key[[target.cut - 1]]$stage == condtnl.stage)
    #filter out along conditional stage requirements
    df_cuts <- list()
    if (target.cut == 2) {
      #this is not doing what it should. regrettably.
      for (j in 1:length(cnd.in.path.idx)) {
        df_cuts[[j]] <- df_cut
        df_cuts[[j]] <-
          filter(df_cuts[[j]], UQ(sym(colnames(df_cut)[1])) == as.character(unlist(stage.key[[target.cut -
                                                                                                1]][cnd.in.path.idx[j], 1])))#filter according to the matching indices
      }
    } else{
      for (j in 1:length(cnd.in.path.idx)) {
        df_cuts[[j]] <- df_cut
        for (k in 1:(length(colnames(stage.key[[target.cut - 1]])) - 2)) {
          df_cuts[[j]] <-
            filter(df_cuts[[j]], UQ(sym(colnames(df_cut)[k])) == as.character(unlist(stage.key[[target.cut -
                                                                                                  1]][cnd.in.path.idx[j], k])))#filter according to the matching indices
        }
      }
    }
    df_paths.cnd <- do.call(rbind, df_cuts)
    df_cuts <- list()
    for (j in 1:length(in.path.idx)) {
      df_cuts[[j]] <- df_paths.cnd
      for (k in (length(colnames(stage.key[[target.cut - 1]])) - 1):(length(colnames(stage.key[[target.cut]])) -
                                                                     2)) {
        df_cuts[[j]] <-
          filter(df_cuts[[j]], UQ(sym(colnames(df_cut)[k])) == as.character(unlist(stage.key[[target.cut]][in.path.idx[j], k])))#filter according to the matching indices
      }
    }
    df_paths <- do.call(rbind, df_cuts)
    # df_paths <- filter(df_paths,Economic==cndtnl.stage.val)
    obsv.stage.count <-
      count(df_paths, UQ(sym(colnames(
        as.data.frame(struct[target.stage.idx])
      )[1])))#how many counts we observe in each stage
    counts <- rep(0, length(target.prior))
    counts[as.numeric(rownames(obsv.stage.count))] <-
      obsv.stage.count$n
    target.prior.vec <- unlist(target.prior)
    p = (lgamma(sum(target.prior.vec)) + sum(lgamma(target.prior.vec +
                                                      counts)) - (sum(lgamma(target.prior.vec)) + lgamma(sum(target.prior.vec) +
                                                                                                           sum(counts))))#logprobability
    #compute the z statistics
    return(p)
  }
