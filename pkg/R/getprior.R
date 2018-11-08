#' A function to set the reference prior on a CEG
#'
#' @param df data in question
#' @param struct
#' @param stage.key which arrows come from which stages
#' @param stages input of stage names should be a character that ends in a numeric value
#' @keywords prior
#' @export
#' @examples
#'  cegb.prior <- get.ref.prior(df, cegb.struct, cuts, cegb.stage.key, cegb.stages)
#'  get.ref.prior()

get.ref.prior <-
  function(df, struct, stage.key, stages) {
    cuts <- colnames(df)
    n <-
      max(apply(df, 2, function(x) {#reference prior as the highest number of levels of categories
        length(levels(as.factor(x)))
      })) #total number of pathways in the CEG
    cuts <-
      colnames(df) #each of the cuts that each variable must pass through
    alpha.bar <-
      max(apply(df, 2, function(x) {
        length(levels(as.factor(x)))
      })) #max number of categories at each level in the dataset
    obsv <-
      lapply(struct, function(x) {
        x$n
      }) #takes the observed values for each of the priors
    ref.prior <-
      list()#will have prior for each stage (9 in case of CHDS example)
    ref.prior[[1]] <-
      rep(n / dim(struct[[1]])[1], dim(struct[[1]])[1]) #initialize the prior for w0
    counter = 1
    for (k in 2:length(cuts)) {
      for (l in 1:length(unique(stage.key[[k]]$stage))) {
        counter = counter + 1 #which stage we're on
        stage <-
          stages[counter]#moving through the stages top to bottom
        in.paths <-
          stage.key[[k]][which(stage.key[[k]]$stage == stage), ]#id the incoming pathways
        stages.of.interest <-
          merge(in.paths[, 1:(k - 1)], stage.key[[(k - 1)]][, c(1:(k - 1), dim(stage.key[[(k - 1)]])[2])])$stage
        ref.prior.idx <-
          unlist(lapply(stages.of.interest, function(x) {
            as.numeric(substr(x, nchar(x), nchar(x))) + 1
          }))
        numtor <-
          sum(sapply(ref.prior[ref.prior.idx], FUN = `[[`, 1))#the 2 here is the index of the cut that we want it to pull out of the prior lists.
        denom <- length(levels(df[[colnames(df)[k]]]))#number of levels of cut we're in
        ref.prior[[counter]] <- rep(numtor / denom, denom)
      }
    }
    return(ref.prior)
  }
