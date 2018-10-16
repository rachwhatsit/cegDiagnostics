CEG.AHC <- function(exampledata = exampledata ,
                    equivsize = 3) {
  exampledata <- exampledata
  equivsize <- 
    max(apply(exampledata, 2, function(x) {#reference prior as the highest number of levels of categories
      length(levels(as.factor(x)))
    })) #total number of pathways in the CEG
  numbvariables <- dim(exampledata)[2]
  numbcat <- c()
  for (k in 1:numbvariables) {
    numbcat <- c(numbcat , nlevels(exampledata[, k]))
  }
  numb <- c(1)
  
  for (i in 2:numbvariables) {
    numb <- c(numb , prod(numbcat [1:(i - 1)]))
  }
  prior <- c()
  for (i in 1:numbvariables) {
    for (j in 1:numb[i]) {
      prior <-
        c(prior , list(rbind(rep(
          equivsize / (numbcat[i] * numb[i]), numbcat[i]
        ))))
    }
  }
  #Datalist1:list of the number of individuals going from the stage along a particular edge in C_{0}
  
  data <- c(list(rbind(table(exampledata [, 1]))))
  for (i in 2:numbvariables) {
    for (j in 1:numb[i]) {
      data <- c(data , list(rbind(ftable(exampledata [, 1:i])[j, ])))
    }
  }
  #List of the stages that can be merged in the first step
  comparisonset <- c()
  for (i in 2:numbvariables) {
    comparisonset <-
      c(comparisonset , list(c((sum(
        numb [1:(i - 1)]
      ) + 1):(sum(
        numb [1:i]
      )))))
  }
  labelling <- c()
  for (k in 1:(numbvariables - 1)) {
    label <- c(1, rep("NA", sum(numb [1:k]) - 1))
    label <- c(label , rep(levels(exampledata[, k]), numb[k]))
    if (k < (numbvariables - 1)) {
      for (i in (k + 1):(numbvariables - 1)) {
        label <-
          c(label , rep(levels(exampledata[, k]), each = numb[i + 1] / numb[k + 1], numb[k +
                                                                                           1] / numbcat[k]))
      }
    }
    labelling <- cbind(labelling , label)
  }
  mergedlist <- c()
  for (i in 1:sum(numb)) {
    mergedlist <- c(mergedlist , list(labelling[i, ]))
  }
  merged1 <- c()
  lik <- 0
  for (i in 1:sum(numb)) {
    alpha <- unlist(prior[i])
    N <- unlist(data[i])
    lik <-
      lik + sum(lgamma(alpha + N) - lgamma(alpha)) + sum(lgamma(sum(alpha)) -
                                                           lgamma(sum(alpha + N)))
  }
  score <- c(lik)
  #At each step we calculate the difference between the current CEG and the CEG in which two stages in the current comparison set have been merged.
  #We go through every possible combination of stages that can be merged. k is an index for the comparisonset we are in,
  #and i and j the position of the stages within the comparison set.
  diff.end <- 1 #to start the algorithm
  while (diff.end > 0) {
    #We stop when no positive difference is obtained by merging two stages
    #while(length(unlist(comparisonset)) >3){
    difference <- 0
    for (k in 1:length(comparisonset)) {
      if (length(comparisonset [[k]]) > 1) {
        #can only merge if more than one stage in the comparisonset
        for (i in 1:(length(comparisonset [[k]]) - 1)) {
          for (j in (i + 1):length(comparisonset [[k]])) {
            #to compare
            compare1 <- comparisonset [[k]][i]
            compare2 <- comparisonset [[k]][j]
            #we calculate the difference between
            #the CEG where two stages are merged
            result <-
              lgamma(sum(prior[[compare1]] + prior [[compare2]])) - lgamma(sum(prior [[compare1]] + data[[compare1]] + prior [[compare2]] + data[[compare2]])) +
              sum(lgamma(prior [[compare1]] + data[[compare1]] + prior[[compare2]] + data[[compare2]])) -
              sum(lgamma(prior [[compare1]] + prior [[compare2]])) - #and the CEG where the two stages are not merged
              (
                lgamma(sum(prior [[compare1]])) - lgamma(sum(prior[[compare1]] + data[[compare1]])) +
                  sum(lgamma(prior [[compare1]] + data[[compare1]])) -
                  sum(lgamma(prior [[compare1]])) + lgamma(sum(prior[[compare2]])) - lgamma(sum(prior[[compare2]] + data[[compare2]])) +
                  sum(lgamma(prior [[compare2]] + data[[compare2]])) - sum(lgamma(prior [[compare2]]))
              )
            #if the resulting difference is greater than the current difference then we replace it
            if (result > difference) {
              difference <- result
              merged <- c(compare1 , compare2 , k)
            }
          }
        }
      }
    }
    diff.end <- difference
    #We update our priorlist , datalist and comparisonset to obtain the priorlist , datalist and comparisonlist for C_{1}
    
    if (diff.end > 0) {
      prior[[merged [1]]] <- prior [[merged [1]]] + prior [[merged [2]]]
      prior[[merged [2]]] <- cbind(NA , NA)
      data[[merged [1]]] <- data[[merged [1]]] + data[[merged [2]]]
      data[[merged [2]]] <- cbind(NA, NA)
      comparisonset [[merged [3]]] <-
        comparisonset [[merged [3]]][-(which(comparisonset [[merged [3]]] == merged [2]))]
      
      mergedlist [[merged [1]]] <-
        cbind(mergedlist[[merged [1]]] , mergedlist [[merged [2]]])
      mergedlist [[merged [2]]] <- cbind(NA , NA)
      lik <- lik + diff.end
      score <- c(score , lik)
      merged1 <- cbind(merged1 , merged)
    }
  }
  #Output: stages of the finest partition to be combined to obtain the most probable CEG structure
  stages <- c(1)
  for (i in 2:numbvariables) {
    stages <- c(stages , comparisonset [[i - 1]])
  }
  result <- mergedlist[stages]
  newlist <-
    list(
      prior = prior ,
      data = data ,
      stages = stages ,
      result = result ,
      score = score ,
      merged = merged1 ,
      comparisonset = comparisonset ,
      mergedlist = mergedlist ,
      lik = lik,
    )
  return(newlist)
}
