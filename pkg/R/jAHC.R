#AHC algorithm, only works for symmetric trees
#Algorithm is started at the finest partition of the CEG C_{0}
#Priorlist1: list of the parameters of the prior Dirichlet distribution for each stage of C_{0},
# i.e. list(prior for u_{0}, prior for u_{1},...,prior for u_{18})

jCEG.AHC<-function(indata,priorN=3){
  no.vars<-dim(indata)[2]
  no.cat<-c()
  for(k in 1:no.vars){ no.cat<-c(no.cat,nlevels(indata[,k]))}
  numb<-c(1)
  for(i in 2:no.vars){ numb<-c(numb,prod(no.cat[1:(i-1)])) }
  prior<-c() # set up prior distribution on vertices
  for(i in 1:no.vars){
    for(j in 1:numb[i]){
      prior<-c(prior,list(rbind(rep(priorN/(no.cat[i]*numb[i]),no.cat[i]))))}
  }
  #Datalist1: list of the number of individuals going from the stage along a particular edge in C_{0}
  data<-c(list(rbind(table(indata[,1]))))
  for (i in 2:no.vars){
    for (j in 1:numb[i]){ data<-c(data,list(rbind(ftable(indata[,1:i])[j,])))   }
  }
  #outlist<-list(prior=prior,data=data)
  #return(outlist)
  #}
  #test
  #List of the stages that can be merged in the first step
  comparisonset<-c()
  for (i in 2:no.vars){
    comparisonset<-c(comparisonset,list(c((sum(numb[1:(i-1)])+1):(sum(numb[1:i]))))) }
  # set up labels  using the factors from the data set.
  labelling<-c() 
  for (k in 1:(no.vars-1)){ #  stop before the outcome variable
    label<-c(1,rep("NA",sum(numb[1:k])-1))
    label<-c(label,rep(levels(indata[,k]),numb[k]))
    if (k<(no.vars-1)){
      for (i in (k+1):(no.vars-1)){
        label<-c(label,rep(levels(indata[,k]),each=numb[i+1]/numb[k+1],numb[k+2]/no.cat[k]))
      }
    }
    labelling<-cbind(labelling,label)
  }
  mergedlist<-c()   # sum(numb) is the number of vertices, excluding the final leaves.
  for (i in 1:sum(numb)){mergedlist<-c(mergedlist,list(labelling[i,]))} # set up labes.
  merged1<-c()
  lik<-0    # initialise the likelihood value. 
  pach<-c()
  for(i in 1: sum(numb)){
    alpha<-unlist(prior[i]) 
    N<-unlist(data[i]) 
    contr<- sum(lgamma(alpha+N)-lgamma(alpha))+sum(lgamma(sum(alpha))-lgamma(sum(alpha+N))) 
    lik<-lik+contr
    pach <- c(pach, contr)
  }
  score<-c(lik)
  #At each step we calculate the difference between the current CEG and the CEG in which two stages in the current comparison set have been merged.
  #We go through every possible combination of stages that can be merged. k is an index for the comparisonset we are in,
  #and i and j the position of the stages within the comparison set.
  diff.end<-1 #to start the algorithm
  while(diff.end>0){#b0  #We stop when no positive difference is obtained by merging two stages
    difference<-0
    for (k in 1:length(comparisonset)){ #b1
      if(length(comparisonset[[k]])>1){ #b2   #can only merge if more than one stage in the comparisonset
        for (i in 1:(length(comparisonset[[k]])-1)){ #b3
          for (j in (i+1):length(comparisonset[[k]])){ #b4
            #to compare
            compare1<-comparisonset[[k]][i] 
            compare2<-comparisonset[[k]][j]
            #we calculate the difference between the CEG where two stages are merged
            result<-lgamma(sum(prior[[compare1]]+prior[[compare2]]))-lgamma(sum(prior[[compare1]]+data[[compare1]]+prior[[compare2]]+data[[compare2]]))+
              sum(lgamma(prior[[compare1]]+data[[compare1]]+prior[[compare2]]+data[[compare2]]))-sum(lgamma(prior[[compare1]]+prior[[compare2]]))-
              #and the CEG where the two stages are not merged
              (lgamma(sum(prior[[compare1]]))-lgamma(sum(prior[[compare1]]+data[[compare1]]))+sum(lgamma(prior[[compare1]]+data[[compare1]]))-
                 sum(lgamma(prior[[compare1]]))+lgamma(sum(prior[[compare2]]))-lgamma(sum(prior[[compare2]]+data[[compare2]]))+
                 sum(lgamma(prior[[compare2]]+data[[compare2]]))-sum(lgamma(prior[[compare2]])))
            #if the resulting difference is greater than the current difference then we replace it
            if (result > difference){ #b5
              difference<-result
              merged<-c(compare1,compare2,k)
            }#b5e
          }#b4e
        }#b3ei
      }#b2e
    }#b1e
    diff.end<-difference
    
    #We update our priorlist, datalist and comparisonset to obtain the priorlist, datalist and comparisonlist for C_{1}
    if(diff.end>0){ #b6
      prior[[merged[1]]]<-prior[[merged[1]]]+prior[[merged[2]]]
      prior[[merged[2]]]<-cbind(NA,NA)
      data[[merged[1]]]<-data[[merged[1]]]+data[[merged[2]]]
      data[[merged[2]]]<-cbind(NA,NA)
      comparisonset[[merged[3]]]<-comparisonset[[merged[3]]][-(which(comparisonset[[merged[3]]]==merged[2]))]
      mergedlist[[merged[1]]]<-cbind(mergedlist[[merged[1]]],mergedlist[[merged[2]]])
      mergedlist[[merged[2]]]<-cbind(NA,NA)
      lik<-lik+diff.end
      score<-c(score,lik)
      merged1<-cbind(merged1,merged)
    }#b6e
  }#b0e
  #Output: stages of the finest partition to be combined to obtain the most probable CEG structure
  stages<-c(1)
  for (i in 2:no.vars) { stages<-c(stages,comparisonset[[i-1]]) }
  result<-mergedlist[stages]
  newlist<-list(no.cat=no.cat, no.vars=no.vars, prior=prior,data=data,stages=stages,result=result,score=score, merged=merged1,comparisonset=comparisonset,mergedlist=mergedlist,lik=lik,pach=pach)
  return(newlist)
}# end of function


