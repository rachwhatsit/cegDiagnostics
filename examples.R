#chds

#radical
radical.df<- read.csv(file = "data1.csv")
sst <- jCEG.AHC(chds.df)
radical.sst <- jCEG.AHC(radical.df) 

radical.stage.key <- tostagekey(radical.df, radical.sst)

radical.stage.key %>% 
  map_df("stage", n_distinct)  
map_at(radica)
(n_distinct(radical.stage.key[[4]]$stage))
#create a table with data frames and what cut they're in
map_df(radical.stage.key, ~distinct(.x,stage)) -> radical.stages
map()
n_distinct(radical.stage.key[[4]])
map_df(radical.stage.key, stage)

#merge this with the observed results 
#store the column names as the struct 




stages <- lapply(unique(radical.stage.key[[1]]$stage)
radical.stages <- 
to.struct <-function(df, stage.key, sst){
radical.sst$data -> radstr
radstr[!is.na(unlist(lapply(radstr, '[[', 1)))]->radical.struct
radical.sst$result[9]#previous rows, colnames(df) has rownames
  
for(i in 2:length(radical.struct)){
  radical.sst$result[[i]]
}
}


radstr[!is.na(unlist(lapply(radstr, '[[', 1)))]->radical.struct
ceg.child.parent.monitor(radical.df,target.stage = 3,condtnl.stage = "w2",struct = radical.struct,stage.key = radical.stage.key,n = 50,learn = F)
#best 
