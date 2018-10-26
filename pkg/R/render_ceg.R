#' A function to automatically visualize the CEG 
#' 
#' This funciton takes all the possible path and divides by the number of florets in each stage to find the reference prior
#'  @param df data in question
#' @param stage.key which arrows come from which stages 
#' @param stages input of stage names should be a character that ends in a numeric value
#' @keywords prior
#' @export
#' @examples
#' render.ceg()
  #get the nodes

#THE RIGHT ONE
renderCEG <- function(stage.key, df){
  cuts<-colnames(df)
  from.ceg <- rep(stage.key[[1]]$stage, length(stage.key[[2]]$stage))
  to.ceg <- c(stage.key[[2]]$stage)
  lbls <- as.vector(unlist(stage.key[[2]][colnames(df)[1]]))
  for (i in 3:length(stage.key)){
    cols <- syms(colnames(df)[1:(i-2)])
    unite(stage.key[[i]],k,!!!cols)->past; unite(stage.key[[i-1]],k,!!!cols)->current
    from.ceg.idx <- match(past$k,current$k)
    from.ceg <- c(from.ceg, stage.key[[i-1]]$stage[from.ceg.idx])#paste all of the possibilities here
    to.ceg <- c(to.ceg, stage.key[[i]]$stage)
    lbls <- c(lbls, as.vector(unlist(stage.key[[i]][colnames(df)[(i-1)]])))
  }
  from.sink <-  rep(unique(stage.key[[length(stage.key)]]$stage), length(levels(df[,length(cuts)]))) 
  to.sink <- rep("winf", length(from.sink) )
  from.vals <- c(from.ceg, from.sink)
  to.vals <- c(to.ceg, to.sink)
  lbls <- c(lbls, rep(as.vector(unlist(unique(df[,length(stage.key)]))),length(to.sink)/length(as.vector(unlist(unique(df[,length(stage.key)]))))))
  
  from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))+1
  to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))+1
  to.vals.n[is.na(to.vals.n)]<-max(na.omit(to.vals.n))+1
  test <- cbind(from.vals.n,to.vals.n)
  #test.d <- unique(test)
  stages <- unique(c(from.vals,to.vals))
  coloursfordf <- distinctColorPalette(k = length(stages), altCol = FALSE, runTsne = FALSE)
  pull(map_df(stage.key, ~distinct(.x,stage,color)),color )->clr.idx
  clrs <- c(coloursfordf[clr.idx+1],coloursfordf[length(stages)])
  nodes <- create_node_df(n=length(stages),type='a',label=stages,fillcolor=clrs)
  edges <- create_edge_df(test[,1],test[,2],label = lbls)
  grf <-create_graph(
    nodes_df = nodes,
    edges_df = edges
    )
  #graph_attrs = "layout = neato")#,
  #  node_attrs = "fontname = Helvetica",
  #  edge_attrs = "color = gray20")
  
  # View the graph
  grf %>% add_global_graph_attrs('layout', 'dot', 'graph') %>% add_global_graph_attrs("rankdir", "LR","graph") %>% render_graph()
  #render_graph(grf)
}

#uses a d3 visualization to sort through the edges
forceCEG <- function(stage.key, df){
  cuts<-colnames(df)
  from.ceg <- rep(stage.key[[1]]$stage, length(stage.key[[2]]$stage))
  to.ceg <- c(stage.key[[2]]$stage)
  lbls <- as.vector(unlist(stage.key[[2]][colnames(df)[1]]))
  for (i in 3:length(stage.key)){
    cols <- syms(colnames(df)[1:(i-2)])
    unite(stage.key[[i]],k,!!!cols)->past; unite(stage.key[[i-1]],k,!!!cols)->current
    from.ceg.idx <- match(past$k,current$k)
    from.ceg <- c(from.ceg, stage.key[[i-1]]$stage[from.ceg.idx])#paste all of the possibilities here
    to.ceg <- c(to.ceg, stage.key[[i]]$stage)
    lbls <- c(lbls, as.vector(unlist(stage.key[[i]][colnames(df)[(i-1)]])))
  }
  from.sink <-  rep(unique(stage.key[[length(stage.key)]]$stage), length(levels(df[,length(cuts)]))) 
  to.sink <- rep("winf", length(from.sink) )
  from.vals <- c(from.ceg, from.sink)
  to.vals <- c(to.ceg, to.sink)
  lbls <- c(lbls, rep("Level",length(to.sink)))
  
  from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))+1
  to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))+1
  to.vals.n[is.na(to.vals.n)]<-max(na.omit(to.vals.n))+1
  test <- cbind(from.vals.n,to.vals.n)
  #test.d <- unique(test)
  stages <- unique(c(from.vals,to.vals))
  
  nodes <- create_node_df(n=length(stages),type='a',label=stages)
  edges <- create_edge_df(test[,1],test[,2],label = lbls)
  grf <-create_graph(
    nodes_df = nodes,
    edges_df = edges)
  #graph_attrs = "layout = neato")#,
  #  node_attrs = "fontname = Helvetica",
  #  edge_attrs = "color = gray20")
  
  # View the graph
  render_graph(grf)
  nodes$id <- nodes$id-1
  edges$id <- edges$id-1
  edges$from <- edges$from-1
  edges$to <- edges$to-1
  nodes$value <- rep(1, length(nodes$id))
  edges$value <- rep(1, length(edges$id))
  nodes$group <- rep(1, length(nodes$id))
  
  forceNetwork(Links = edges, Nodes = nodes,
               Source = "from", Target = "to",
               Value = "value", NodeID = "label",Group="group",opacity = 0.8,arrows=TRUE)
}

# render.ceg.AHC <- function(stage.key){#takes as avariabe the output from jAHC.R 
#   
#   from.edges <- c()
#   to.edges <- c()
#   edge.labels <- c()
#   strt.idx=2; 
#   for (i in 1:length(sst$comparisonset)){
#   to.stg.idx <- unlist(sst$comparisonset[i])
#   to.stg.name <- u.stage[strt.idx:(strt.idx+length(unlist(sst$comparisonset[i]))-1)]
#   key <- sst$result[strt.idx:(strt.idx+length(unlist(sst$comparisonset[i]))-1)]#in labels
#   strt.idx=strt.idx + length(unlist(sst$comparisonset[i])); 
#   
#     for (j in 1:length(key)){
#       u.cntrb <- as.data.frame(key[j],stringsAsFactors = F)#pathways of contribution
#       new.edge.labels <- u.cntrb[i,]#i indicates what row we're looking at
#       names(new.edge.labels) <- NULL; new.edge.labels <- unlist(new.edge.labels)#faffing about to fix the names
#       
#       new.to.edges <- rep(to.stg.name[j], length(new.edge.labels))
#       if(i==1)
#         {new.from.edges <- rep('u0',length(new.edge.labels))}
#       else{
#         new.from.edges <- rep(NA, length(new.edge.labels))
#         for (k in 1:length(sst$comparisonset[[i-1]])){
#           yin <- t(as.data.frame(u.cntrb[1:(i-1),],stringsAsFactors=F))
#           yang <- t(as.data.frame(sst$result[[sst$comparisonset[[i-1]][k]]],stringsAsFactors = F)[1:(i-1),])
#           if(dim(yin)[2]==1){
#             new.from.edges[which(as.vector(yin) == as.vector(yang))] <- u.stage[which(w.stage==sst$comparisonset[[i-1]][k])]
#             
#           }
#           else if (dim(yin)[1]==1 && dim(yin)[2]==2){
#             yin_key <- paste0(yin[,1],yin[,2])
#             yang$key <- apply( yang[ , (1:i-1) ] , 1 , paste , collapse = "" )
#             new.from.edges[which(yin_key == yang$key)] <- u.stage[which(w.stage==sst$comparisonset[[i-1]][k])]
#             
#           }
#           else{
#             yin$key <- apply( yin[ , (1:i-1) ] , 1 , paste , collapse = "" )
#             yang$key <- apply( yang[ , (1:i-1) ] , 1 , paste , collapse = "" )
#             new.from.edges[which(yin$key %in% yang$key)] <- u.stage[which(w.stage==sst$comparisonset[[i-1]][k])]
#             
#           }
#         }
#       }nm
#       from.edges <- c(from.edges, new.from.edges)
#       to.edges <- c(to.edges, new.to.edges)
#       edge.labels <-  c(edge.labels, new.edge.labels)
#     }
#      
#     
#   }
#   
#   
#  
#   from.sink <-  rep(unique(stage.key[[length(cuts)-1]]$stage), length(levels(df[,length(cuts)]))) 
#   to.sink <- rep("winf", length(from.sink) )
#   
#   #nodes <- create_node_df(n=length(stages)+1,type=c(stages,'winf'))
#   from.vals <- c(from.root, from.ceg, from.sink)
#   to.vals <- c(to.root, to.ceg, to.sink)
#   from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))
#   to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))
#   to.vals.n[is.na(to.vals.n)]<-length(stages)+1
#   test <- cbind(from.vals.n,to.vals.n)
#   test.d <- unique(test)
#   nodes <- create_node_df(n=length(sst$stages),type='a',label=as.character(1:length(sst$stages)))
#   edges <- create_edge_df(test.d[,1],test.d[,2] )
#   grf <-create_graph(
#     nodes_df = nodes,
#     edges_df = edges)
#   #graph_attrs = "layout = neato")#,
#   #  node_attrs = "fontname = Helvetica",
#   #  edge_attrs = "color = gray20")
#   
#   # View the graph
#   render_graph(grf,output = 'LR')
# }
# 
# # render.ceg <- function(cuts, stage.key, stages){
# #   short.stages <-lapply(stages,function(x){as.character(substr(x,nchar(x)-1,nchar(x)))})#stages w labels removed
#   
#   #match(interaction(stage.key[[4]]$Social, stage.key[[4]]$Economic),interaction(stage.key[[3]]$Social, stage.key[[3]]$Economic))#how to functionalize?
#   
#   from.root.idx <- match(unlist(stage.key[[2]][cuts[1]]), unlist(stage.key[[1]][cuts[1]]))
#   from.root <- stage.key[[1]]$stage[from.root.idx]
#   to.root <- c(stage.key[[2]]$stage)
#   
#   from.ceg <- c(); to.ceg <- c()                          
#   for(i in 3:length(cuts)){##FIX THIS: need all previous labels for the pathway search applying recursive formulas in R
#     from.ceg.idx <- match(interaction(unlist(stage.key[[i]][cuts[i-2]]), unlist(stage.key[[i]][cuts[i-1]])),interaction(unlist(stage.key[[i-1]][cuts[i-2]]), unlist(stage.key[[i-1]][cuts[i-1]])))
#     from.ceg <- c(from.ceg, stage.key[[i-1]]$stage[from.ceg.idx])#paste all of the possibilities here
#     to.ceg <- c(to.ceg, stage.key[[i]]$stage)
#     }
#   
#   from.sink <-  rep(unique(stage.key[[length(cuts)-1]]$stage), length(levels(df[,length(cuts)]))) 
#   to.sink <- rep("winf", length(from.sink) )
#   
#   #nodes <- create_node_df(n=length(stages)+1,type=c(stages,'winf'))
#   from.vals <- c(from.root, from.ceg, from.sink)
#   to.vals <- c(to.root, to.ceg, to.sink)
#   from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))
#   to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))
#   to.vals.n[is.na(to.vals.n)]<-length(stages)+1
#   test <- cbind(from.vals.n,to.vals.n)
#   test.d <- unique(test)
#   nodes <- create_node_df(n=length(stages),type='a',label=unlist(short.stages))
#   edges <- create_edge_df(test.d[,1],test.d[,2] )
#   grf <-create_graph(
#       nodes_df = nodes,
#       edges_df = edges)
#   #graph_attrs = "layout = neato")#,
#     #  node_attrs = "fontname = Helvetica",
#     #  edge_attrs = "color = gray20")
#   
#   # View the graph
#   render_graph(grf,output = 'LR')
# }
# # 
# #for the extremist example, and probably a better general  model 
# render_ceg <- function(stage.key){
#   edges_df <- as.data.frame(cbind(stage.key[[1]]$fromstage, stage.key[[1]]$stage, stage.key[[1]][,1]))
#   colnames(edges_df) <- c('V1','V2','V3')
#   for (i in 2:length(stage.key)){
#     addme <-as.data.frame(cbind(stage.key[[i]]$fromstage, stage.key[[i]]$stage, stage.key[[i]][,i]))
#     colnames(addme) <- c('V1','V2','V3')
#     edges_df <- rbind(edges_df, addme)
#   }
#   edges_df$key <- paste0(edges_df[,1], edges_df[,2])
#   edges_df %>% distinct(V1,V2,V3) -> edges_df
#   nodes <- create_node_df(n=length(stages),label=stages)
#   edges<-create_edge_df(from=match(edges_df[,1], stages),to=match(edges_df[,2],stages), rel='related',label=as.character(edges_df[,3]))
#   grf <-create_graph(
#     nodes_df = nodes,
#     edges_df = edges)  
#   render_graph(grf,layout='LR')
# }

