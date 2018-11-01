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
  from.ceg <- rep(stage.key[[1]]$pos, length(stage.key[[2]]$pos))
  to.ceg <- c(stage.key[[2]]$pos)
  lbls <- as.vector(unlist(stage.key[[2]][colnames(df)[1]]))
  for (i in 3:length(stage.key)){
    cols <- syms(colnames(df)[1:(i-2)])
    unite(stage.key[[i]],k,!!!cols)->past; unite(stage.key[[i-1]],k,!!!cols)->current
    from.ceg.idx <- match(past$k,current$k)
    from.ceg <- c(from.ceg, stage.key[[i-1]]$pos[from.ceg.idx])#paste all of the possibilities here
    to.ceg <- c(to.ceg, stage.key[[i]]$pos)
    lbls <- c(lbls, as.vector(unlist(stage.key[[i]][colnames(df)[(i-1)]])))
  }
  from.sink <-  rep(unique(stage.key[[length(stage.key)]]$pos), length(levels(df[,length(cuts)]))) 
  to.sink <- rep("winf", length(from.sink) )
  from.vals <- c(from.ceg, from.sink)
  to.vals <- c(to.ceg, to.sink)
  lbls <- c(lbls, rep(as.vector(unlist(unique(df[,length(stage.key)]))),length(to.sink)/length(as.vector(unlist(unique(df[,length(stage.key)]))))))
  
  from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))+1
  to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))+1
  to.vals.n[is.na(to.vals.n)]<-max(na.omit(to.vals.n))+1
  test <- cbind(from.vals.n,to.vals.n)
  #test.d <- unique(test)
  positions <- unique(c(from.vals,to.vals))
  
  pull(map_df(stage.key, ~distinct(.x,stage,pos)),stage )->stages
  coloursfordf <- distinctColorPalette(k = length(unique(stages))+1, altCol = FALSE, runTsne = FALSE)
  clr.idx <- as.numeric(str_sub(stages, 2, str_length(stages)))
  clrs <- c(coloursfordf[clr.idx+1],coloursfordf[length(stages)])#plus one for the infinity node
  nodes <- create_node_df(n=length(positions),type='a',label=positions,fillcolor=clrs)
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
}


getTreeRenderable <- function(stage.key, df){
  cuts<-colnames(df)
  from.ceg <- rep(stage.key[[1]]$pos, length(stage.key[[2]]$pos))
  to.ceg <- c(stage.key[[2]]$pos)
  lbls <- as.vector(unlist(stage.key[[2]][colnames(df)[1]]))
  for (i in 3:length(stage.key)){
    cols <- syms(colnames(df)[1:(i-2)])
    unite(stage.key[[i]],k,!!!cols)->past; unite(stage.key[[i-1]],k,!!!cols)->current
    from.ceg.idx <- match(past$k,current$k)
    from.ceg <- c(from.ceg, stage.key[[i-1]]$pos[from.ceg.idx])#paste all of the possibilities here
    to.ceg <- c(to.ceg, stage.key[[i]]$pos)
    lbls <- c(lbls, as.vector(unlist(stage.key[[i]][colnames(df)[(i-1)]])))
  }
  from.sink <-  rep(unique(stage.key[[length(stage.key)]]$pos), length(levels(df[,length(cuts)]))) 
  to.sink <- rep("winf", length(from.sink) )
  from.vals <- c(from.ceg, from.sink)
  to.vals <- c(to.ceg, to.sink)
  lbls <- c(lbls, rep(as.vector(unlist(unique(df[,length(stage.key)]))),length(to.sink)/length(as.vector(unlist(unique(df[,length(stage.key)]))))))
  
  from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))+1
  to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))+1
  to.vals.n[is.na(to.vals.n)]<-max(na.omit(to.vals.n))+1
  test <- cbind(from.vals.n,to.vals.n)
  #test.d <- unique(test)
  pos <- unique(c(from.vals,to.vals))
  coloursfordf <- distinctColorPalette(k = length(pos), altCol = FALSE, runTsne = FALSE)
  pull(map_df(stage.key, ~distinct(.x,stage,color)),color )->clr.idx
  clrs <- c(coloursfordf[clr.idx+1],coloursfordf[length(pos)])
  nodes <- create_node_df(n=length(pos),type='a',label=pos,fillcolor=clrs)
  edges <- create_edge_df(test[,1],test[,2],label = lbls)
  test.df <- rbind(c(NA, 1),as.data.frame(test)) #add one for the root node
  
  test.df$color <- nodes$fillcolor[test.df$to.vals.n]
  return(test.df)
}
