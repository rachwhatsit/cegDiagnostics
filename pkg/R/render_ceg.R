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


render.ceg.AHC <- function(sst, df){#takes as avariabe the output from jAHC.R 
  #short.stages <- lapply(stages,function(x){as.character(substr(x,nchar(x)-1,nchar(x)))})#stages w labels removed
  
  u.stage <- paste0(rep('u',length(sst$stages)), as.character(1:length(sst$stages))) #listed stages here
  #match(interaction(stage.key[[4]]$Social, stage.key[[4]]$Economic),interaction(stage.key[[3]]$Social, stage.key[[3]]$Economic))#how to functionalize?
  
  #do the stages for the first variable, first cut, first stage
  from.root <- rep(u.stage[1],length(sst$comparisonset[[1]])*sst$no.cat[1])
  to.root <- rep(u.stage[sst$comparisonset[[1]]],sst$no.cat[1])
  #edge.label <- c('1','2')#FIXTHIS
  edge.label <- sst$mergedlist[[2]][1,]
  
#START HERE TOMORROW
  for (i in 2:length(sst$comparisonset)){
    rep(u.stage[sst$comparisonset[[i-1]]], length(sst$comparisonset[[i]])*sst$no.cat[i]))
    from.root <- c(from.root, new.from.root)
  }
  from.root.idx <- match(unlist(stage.key[[2]][cuts[1]]), unlist(stage.key[[1]][cuts[1]]))
  from.root <- stage.key[[1]]$stage[from.root.idx]
  to.root <- c(stage.key[[2]]$stage)
  
  from.ceg <- c(); to.ceg <- c()                          
  for(i in 3:length(cuts)){##FIX THIS: need all previous labels for the pathway search applying recursive formulas in R
    from.ceg.idx <- match(interaction(unlist(stage.key[[i]][cuts[i-2]]), unlist(stage.key[[i]][cuts[i-1]])),interaction(unlist(stage.key[[i-1]][cuts[i-2]]), unlist(stage.key[[i-1]][cuts[i-1]])))
    from.ceg <- c(from.ceg, stage.key[[i-1]]$stage[from.ceg.idx])#paste all of the possibilities here
    to.ceg <- c(to.ceg, stage.key[[i]]$stage)
  }
  
  from.sink <-  rep(unique(stage.key[[length(cuts)-1]]$stage), length(levels(df[,length(cuts)]))) 
  to.sink <- rep("winf", length(from.sink) )
  
  #nodes <- create_node_df(n=length(stages)+1,type=c(stages,'winf'))
  from.vals <- c(from.root, from.ceg, from.sink)
  to.vals <- c(to.root, to.ceg, to.sink)
  from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))
  to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))
  to.vals.n[is.na(to.vals.n)]<-length(stages)+1
  test <- cbind(from.vals.n,to.vals.n)
  test.d <- unique(test)
  nodes <- create_node_df(n=length(sst$stages),type='a',label=as.character(1:length(sst$stages)))
  edges <- create_edge_df(test.d[,1],test.d[,2] )
  grf <-create_graph(
    nodes_df = nodes,
    edges_df = edges)
  #graph_attrs = "layout = neato")#,
  #  node_attrs = "fontname = Helvetica",
  #  edge_attrs = "color = gray20")
  
  # View the graph
  render_graph(grf,output = 'LR')
}

render.ceg <- function(cuts, stage.key, stages){
  short.stages <-lapply(stages,function(x){as.character(substr(x,nchar(x)-1,nchar(x)))})#stages w labels removed
  
  #match(interaction(stage.key[[4]]$Social, stage.key[[4]]$Economic),interaction(stage.key[[3]]$Social, stage.key[[3]]$Economic))#how to functionalize?
  
  from.root.idx <- match(unlist(stage.key[[2]][cuts[1]]), unlist(stage.key[[1]][cuts[1]]))
  from.root <- stage.key[[1]]$stage[from.root.idx]
  to.root <- c(stage.key[[2]]$stage)
  
  from.ceg <- c(); to.ceg <- c()                          
  for(i in 3:length(cuts)){##FIX THIS: need all previous labels for the pathway search applying recursive formulas in R
    from.ceg.idx <- match(interaction(unlist(stage.key[[i]][cuts[i-2]]), unlist(stage.key[[i]][cuts[i-1]])),interaction(unlist(stage.key[[i-1]][cuts[i-2]]), unlist(stage.key[[i-1]][cuts[i-1]])))
    from.ceg <- c(from.ceg, stage.key[[i-1]]$stage[from.ceg.idx])#paste all of the possibilities here
    to.ceg <- c(to.ceg, stage.key[[i]]$stage)
    }
  
  from.sink <-  rep(unique(stage.key[[length(cuts)-1]]$stage), length(levels(df[,length(cuts)]))) 
  to.sink <- rep("winf", length(from.sink) )
  
  #nodes <- create_node_df(n=length(stages)+1,type=c(stages,'winf'))
  from.vals <- c(from.root, from.ceg, from.sink)
  to.vals <- c(to.root, to.ceg, to.sink)
  from.vals.n <- as.numeric(gsub("[^\\d]+", "", from.vals, perl=TRUE))
  to.vals.n <- as.numeric(gsub("[^\\d]+", "", to.vals, perl=TRUE))
  to.vals.n[is.na(to.vals.n)]<-length(stages)+1
  test <- cbind(from.vals.n,to.vals.n)
  test.d <- unique(test)
  nodes <- create_node_df(n=length(stages),type='a',label=unlist(short.stages))
  edges <- create_edge_df(test.d[,1],test.d[,2] )
  grf <-create_graph(
      nodes_df = nodes,
      edges_df = edges)
  #graph_attrs = "layout = neato")#,
    #  node_attrs = "fontname = Helvetica",
    #  edge_attrs = "color = gray20")
  
  # View the graph
  render_graph(grf,output = 'LR')
}

#for the extremist example, and probably a better general  model 
render_ceg <- function(stage.key){
  edges_df <- as.data.frame(cbind(stage.key[[1]]$fromstage, stage.key[[1]]$stage, stage.key[[1]][,1]))
  colnames(edges_df) <- c('V1','V2','V3')
  for (i in 2:length(stage.key)){
    addme <-as.data.frame(cbind(stage.key[[i]]$fromstage, stage.key[[i]]$stage, stage.key[[i]][,i]))
    colnames(addme) <- c('V1','V2','V3')
    edges_df <- rbind(edges_df, addme)
  }
  edges_df$key <- paste0(edges_df[,1], edges_df[,2])
  edges_df %>% distinct(V1,V2,V3) -> edges_df
  nodes <- create_node_df(n=length(stages),label=stages)
  edges<-create_edge_df(from=match(edges_df[,1], stages),to=match(edges_df[,2],stages), rel='related',label=as.character(edges_df[,3]))
  grf <-create_graph(
    nodes_df = nodes,
    edges_df = edges)  
  render_graph(grf,layout='LR')
}


# df <- data.frame(col1 = c("Cat", "Dog", "Bird"),
#                  col2 = c("Feline", "Canis", "Avis"),
#                  stringsAsFactors = FALSE)
# uniquenodes <- unique(c(df$col1, df$col2))
# 
# uniquenodes
# 
# library(DiagrammeR)
# 
# nodes <- create_node_df(n=length(uniquenodes), 
#                         type="number", 
#                         label=uniquenodes)
# edges <- create_edge_df(from=match(df$col1, uniquenodes), 
#                         to=match(df$col2, uniquenodes), 
#                         rel="related")
# g <- create_graph(nodes_df=nodes, 
#                   edges_df=edges)
# render_graph(g)
