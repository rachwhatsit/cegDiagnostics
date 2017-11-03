#' A function to automatically visualize the CEG 
#' 
#' This funciton takes all the possible path and divides by the number of florets in each stage to find the reference prior
#'  @param df data in question
#' @param struct
#' @param stage.key which arrows come from which stages 
#' @param stages input of stage names should be a character that ends in a numeric value
#' @keywords prior
#' @export
#' @examples
#' render.ceg()
  #get the nodes
render.ceg <- function(struct, cuts, stage.key, stages){
  short.stages <-lapply(stages,function(x){as.character(substr(x,nchar(x)-1,nchar(x)))})#stages w labels removed
  
  #match(interaction(stage.key[[4]]$Social, stage.key[[4]]$Economic),interaction(stage.key[[3]]$Social, stage.key[[3]]$Economic))#how to functionalize?
  
  from.root <- c(rep(stage.key[[1]]$stage, length(stage.key[[2]]$stage))) 
  to.root <- c(stage.key[[2]]$stage)
  
  from.ceg <- c(); to.ceg <- c()                          
  for(i in 3:length(cuts)){##FIX THIS: need all previous labels for the pathway search applying recursive formulas in R
    test.x <- unlist(stage.key[[i]][cuts[i-2]])
    test.y <- as.factor(stage.key[[i]][cuts[i-1]])
    test <- interaction(test.x, test.y)
    from.ceg <- match(interaction(stage.key[[i]][cuts[i-2]], stage.key[[i]][cuts[i-1]]))
    to.ceg <- interaction(stage.key[[i-1]][cuts[i-2]], stage.key[[i-1]][cuts[i-1]])
    }
  
  from.sink <-  rep(unique(stage.key[[length(cuts)]]$stage), length(levels(df[,length(cuts)]))) 
  to.sink <- rep("winf", (length(unique(stage.key[[length(cuts)]]$stage)))*(length(levels(df[,length(cuts)]))) )
  
  nodes <- c("winf", stages)
  edges <- c(create_edges(from = c(from.root, from.ceg, from.sink), to = c(to.root, to.ceg, to.sink)))
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges,
      graph_attrs = "layout = neato",
      node_attrs = "fontname = Helvetica",
      edge_attrs = "color = gray20")
  
  # View the graph
  render_graph(graph)
}