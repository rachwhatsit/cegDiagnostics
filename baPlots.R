library(DiagrammeR)
#images for paper

#BN A

grViz("
digraph dot {
      
      graph [layout = twopi]
      
      node [shape = circle,
      color = grey]
      
      
      s [label='X@_{s}'] 
      e[label='X@_{e}'] 
      l[label='X@_{l}'] 
      h[label='X@_{h}']
      
      edge [color = grey]
      s -> {e h l}
      e -> l
      l -> h
      }")

