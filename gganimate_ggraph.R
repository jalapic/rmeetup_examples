## Animation 1.

set.seed(787)
 gr  <-  aging.prefatt.game(100, pa.exp=1, aging.exp=-3,   aging.bin=500)
 edges <- data.frame(from  = get.edgelist(gr)[,1], to = get.edgelist(gr)[,2])
 edges <- edges[order(edges$from),]
 
  edges$time <- 1:nrow(edges)
   edges$timebins <- as.numeric(cut(edges$time, breaks = 100))
   
     
     # We want that nice fading effect so we need to add extra data for the trailing
     edgesAnim <- lapply(1:10, function(i) {edges$timebins  <- edges$timebins + i; edges$delay <- i; edges})
   edges$delay <- 0
     edgesAnim <- rbind(edges, do.call(rbind, edgesAnim))
     edgesGraph <- graph_from_data_frame(edgesAnim, directed = F)
     
       
       
       
       # We use only original data for the layout
       subGr <- subgraph.edges(edgesGraph, which(E(edgesGraph)$delay == 0))
       V(subGr)$degree <- degree(subGr)
       V(subGr)$group <- cluster_edge_betweenness(subGr)$membership
       lay <- createLayout(subGr, 'igraph', algorithm = 'lgl')
       
         
         # Then we reassign the full graph with edge trails
         attr(lay, 'graph') <- edgesGraph
         head(lay)
        
          cols <- gplots::col2hex(terrain.colors(10)[-10])
 cols[lay$group]
 cols$group <- cols[lay$group]

  cols <- gplots::col2hex(terrain.colors(10)[-10])
 lay$group <- cols[lay$group]

  # Now we create the graph with timebins as frame
   p <- ggraph(data = lay) + 
     geom_node_point(aes(size = degree, colour = group)) + #, colour = '#8b4836') + 
     geom_edge_link0(aes(frame = timebins, alpha = delay, width = delay), edge_colour = '#ccf2ff') + 
     scale_edge_alpha(range = c(1, 0), guide = 'none') + 
     scale_edge_width(range = c(0.5, 1.5), trans = 'exp', guide = 'none') + 
     scale_size(guide = 'none') + 
     ggtitle('Temporal Ordering of Edge Attachment') +
     ggforce::theme_no_axes() + 
     theme(plot.background = element_rect(fill = '#000'), 
                     panel.background = element_blank(), 
                     panel.border = element_blank(), 
                     plot.title = element_text(color = '#cecece'))
 
   # And then we animate
   animation::ani.options(interval=0.1)
 #gg_animate(p, 'animation.gif', title_frame = FALSE)
   gg_animate(p, 'animation.mp4', title_frame = FALSE)
   
   
   