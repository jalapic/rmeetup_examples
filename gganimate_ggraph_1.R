#devtools::install_github('thomasp85/ggforce')
#devtools::install_github("dgrtwo/gganimate")
#devtools::install_github('thomasp85/ggraph')
#devtools::install_github("hadley/ggplot2")

library(ggraph)
library(ggforce)
library(gganimate)
library(ggplot2)
library(igraph)

set.seed(787)
N=100

gr <- barabasi.game(100)
#gr <- sample_forestfire(N, fw.prob=0.3,bw.factor=.9,directed=F)

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




# Now we create the graph with timebins as frame
p <- ggraph(data = lay) + 
  geom_node_point(aes(size = degree, colour = factor(group))) + 
  geom_edge_link0(aes(frame = timebins, alpha = delay, width = delay, colour = factor(node1.group)), data = gEdges(nodePar = 'group')) +  
  #  geom_edge_link0(aes(frame = timebins, alpha = delay, width = delay), edge_colour = '#ffffff') + 
  scale_edge_alpha(range = c(1, 0), guide = 'none') + 
  scale_edge_width(range = c(0.5, 1.5), trans = 'exp', guide = 'none') + 
  scale_size(guide = 'none') + 
  ggtitle('Temporal Ordering of Edge Attachment') +
  ggforce::theme_no_axes() + 
  theme(plot.background = element_rect(fill = '#000000'), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        plot.title = element_text(color = '#000000'),
        legend.position="none")

p

# And then we animate
animation::ani.options(interval=0.2)
#gg_animate(p, 'animation.gif', title_frame = FALSE)
gg_animate(p, 'animation.mp4', title_frame = FALSE)
getwd()
