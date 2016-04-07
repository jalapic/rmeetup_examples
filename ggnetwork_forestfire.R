# may need to detach sna/network if have them loaded

detach(package:igraph)
detach(package:sna)
detach(package:network)

library(igraph)
library(viridis)
library(ggnetwork)
library(ggplot2)

set.seed(56)
g <- sample_forestfire(99, fw.prob=0.3,bw.factor=.9)
V(g)$Group <- LETTERS[as.numeric(cut(V(g), 3))]

ggplot(ggnetwork(g,arrow.gap=0,layout = "fruchtermanreingold", cell.jitter = 0.05), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodes(aes(color = Group), size = 8) + 
  geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
  scale_color_viridis(discrete=T, option="D",begin=.5) +
  theme_blank() +
  guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))

