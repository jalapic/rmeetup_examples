detach(package:igraph)
detach(package:sna)
detach(package:network)


# ggnetwork + ggiraph

library(ggplot2)
library(ggiraph)
library(network)
library(sna)
library(ggnetwork)

n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
e <- network.edgecount(n)
set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))


df<-ggnetwork(n, layout = "fruchtermanreingold", cell.jitter = 0.75)
df$tooltip <- paste0("Betweenness = ", round(betweenness(n)[df$vertex.names],2))

gg_point_1 <- ggplot(df, aes(x = x, y = y, xend=xend, color=family, yend=yend, tooltip = tooltip) )  +
  geom_edges(aes(linetype = type), color = "grey50") +
  geom_nodes(color = "black", size = 8) +
  theme_blank() +
  geom_nodetext(aes(label = LETTERS[vertex.names]), fontface = "bold") + 
  geom_point_interactive(size=5)

# htmlwidget call
ggiraph(code = {print(gg_point_1)}, width = 7, height = 6) 


