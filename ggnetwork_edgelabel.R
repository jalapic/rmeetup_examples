### From ggnetwork vignette:

detach(package:igraph)
detach(package:sna)
detach(package:network)

library(ggplot2) #must be >2.0.0
library(ggnetwork)
library(network)
library(sna)
library(rsvg)
library(svglite)

svglite("plot.svg", width = 11, height = 8)
# Random graph  
set.seed(1)
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
n %v% "importance" <- sample(1:3, 10, replace = TRUE)
e <- network.edgecount(n) #get total edges
set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))
set.edge.attribute(n, "day", sample(1:3, e, replace = TRUE))

# add ggrepel style edge labels
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey75", size=1) +
  geom_nodes(color = "#ffff4d", size = 12) +
  geom_nodetext(aes(label = LETTERS[ vertex.names ]),size=8) +
  geom_edgetext_repel(aes(label = day), color = "white", fill = "gray20",
                      box.padding = unit(1, "lines"),
                      label.size = .25,
                      size=6) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "gray20", color = "gray20"),
        plot.background = element_rect(fill = "gray20"),
        panel.grid = element_blank(),
        legend.position="none")

dev.off()
png::writePNG(rsvg("plot.svg"), "img/ggnetworkplot1.png")

###




