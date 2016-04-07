### Plots made in Presentation


library(svglite)
library(rsvg)

getwd()
setwd("C:/Users/James Curley/Dropbox/Work/R/RMeetup/presentation")
setwd("C:/Users/curley1/Dropbox/Work/R/RMeetup/presentation")

kohda <- read.csv("C:/Users/James Curley/Dropbox/Work/phylogenies/Comparative Dominance/Shizuka_Data/Shizuka2015_RawData/Kohda1991-2.csv", stringsAsFactors=FALSE)[,-1]

rownames(kohda)<-colnames(kohda)
dim(kohda)
kohda[1:9,1:9]

depth <- c(rep("shallow",2), "mid", rep("shallow",5), rep("mid",2), "shallow", "mid", rep("deep",2), rep("mid",4), "shallow", rep("deep",2), "mid", rep("deep",2), "shallow", rep("mid",5), "shallow")

library(ggnet)
library(network)
library(sna)
library(ggplot2)
library(RColorBrewer)

kohda.net <- as.network.matrix(kohda, directed=T) #make network object
kohda.net %v% "depth" = depth  #add vertex attribute
kohda.net

###

svglite("plot.svg", width = 11, height = 8)
set.seed(576)
ggnet2(kohda.net, 
       node.size = 8, node.color = "depth", color.palette = "Set2", color.legend = "Water Depth", 
       edge.size = 1, edge.color = "black",
       arrow.size = 9,  arrow.gap = 0.03)
dev.off()
png::writePNG(rsvg("plot.svg"), "img/cichlids.png")

###

svglite("plot.svg", width = 11, height = 8)
set.seed(576)
ggnet2(kohda.net, 
       node.size = 8, node.color = "depth", color.palette = "Set2", color.legend = "Water Depth", 
       edge.size = 1, edge.color = "black",
       arrow.size = 9,  arrow.gap = 0.03)
dev.off()
png::writePNG(rsvg("plot.svg"), "img/cichlids.png")

###
#legend.text argument controls the size of the legends symbols, text labels and title, 
#legend.position argument controls its placement:

svglite("plot.svg", width = 11, height = 8)
set.seed(576)
ggnet2(kohda.net, 
       node.size = 8, node.color = "depth", color.palette = "Set2", color.legend = "Water Depth", 
       edge.size = 1, edge.color = "black",
       arrow.size = 9,  arrow.gap = 0.027, 
       legend.size=20)  + 
       guides(color=guide_legend(keyheight=0.5,default.unit="inch",override.aes = list(size=6)))
dev.off()
png::writePNG(rsvg("plot.svg"), "img/cichlids.png")

###


svglite("plot.svg", width = 11, height = 8)
set.seed(576)
ggnet2(kohda.net, 
       node.size = 8, node.color = "depth", color.palette = "Set2", color.legend = "Water Depth", 
       edge.size = 1, edge.color = c("color", "gray88"),
       arrow.size = 9,  arrow.gap = 0.027, 
       legend.size=20)  + 
  guides(color=guide_legend(keyheight=0.5,default.unit="inch",override.aes = list(size=6)))
dev.off()
png::writePNG(rsvg("plot.svg"), "img/cichlids1.png")

#[1] which attr to color on, [2] which color to code nodes between groups

######

svglite("plot.svg", width = 11, height = 8)
set.seed(576)
ggnet2(kohda.net, 
       size = "outdegree", size.cut = 4, 
       node.color = "depth", 
       color.palette = "Set2",
       edge.size = 1, edge.color = c("color", "gray88"),
       arrow.size = 9,  arrow.gap = 0.022
       )  + 
       guides(color = FALSE, size = FALSE)
dev.off()
png::writePNG(rsvg("plot.svg"), "img/cichlids2.png")

###
# ggnetwork + ggiraph

library(ggplot2)
library(rvg)
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


###

library(igraph)
library(viridis)
library(ggnetwork)

set.seed(56)
g <- sample_forestfire(99, fw.prob=0.3,bw.factor=.9)
V(g)$Group <- LETTERS[as.numeric(cut(V(g), 3))]


svglite("plot.svg", width = 11, height = 8)
ggplot(ggnetwork(g,arrow.gap=0,layout = "fruchtermanreingold", cell.jitter = 0.05), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodes(aes(color = Group), size = 8) + 
  geom_nodetext(aes(label = vertex.names),color="black", fontface = "bold") +
  scale_color_viridis(discrete=T, option="D",begin=.5) +
  theme_blank() +
  guides(color=guide_legend(keyheight=0.3,default.unit="inch",override.aes = list(size=6)))
dev.off()
png::writePNG(rsvg("plot.svg"), "img/forestfire.png")


###

