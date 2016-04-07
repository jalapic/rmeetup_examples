### Sparrow Example.

library(igraph)
library(DT)            # pretty tables
library(dplyr)
library(ggrepel)
library(svgPanZoom)    # zoom, zoom
library(SVGAnnotation) # to help svgPanZoom; it's a bioconductor package

sparrows <- read.csv("C:/Users/curley1/Dropbox/Work/phylogenies/Comparative Dominance/Shizuka_Data/Shizuka2015_RawData/Patterson1977-6.csv", stringsAsFactors=FALSE)[,-1]
sparrows <- sparrows[-8,-8]
rownames(sparrows)<-colnames(sparrows)

#Make graph with igraph
gr <- graph_from_adjacency_matrix(as.matrix(sparrows))

#att vertex attribute size according to degree centrality
V(gr)$size <- centralization.degree(gr)$res
V(gr)$power <- round(hub.score(gr)$vector,2)


#take a look
datatable(arrange(data_frame(sparrow=V(gr)$name, centrality_degree=V(gr)$size, hub_score=V(gr)$power), desc(centrality_degree)))

#ggiraph example
library(ggiraph)
sparrowdf <- data_frame(sparrow=V(gr)$name, centrality_degree=V(gr)$size, hub_score=V(gr)$power)

gg_point_0 <- ggplot(sparrowdf, aes(x = centrality_degree, y = hub_score, tooltip = sparrow, data_id = sparrow) ) + 
  geom_point_interactive(size=3) + theme_bw() + theme(text = element_text(size = rel(5.5))) +ylab("Hub Score")+
  xlab("Degree Centrality")

tooltip_css <- "background-opacity:0;font-size: 200%;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
ggiraph(code = {print(gg_point_0)}, tooltip_offx = 10, tooltip_offy = -30,tooltip_extra_css = tooltip_css, tooltip_opacity = .75,hover_css = "stroke:red;fill:red;stroke-width:7pt" )




#give all edges weight 1 (unweighted) and simplify network
E(gr)$weight <- 1
g <- simplify(gr, edge.attr.comb="sum")

#remove sna,network, igraph - issue arises if igraph loaded last as sna's is.bipartite masked by igraph
detach("package:sna", unload=TRUE)
detach("package:network", unload=TRUE)
detach("package:igraph", unload=TRUE)

library(igraph)
library(network)
library(sna)
library(ggnetwork)
# ggnetwork visualize
set.seed(777)
dat <- ggnetwork(g, layout="fruchtermanreingold", arrow.gap=0.0, cell.jitter=0)

# add a "repelling label" to the nodes with higher centrality so it's easier to see who the "top talkers" are.

ggplot() +
  geom_edges(data=dat, 
             aes(x=x, y=y, xend=xend, yend=yend),
             color='#ccccff', curvature=0.3, size=0.2, alpha=0.7) +
  geom_nodes(data=dat,
             color="#6666ff",
             aes(x=x, y=y, xend=xend, yend=yend, size=sqrt(size)),
             alpha=0.5) +
  geom_label_repel(data=unique(dat[dat$size>30,c(1,2,6)]),
                   aes(x=x, y=y, label=vertex.names), 
                   size=2, color="#3333ff") +
  theme_blank() +
  theme(legend.position="none") -> gg



# pass the ggplot object to svgPlot and svgPanZoom to make it easier to generate a huge graph but still make it explorable.

svgPanZoom(svgPlot(show(gg), height=3, width=3), 
           width="600px",
           controlIconsEnabled=TRUE)




#REF:  https://rud.is/projects/clinton_emails_01.html