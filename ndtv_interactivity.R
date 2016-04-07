#### Interactivity

# ndtv
library(igraph)
library(intergraph)
library(network)
library(randomNames)

set.seed(5)
N=30
genders <- sample(c("Female", "Male"), N,T)

df <- data.frame(id = randomNames(N,gender=genders, name.order = "first.last", name.sep = " "),
           sex=genders)

g <- sample_forestfire(N, fw.prob=0.3,bw.factor=.9,directed=F)
net<-intergraph::asNetwork(g)
net %v% "col" <- c("green", "gold", "blue", "red", "pink")[edge.betweenness.community(g)$membership]

net %v% "sex" <- genders
net %v% 'id'<- as.character(df$id)
net %v% "sizevar" <- sample(5:15,vcount(g),T)

net %e% "type" <- sample(LETTERS[1:4],ecount(g),T) 
net %e% "weight"  <- igraph::degree(g)

library(ndtv)
render.d3movie(net, usearrows = F, displaylabels = F, bg="#111111", 
               vertex.border="#ffffff", vertex.col =  net %v% "col",
               vertex.cex = (net %v% "sizevar")/8, 
               edge.lwd = (net %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net %v% 'id') , "<br>",
                                      "<b>Gender:</b>", (net %v% 'sex')),
               edge.tooltip = paste("<b>Edge type:</b>", (net %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net %e% "weight" ) ),
               launchBrowser=T, filename="Network.html" )  
