# ndtv & networkDynamic

detach(package:sna)
detach(package:network)
detach(package:igraph)

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



library(networkDynamic)
library(ndtv)
## Animation 6.

# use 'net' made in ndtv_interactivity
detach(package:igraph)

net #30 nodes, #44 edges

vs <- data.frame(onset=0, terminus=45, vertex.id=1:30)
es <- data.frame(onset=1:45, terminus=45, 
                 head=as.matrix(net, matrix.type="edgelist")[,1],
                 tail=as.matrix(net, matrix.type="edgelist")[,2])

net.dyn <- networkDynamic(base.net=net, edge.spells=es, vertex.spells=vs)


plot(net.dyn, vertex.cex=(net %v% "size")/7, vertex.col="col")

# Show time evolution through static images at different time points:
filmstrip(net.dyn, displaylabels=F, mfrow=c(4, 8),
          slice.par=list(start=0, end=45, interval=5, 
                         aggregate.dur=5, rule='any'))



compute.animation(net.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=45, interval=2, 
                                 aggregate.dur=1, rule='any'))


render.d3movie(net.dyn, usearrows = F, displaylabels = F, label=net %v% "id",
               bg="#111111", 
               #vertex.border="#ffffff", 
               vertex.col =  net %v% "col",
               vertex.cex = function(slice){ degree(slice)/2.5 },  
               edge.lwd = (net %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net %v% 'id') , "<br>",
                                      "<b>Gender:</b>", (net %v% 'sex')),
               edge.tooltip = paste("<b>Edge type:</b>", (net %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net %e% "weight" ) ),
               launchBrowser=F, filename="NetworkDynamic2.html",
               render.par=list(tween.frames = 15, show.time = F), 
               script.type='remoteSrc')

