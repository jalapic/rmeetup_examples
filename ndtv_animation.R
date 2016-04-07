library(networkDynamic)
library(ndtv)
## Animation 6.

# use 'net' made in ndtv_interactivity
detach(package:igraph)
net
plot(net)
net #30 nodes, #44 edges

vs <- data.frame(onset=0, terminus=45, vertex.id=1:30)
es <- data.frame(onset=1:45, terminus=45, 
                 head=as.matrix(net, matrix.type="edgelist")[,1],
                 tail=as.matrix(net, matrix.type="edgelist")[,2])

net.dyn <- networkDynamic(base.net=net, edge.spells=es, vertex.spells=vs)


plot(net.dyn, vertex.cex=(net %v% "size")/7, vertex.col="col")


# Pre-compute animation coordinates
compute.animation(net.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=45, interval=1, 
                                 aggregate.dur=1, rule='any'))

# Show time evolution through static images at different time points:
filmstrip(net.dyn, displaylabels=F, mfrow=c(4, 8),
          slice.par=list(start=0, end=45, interval=5, 
                         aggregate.dur=5, rule='any'))



compute.animation(net.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=45, interval=2, 
                                 aggregate.dur=1, rule='any'))


getwd()
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

