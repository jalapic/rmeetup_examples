### Animation 4.

detach(package:sna)
detach(package:network)
detach(package:igraph)

library(igraph)

N=100
set.seed(11)
g <- sample_forestfire(N, fw.prob=0.3,bw.factor=.9,directed=F)
V(g)$color <- cluster_edge_betweenness(g)$membership
E(g)$time <- 1:length(E(g))


ti <- 1
gt <- delete_edges(g,which(E(g)$time > ti)) #remove edges which are not present

#generate first layout using graphopt with normalized coordinates. This places the initially connected set of nodes in the middle. If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)




#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10 
#of the time (i.e. 10 snapshots) between adding two consecutive nodes 
dt <- 0.1


#setwd for output
setwd("C:/Users/curley1/Dropbox/Work/R/RMeetup/presentation/gists/gistimg")


#Output for each frame will be a png with HD size 800x450 :)
png(file="output_%03d.png", width=800,height=450)


#Time loop starts
for(time in seq(1, total_time,dt)){
  
  #remove edges which are not present
  gt <- delete_edges(g,which(E(g)$time > time))
  #with the new graph, we update the layout a little bit
  layout.new <- layout_with_fr(gt,coords=layout.old,niter=10,start.temp=0.05,grid="nogrid")
  #plot the new graph
  plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,asp=9/16,margin=-0.15)
  #use the new layout in the next round
  layout.old <- layout.new 
}
dev.off()

