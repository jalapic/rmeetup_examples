library(igraph)
library(randomNames)
library(threejs)

## Function for making random name edgelist dataframe
random_name_df <- function(nlinks = 100,iter=1000,prox=10,seed=444){
set.seed(seed)
df <- data.frame(source = randomNames(iter,which.names='both', name.order = 'first.last', name.sep=' '), target = '')
df <- df[rep(seq_len(nrow(df)), sample(1:prox,nrow(df), replace=T)),]
df <- df[sample(nrow(df),nlinks),] 
df$target = sample(df$source,nrow(df), replace = T)
df = df[df[,1]!=df[,2], ] 
return(df)
}

mydf <- random_name_df()
g = graph.data.frame(mydf, directed=F) # raw graph
i <- edge.betweenness.community(g)$membership


#V(g)$group <- edge.betweenness.community(g)$membership # betweeness centrality for each node for grouping
#vertices<-data.frame('name' = unique(unlist(mydf))) # node names
#g = graph.data.frame(mydf, directed=F, vertices=vertices) # raw graph
#vertices$group = edge.betweenness.community(g)$membership # betweeness centrality for each node for grouping
#plot(g) #basic igraph plot
#graphjs(g) #link distance too great - charge too strong? - but works
#graphjs(g, repulsion=0.2)


#color names or hex(without opacity) are ok
g <- set_vertex_attr(g, "color", value=c("#e70351", "#e8fd02", "#eb03fe", "#fb9104", "#fd99ee", "#e8d97d", "#ea958a", "#fd01af")[i])
g  <- set_edge_attr(g,"color", value = "black")
g  <- set_edge_attr(g, "weight", value=3)

graphjs(g, repulsion=0.15,bg="white")
#



