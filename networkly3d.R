### networkly: network visualization in R using Plotly

#devtools::install_github("dgrapov/networkly")
library(networkly)
library(plotly)
library(igraph)
library(randomNames)

## Function for making random name edgelist dataframe
random_name_df <- function(nlinks = 150,iter=1000,prox=10,seed=444){
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
max(i)



edge.list<-data.frame(source=mydf[,1],
                      target=mydf[,2],
                      color='#000000',
                      size=2,
                      names="A")




node.data<-data.frame(color=terrain.colors(max(i))[i],
                      size=degree(g),
                      names=V(g)$name,
                      stringsAsFactors = FALSE)


### Create 3D network
layout<-"fruchtermanreingold" #see networkly::get_network for 2D and 3D options
#net params
type<-"3d"
color<-'color'
size<-'size'
name<-'names'

#create network objects
obj<-get_network(edge.list,type=type,layout=layout)
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),
       get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))


net<-shiny_ly(net) 

#add layout options
layout(net,
       scene = list(showlegend=F,
                    yaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                    xaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                    zaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title="")))

