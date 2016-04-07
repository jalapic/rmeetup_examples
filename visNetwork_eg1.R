
### vizNetwork

##vn1
library(igraph)
require(visNetwork, quietly = TRUE)
# minimal example
nodes <- data.frame(id = 1:5)
edges <- data.frame(from = c(2,2,4,1,1), to = c(1,3,2,3,5))
visNetwork(nodes, edges, width = "100%")



#vn 2

### Sade.
nodes <- data.frame(id = c("oldF", "1956F","1957M","1959M","1960M", "1960F", "1961M", "1961F"),
                    group = c("F", "F", "M", "M","M","F", "M", "F"))

edges <- data.frame(from = c(rep("oldF",5),
                             rep("1960F",4),
                             rep("1961F",1),
                             rep("1957M",4),
                             rep("1959M",3),
                             rep("1956F",6),
                             rep("1960M",3),
                             rep("1961M",1)
                             ), 
                    to = c("1960F","1957M","1961F","1956F","1959M",
                           "old","1957M","1961F","1956F",
                           "old",
                           "old","1959M","1961M","1956F",
                           "old","1956F","1957M",
                           "old","1959M","1961M","1960M","1957M","1960F",
                           "1956F","1960F","1961M",
                           "1956F"),
                    width = c(2,1,2,1,1,2,1,1,2,2,1,1,1,1,
                              1,1,1,1,1,2,2,1,2,2,1,1,2
                    )
                    
                          )

nodes$label = nodes$id
nodes$font.size = 24

# edges data.frame for legend
ledges <- data.frame(color = c("darkblue", "red"),
                     label = c("grooms  ", "   groomed"), arrows =c("to", "from"))



head(nodes)
head(edges)
ledges


visNetwork(nodes, edges,width="100%") %>% visEdges(arrows = 'from')  %>%
  visGroups(groupname = "M", color = "darkblue", shape = "square", shadow = list(enabled = T)) %>% 
  visGroups(groupname = "F", color = "red", shape = "triangle") %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -60)) %>%
  visLegend(width=0.2,addEdges = ledges)


##vn3
### do with fontawesome - not stable

visNetwork(nodes, edges,width="100%") %>% visEdges(arrows = 'from')  %>%
  visGroups(groupname = "M", shape = "icon",
            icon = list(code = "f222", color = "darkblue", size = 55)) %>%
  visGroups(groupname = "F", shape = "icon",
            icon = list(code = "f221", color = "red",size = 55)) %>%
  addFontAwesome() %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -60)) %>%
  visLegend(addNodes = list(
    list(label = "Males", shape = "icon", 
         icon = list(code = "f222", size = 30),color="darkblue"),
    list(label = "Females", shape = "icon", 
         icon = list(code = "f221", size = 30, color = "red"))), 
    useGroups = FALSE,
    width=0.2,
    addEdges = ledges)



# vn4

library(igraph)
set.seed(576)
g <- sample_forestfire(125, fw.prob=0.05, bw.factor = 0.2, ambs = 2,directed = F)
nodes <- data.frame(id = as.character(V(g)))
nodes$font.size<-20
edges <- data.frame(get.edgelist(g))
colnames(edges)<-c("from","to")

# with defaut layout
visNetwork(nodes, edges, height = "600px") %>%
  visIgraphLayout() %>%
  visNodes(size = 25) %>%
  visOptions(highlightNearest = T) %>%
  visInteraction(keyboard = TRUE)

        



# vn5

#####  Select by Group
nodes$group <- cluster_fast_greedy(g)$membership
nodes$value = betweenness(g,directed=F, normalized = T)
nodes$font.size <-28

visNetwork(nodes, edges, height = "600px") %>%
  visIgraphLayout() %>%
  visNodes(size = 40) %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE,
                 dragNodes = T, 
                 dragView = T, 
                 zoomView = T)


#vn6
visNetwork(nodes, edges, height = "600px") %>%
  visPhysics(stabilization=F) %>%
  visNodes(size = 40) %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE,
                 dragNodes = T, 
                 dragView = T, 
                 zoomView = T)







#### Tooltip
nodes$title <- paste0("<p>Group: ", cluster_fast_greedy(g)$membership, "<br>",
                      "Degree: ", degree(g),"</p>")
  
visNetwork(nodes, edges, height = "600px") %>%
  visIgraphLayout() %>%
  visNodes(size = 40) %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE,
                 dragNodes = T, 
                 dragView = T, 
                 zoomView = T)




visNetwork(nodes, edges, height = "600px") %>%
  visIgraphLayout() %>%
  visNodes(size = 40) %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE,
                 dragNodes = T, 
                 dragView = T, 
                 zoomView = T)


#vn6
visNetwork(nodes, edges, height = "600px") %>%
  visPhysics(stabilization=F) %>%
  visNodes(size = 40) %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE,
                 dragNodes = T, 
                 dragView = T, 
                 zoomView = T)






#vn8
## Very Large Network

set.seed(576)
g <- g <- sample_pa(10000, directed = F)
nodes <- data.frame(id = as.character(V(g)))
nodes$group <- cluster_fast_greedy(g)$membership
nodes$title <- paste0("<p>Group: ", cluster_fast_greedy(g)$membership, "<br>",
                      "Degree: ", degree(g),"</p>")
nodes$font.size <- 0

edges <- data.frame(get.edgelist(g))
colnames(edges)<-c("from","to")


visNetwork(nodes, edges, height = "600px") %>%
  visIgraphLayout() %>%
  visNodes(size = 10) %>%
  visOptions(selectedBy = "group") %>%
  visInteraction(keyboard = TRUE,
                 dragNodes = T, 
                 dragView = T, 
                 zoomView = T)

