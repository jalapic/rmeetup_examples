library(igraph)
library(threejs)

atts1 <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/Attributes/PPRaces.csv", stringsAsFactors=FALSE)
atts <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/Attributes/PPCondRaceGender.csv", stringsAsFactors=FALSE)
mat <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/FriendMatrixT1.csv", stringsAsFactors=FALSE)
rownames(mat)<-paste0("X", mat[,1])
mat<-mat[,-1]
mat <- as.matrix(mat)

#get gaph object
g <- graph.adjacency(mat)

#get giant component
cl<-clusters(g)
g <- induced.subgraph(g, which(cl$membership == which.max(cl$csize)))

#set node and edge attributes
bt <- betweenness(g, normalized=T,directed=F)
V(g)$size <- 2*(bt/max(bt))
postbac<-as.numeric(addNA(atts$Postbac[match(V(g)$name, paste0("X",atts[,1]))]))
V(g)$postbac <- postbac
V(g)$color <- c("#66ccff", "#ffc299", "#d6d6c2")[postbac]
E(g)$color <- "#e6e6e6"
genders<-atts$Gender[match(V(g)$name, paste0("X",atts[,1]))]
V(g)$gender <- ifelse(genders==1, "Female", ifelse(genders==0, "Male", "NA"))
V(g)$race <-  atts1$RaceCode[match(V(g)$name, paste0("X",atts1[,1]))]


graphjs(g, repulsion = 0.3, curvature = 0.4, bg="black", fg="white")
