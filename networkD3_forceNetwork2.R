## Need To Clean Up
#extra force directed network examples


#fn7 - intro-bio friend network betweenness
#fn8 - cultural psych network - college, t1/t2  ? divs side-by-side ?





library(igraph)
library(networkD3)
library(dplyr)

atts <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/Attributes/PPCondRaceGender.csv", stringsAsFactors=FALSE)
mat <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/FriendMatrixT1.csv", stringsAsFactors=FALSE)
attsrace <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/Attributes/PPRaces.csv", stringsAsFactors=FALSE)

rownames(mat)<-paste0("X", mat[,1])
mat<-mat[,-1]

mat <- as.matrix(mat)

g <- graph.adjacency(mat)


df <- as.data.frame(get.edgelist(g))
colnames(df) <- c("source", "target")

vertices <- as.data.frame(rownames(mat))
colnames(vertices) <- c("name")
vertices$postbac <- atts$Postbac[match(vertices$name, paste0("X",atts[,1]))]
vertices$group = edge.betweenness.community(g)$membership 
vertices$race <- attsrace$RaceCode[match(vertices$name, paste0("X",attsrace[,1]))]

vertices$size <- log10(betweenness(g,directed=F,normalized=T) + 1) *1000
vertices$betweenness <- betweenness(g,directed=F,normalized=T)


# indices
df$source.index = match(df$source, vertices$name)-1
df$target.index = match(df$target, vertices$name)-1

# 
# d3 = forceNetwork(Links = df, Nodes = vertices,
#                   Source = 'source.index', Target = 'target.index',
#                   NodeID = 'name',
#                   Group = 'postbac',
#                   Nodesize = 'size',
#                   charge = -50, 
#                   linkDistance = 20,
#                   zoom = T, 
#                   opacity = 0.9
#                   )
# 
# show(d3)
# 
# 


### Add custom color for betweenness

d3 = forceNetwork(Links = df, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group = 'betweenness',
                  colourScale = JS('d3.scale.linear().domain([0, 0.05, 0.26]).range(["lightgreen", "darkgreen", "black"])'),
                  charge = -50, 
                  linkDistance = 20,
                  zoom = T, 
                  opacity = 0.9
)

show(d3)


                  


### Do two time points - side by side in divs.


#############   Cultural Psych

#t1 <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/CulFriendT1.csv", stringsAsFactors=FALSE)
attst1 <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/Attributes/Attributes-SNACUL.csv", stringsAsFactors=FALSE)

t1 <- read.csv("C:/Users/curley1/Dropbox/Work/Mentoring/KayteeTuretsky/For James/CulFriendT2.csv", stringsAsFactors=FALSE)



rownames(t1)<-paste0("X", t1[,1])
t1<-t1[,-1]

t1 <- as.matrix(t1)
g <- graph.adjacency(t1)

df <- as.data.frame(get.edgelist(g))
colnames(df) <- c("source", "target")

vertices <- as.data.frame(rownames(t1))
colnames(vertices) <- c("name")
head(attst1)

vertices$school <- attst1$SchoolSimplified[match(vertices$name, paste0("X",attst1$PPID))]
vertices$race <- attst1$RaceSimplified[match(vertices$name, paste0("X",attst1$PPID))]

vertices$size <- log10(betweenness(g,directed=F,normalized=T) + 1) *100
vertices$betweenness <- betweenness(g,directed=F,normalized=T)
max(vertices$betweenness)



# indices
df$source.index = match(df$source, vertices$name)-1
df$target.index = match(df$target, vertices$name)-1

attst1$SchoolSimplified1<-ifelse(attst1$School=="CC", "CC",
                          ifelse(attst1$School=="BC", "BC",
                          ifelse(grepl("GS",attst1$School), "GS", "Other")))

vertices$school1 <- attst1$SchoolSimplified1[match(vertices$name, paste0("X",attst1$PPID))]

library(htmltools)
browsable(
  tagList(
   forceNetwork(Links = df, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group= "school1",
                  colourScale = JS('d3.scale.ordinal()
                       .domain(["GS", "BC", "Other","CC"])
                                 .range(["MediumSeaGreen", "Tomato", "gray", "RoyalBlue"]);'
                ),
                  linkColour = "Black",
                  linkWidth = 1,
                  charge = -80, 
                  linkDistance = 50,
                  zoom = T, 
                  bounded=T,
                  legend = T,
                  opacity = 1,
                  fontSize=24
),
tags$script(
  '
  document.body.style.backgroundColor = "#d9d9d9"
  '      
)
)
)

