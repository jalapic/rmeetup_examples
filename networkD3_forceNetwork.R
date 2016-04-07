library(networkD3)
library(randomNames) 
library(igraph)

# Basic Graph
df <- random_name_df(seed=221)
g <- graph.data.frame(df, directed=F) # raw graph


## Make a vertices df
vertices<-data.frame(
name = V(g)$name,
group = edge.betweenness.community(g)$membership,
betweenness = (betweenness(g,directed=F,normalized=T)*115)+0.1
)


# create indices (indexing needs to be JS format)
df$source.index = match(df$source, vertices$name)-1
df$target.index = match(df$target, vertices$name)-1
head(df)


## fn1

# supply a edgelist + nodelist
d3 = forceNetwork(Links = df, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group = 'group', # color nodes by group calculated earlier
                  charge = -50, # node repulsion
                  linkDistance = 20,
                  zoom = T, 
                  opacity = 1,
                  fontSize=24)

show(d3)



##fn2

### Now add node size by betweenness
d3 = forceNetwork(Links = df, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Nodesize = 'betweenness', #sizing nodes by centrality
                  Group = 'group', # color nodes by group calculated earlier
                  charge = -50, # node repulsion
                  linkDistance = 20,
                  zoom = T, 
                  opacity = 1,
                  fontSize=24)

show(d3)


# fn3

## The "JS" function allows you to directly write javascript

### Adding a colorScale

library(RColorBrewer)

scalecolors <- function(nodes, palette) {
  n <- max(unique(vertices$group))
  cols <- rev(RColorBrewer::brewer.pal(n, palette))
  cols <- paste0("'", paste(cols, collapse = "', '"), "'")
  networkD3::JS(paste0('d3.scale.ordinal().domain([0,', n, ']).range([', cols, '])'))
}

scalecolors(vertices, 'YlOrRd')

#"d3.scale.ordinal().domain([0,9]).range(['#FFFFCC', '#FFEDA0', '#FED976', '#FEB24C', '#FD8D3C', '#FC4E2A', '#E31A1C', '#BD0026', '#800026'])"


#Yellow-Orange-Red
d3 = forceNetwork(Links = df, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group = 'group', 
                  Nodesize = 'betweenness',
                  colourScale = scalecolors(vertices, 'YlOrRd'),
                  charge = -70, # node repulsion
                  linkDistance = 25,
                  zoom = T, 
                  opacity = 1,
                  fontSize=24)

show(d3)






### to get customized text strings for colors

cat(paste(shQuote(gplots::col2hex(heat.colors(9)), type="cmd"), collapse=", "))
cat(paste(shQuote(gplots::col2hex(terrain.colors(9)), type="cmd"), collapse=", "))
cat(paste(shQuote(gplots::col2hex(terrain.colors(10)[-10]), type="cmd"), collapse=", ")) #to avoid last color being too pale - "#F2F2F2"



### Couple of extra things - thanks to Kent Russell for help

# fn4

### Change Background Color and Edge Color

library(htmltools)
browsable(
  tagList(
    forceNetwork(Links = df, Nodes = vertices,
                 Source = 'source.index', Target = 'target.index',
                 NodeID = 'name',
                 Group = 'group', 
                 Nodesize = 'betweenness',
                 colourScale = scalecolors(vertices, 'YlOrRd'),
                 linkColour = "#fff",
                 charge = -80, # node repulsion
                 linkDistance = 25,
                 zoom = T, 
                 opacity = 1,
                 fontSize=24),
    tags$script(
      '
      document.body.style.backgroundColor = "#000000"
      '      
    )
  )
)

## This hack also works but it's a bit naughty
d3 = forceNetwork(Links = df, Nodes = vertices,
             Source = 'source.index', Target = 'target.index',
             NodeID = 'name',
             Group = 'group', 
             Nodesize = 'betweenness',
             colourScale = scalecolors(vertices, 'YlOrRd'),
             linkColour = "#fff",
             charge = -80, # node repulsion
             linkDistance = JS('function(){d3.select("body").style("background-color", "#000"); return 25;}'),
             zoom = T, 
             opacity = 1,
             fontSize=24)

show(d3)



##fn5 

## Add some clicking interactivity  "clickAction"

# standard effect adapted from vignette:
# forceNetwork 
data(MisLinks)
data(MisNodes)

# Create graph
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T)

# with a simple click action - make the circles bigger when clicked
MyClickScript <- 
  ' d3.select(this).select("circle")
.transition().duration(750).attr("r", 40)
'


forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = T, fontSize=24,
             clickAction = MyClickScript)

###




## fn6

### Attempt at making a script to add text when node clicked

myClickScript <-   "
// debugger is always our friend :)
debugger;

// use d3 enter, update, exit
//  make sure to get svg g so our tooltip will move with zoom&drag
var tooltip = d3.select(el).select('svg g').selectAll('text#textid')
.data([d.name + ' | group: ' + d.group]);

// add if not there yet
tooltip.enter().append('text')

// update our attributes
tooltip
.text(function(d){return d;})
.attr('id', 'textid')
.attr({'x':d.x,'y':d.y})
.style('font-size','22px');
"


d3 = forceNetwork(Links = df, Nodes = vertices,
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group = 'group', 
                  Nodesize = 'betweenness', 
                  charge = -50, 
                  linkDistance = 20,
                  colourScale = JS('d3.scale.ordinal()
                                   .range(["#e70351", "#e8fd02", "#eb03fe", "#fb9104", "#fd99ee", 
                                            "#e8d97d", "#ea958a", "#fd01af", "#fc3002"])
                                   .domain(d3.range(0,9))'),
                  zoom = T, 
                  opacity = 0.9,
                  fontSize = 0,
                  clickAction = myClickScript)

show(d3)





####
# Friendship Network...  with legend  - dark background color nodes fluorescent


