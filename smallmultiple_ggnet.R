
## Growing Model Graph - small multiple ggnet implementation

library(dplyr)
library(network)
library(ggplot2)
library(ggnet)
library(RColorBrewer)

# Set up our Network
set.seed(777)
g <- igraph::sample_pa_age(100, pa.exp=1, aging.exp=-0.4, aging.bin=20) # get igraph object from aging barbarsi game
net <- network(intergraph::asNetwork(g),directed=F) #convert igraph object to network object

# Set up the initial layout
x = gplot.layout.fruchtermanreingold(net, NULL) 
net %v% "x" = x[, 1]
net %v% "y" = x[, 2]

# Get a data.frame of edges and add an arbitrary time unit
dat <- as.data.frame(igraph::get.edgelist(g), stringsAsFactors = F) #get dataframe of edges
colnames(dat)<-c("from", "to") #add column names
dat$time <- round(seq.int(1,8,length.out=nrow(dat)),0) #add a time variable

# Convert df to a matrix of when node present or absent
tmp = data.frame(nodeid = c(dat$from,dat$to), time=dat$time) %>% group_by(nodeid) %>% 
      filter(time==min(time)) %>% unique %>% arrange(nodeid)

out <- sapply(tmp$time, function(i) c(rep(0, i-1), rep(1,8-i+1)))
out[out==0]<-NA



# Define vertex attribute activation as 1 or NA:
net %v% "t1" = out[1,]
net %v% "t2" = out[2,]
net %v% "t3" = out[3,]
net %v% "t4" = out[4,]
net %v% "t5" = out[5,]
net %v% "t6" = out[6,]
net %v% "t7" = out[7,]
net %v% "t8" = out[8,]

#for color
mycols <- rev(brewer.pal(9, "Greens")[-1]) #remove really overly light color


# Create ggnet2 plots removing inactive nodes and setting initial layout
t1 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t1")
t2 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t2")
t3 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t3")
t4 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t4")
t5 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t5")
t6 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t6")
t7 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t7")
t8 = ggnet2(net, mode = c("x", "y"), size = 0,  node.color = mycols[tmp$time], na.rm = "t8")


# Set up some plot features
b1 = theme(panel.background = element_rect(color = "grey50"),
           plot.title = element_text(size=rel(2.1)))
b2 = geom_point(aes(color = color), size = 4, color = "white")
b3 =  geom_point(aes(color = color), size = 3, alpha = 0.4)
b4 =  geom_point(aes(color = color), size = 3) 
b5 =  guides(color = FALSE)
y1 = scale_y_continuous(limits = range(x[, 2] * 1.1), breaks = NULL)
x1 = scale_x_continuous(limits = range(x[, 1] * 1.1), breaks = NULL)

# show each temporal network
gridExtra::grid.arrange(t1 + x1 + y1  + ggtitle("t = 1") + b1 + b2 + b3 + b4 + b5,
                        t2 + x1 + y1  + ggtitle("t = 2") + b1 + b2 + b3 + b4 + b5,
                        t3 + x1 + y1  + ggtitle("t = 3") + b1 + b2 + b3 + b4 + b5,
                        t4 + x1 + y1  + ggtitle("t = 4") + b1 + b2 + b3 + b4 + b5,
                        t5 + x1 + y1  + ggtitle("t = 5") + b1 + b2 + b3 + b4 + b5,
                        t6 + x1 + y1  + ggtitle("t = 6") + b1 + b2 + b3 + b4 + b5,
                        t7 + x1 + y1  + ggtitle("t = 7") + b1 + b2 + b3 + b4 + b5,
                        t8 + x1 + y1  + ggtitle("t = 8") + b1 + b2 + b3 + b4 + b5,
                        nrow = 2)



