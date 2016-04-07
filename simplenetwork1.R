library(networkD3)

# Create data
src <- c("A", "I", "A", "B",
         "B", "E", "C", "C", "D", "C", "B")
target <- c("B", "C", "F", "J",
            "H", "F", "G", "H", "I", "I", "I")

networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData, nodeColour = "red", nodeClickColour="blue",zoom=T,height=300,width=300, fontSize = 16)
