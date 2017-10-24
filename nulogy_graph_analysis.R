# A. A-S, October 2017 | Simple Network Graph Analysis
# Resource for igraph: http://kateto.net/netscix2016
library(igraph)

# Read data files and store in dataframes
nodes <- read.csv("nodesAndLabel.csv", header=T, as.is=T)
links <- read.csv("shipAndConsignRelation.csv", header=T, as.is=T)

# Vectors defining visual properties of graphs
colors <- c("blue", "red")
shapes <- c("circle","square")

# Encode labels as numbers
labels <- as.numeric(unclass(as.factor(V(net)$label)))

# Create undirected graphical network object
net <- graph_from_data_frame(d=links[c('SHIPPER','CONSIGNEE','total.qty')], vertices=nodes[c('name','label')], directed=F)

# Set colour and shape properties based on the label (consignee/shipper)
V(net)$color <- colors[labels]
V(net)$shape <- shapes[labels]

# Plot the network
plot(net, edge.width=0.5,margin = 0,edge.color="black",vertex.label=V(net)$label, vertex.size=7, asp=0.4,vertex.label.color="black",vertex.label.font=3,vertex.label.cex=0.55)
legend(x=-1, y=-1, c("Consignee","Shipper"), pch=c(21,22),pt.bg=colors, pt.cex=2, cex=.7, bty="n")

# Calculate tables with degrees for consignee and shipper nodes
deg.consignee.nodes <- table(links[c('CONSIGNEE')])
deg.shipper.nodes   <- table(links[c('SHIPPER')])

# Compute minimum and maximum degrees
min.deg <- min(min(deg.consignee.nodes), min(deg.shipper.nodes))
max.deg <- max(max(deg.consignee.nodes), max(deg.shipper.nodes))

# Number of nodes and number of edges
n.nodes <- length(unique(nodes$name))
n.edges <- nrow(unique(links[,c("SHIPPER", "CONSIGNEE")]))

# Density formula for undirected simple graph: https://math.stackexchange.com/questions/1526372/what-is-the-definition-of-the-density-of-a-graph
density <- (2 * n.edges) / (n.nodes * (n.nodes - 1))

min.deg
max.deg
density