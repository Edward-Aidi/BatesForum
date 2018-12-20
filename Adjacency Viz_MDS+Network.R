# 1017 Adjacency visuliazation through dissimilarity matrix and MDS

# I would like to see whether MDS will help us to visualize the adjacency between groups
# we need to first create the adjacency matrix, rows of which and columns of which are groups

rm(list = ls())
# setwd("C:\\Users\\aid\\Desktop\\Adjacency")
setwd("S:/Reference/Code (R,python,SQL,DAX,etc)/R/Badge Data/Adjacency")
adj <- read.csv("Adjacencies Matrix - TEMP.csv") # The TEMP file is a made-up data for testing
sub_adj <- subset(adj, select = c("Group.Interviewed", "Request", "Adjacency")) 
# Some data cleaning
sub_adj <- sub_adj[!grepl("\\?", sub_adj$Request),] # get rid of the rows that contain "?"
# I manually selected the value representing the distance as primary=0, secondary=5, cannot be by=20, does not matter=10
sub_adj$Adjacency_num  <- ifelse(sub_adj$Adjacency == "Primary", 0, 
                                 ifelse(sub_adj$Adjacency == "Secondary", 5, 
                                        ifelse(sub_adj$Adjacency == "Cannot be by", 20,10)))

# Here we assume the distance is symmetric, i.e. dist(A, B) = dist(B,A)
# We manage to conform this by changing the column of Request and Gorup.Interviewed and rbind to the bottom of the original data
sub_sub_adj <- sub_adj[,c("Request", "Group.Interviewed", "Adjacency", "Adjacency_num")]
colnames(sub_sub_adj) <- colnames(sub_adj)

sub_adj_c <- rbind(sub_adj, sub_sub_adj)
sub_adj_c <- sub_adj_c[!duplicated(sub_adj_c),]
sub_adj_c <- sub_adj_c[order(sub_adj_c$Group.Interviewed),]

# create the distance matrix
library(tidyr)
adj_matrix <- spread(sub_adj_c[,c(1,2,4)], Request, Adjacency_num) # Change the long table to wide
adj_matrix[is.na(adj_matrix)] <- 10 # set the NA as those groups does not matter, so the value is 10 (could be changed as previous)

# Since our data is a made-up one, so we may encounter some data cleaning for the dist matrix
adj_matrix$V1 <- NULL
adj_matrix$`<NA>` <- NULL
adj_matrix <- adj_matrix[-24,]
# Change the column of the Group.Interviewed as row names
rownames(adj_matrix) <- adj_matrix$Group.Interviewed
adj_matrix$Group.Interviewed <- NULL

# Make the principal diagonal as 0
adj_matrix[row(adj_matrix) == col(adj_matrix)] <- 0

####################
# This part just set random value drawn from normal distribution as the does not matter value rather than all 10
# Aim to split the point on the graph a little bit

# i <- 1
# set.seed(2018)
# while(i <= length(adj_matrix[,1])){
#   adj_matrix[i,][grepl(10, adj_matrix[i,])] <- rnorm(1,10,1)
#   i <- i + 1
# }
###################

# Creating the distance matrix
dist_matrix <- dist(adj_matrix, method = "euclidean")
fit <- cmdscale(dist_matrix, eig = TRUE, k = 2)
# f <- isoMDS(dist_matrix) # Another way of calculating the distance for Kruskal's Non-metric MDS

# plot the distance matrix on a 2D graph
x <- fit$points[, 1]
y <- fit$points[, 2]

# adj_plot_data <- as.data.frame(rownames(adj_matrix))
# colnames(adj_plot_data) <- "Group.Interviewed"
# adj_plot_data <- merge(adj_plot_data, adj, by.x = "Group.Interviewed", by.y = "Group.Interviewed", all.x = TRUE)
# adj_plot_data <- subset(adj_plot_data, select = c("Group.Interviewed", "Department"))
# adj_plot_data <- adj_plot_data[!duplicated(adj_plot_data),]

plot(x, y, xlab = "", ylab = "", asp = 1, axes = TRUE, main = "cmdscale")
# CHange the data label posistion on the graph
pos_vector <- rep(3, length(adj_matrix$Group.Interviewed))
pos_vector[adj_matrix$Group.Interviewed %in% c("Strategy", "Order to Delivery", "Powertrain (Engine Engineering)")] <- 2
pos_vector[adj_matrix$Group.Interviewed %in% c("Certification & Compliance")] <- 1
# Add the data label
text(x,y, rownames(adj_matrix), pos = pos_vector, cex = 0.8)

#############################################################
# plot the dist_matrix that has edges and nodes, but it is a lot bit messy because there are too many nodes and edges
# Reference http://www.mjdenny.com/Preparing_Network_Data_In_R.html
d <- as.matrix(dist_matrix)
net <- as.network(x = dd, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)

network.vertex.names(net) <- c(as.character(rownames(adj_matrix)))

summary.network(net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)

plot.network(net, # our network object
             #vertex.col = node_colors, # color nodes by gender
             #vertex.cex = (age)/5, # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5 # display the names directly over nodes
)

######################################################
# Social Network Viz
######################################################
# Tribute to Katya Ognyanova
# Retrieved from http://kateto.net/network-visualization
install.packages("igraph") 
# install.packages("network") 
# install.packages("sna")
# install.packages("visNetwork")
# install.packages("threejs")
# install.packages("networkD3")
# install.packages("ndtv")

# Import data could be edgelist or matrix for the link file, and I will use edgelist as our data input
# There  are two new csv files that we need to create from the raw data file. And this process could be automated. (WIP)
rm(list = ls())
library("igraph")

# setwd("C:\\Users\\aid\\Desktop\\Adjacency")
# nodes <- read.csv("Adjacencies Matrix-TEMP_nodes.csv", header=T, as.is=T)
# links <- read.csv("Adjacencies Matrix-TEMP_edges.csv", header=T, as.is=T)

setwd('S:\\Reference\\Code (R,python,SQL,DAX,etc)\\R\\Adjacency')

raw_data <- read.csv("Adjacencies  Headcount Matrix - FOR SYNTHESIS (002).csv", as.is=T)
links <- subset(raw_data, select = c("Group.Interviewed", "request.used.for.visual", "Adjacency", "Notes"))
nodes <- subset(raw_data, select = c("Group.Interviewed","Interview.Date", "Interview.Lead", "Group.Current.Location", "Number.of.employees.in.Group.Interviewed"))
# Since there are some groups that are being requested but not being interviewed, in the nodes, we need to have all their information
library(plyr)
nodes <- rbind.fill(nodes, data.frame(Group.Interviewed = unique(links$request.used.for.visual)))
nodes <- nodes[!duplicated(nodes$Group.Interviewed),]
nodes <- nodes[nodes$Group.Interviewed != "" & nodes$Group.Interviewed != "??",]
# nodes$Number.of.employees.in.Group.Interviewed[which(is.na(nodes$Number.of.employees.in.Group.Interviewed))] <- 1
nodes$Number.of.employees.in.Group.Interviewed[which(nodes$Number.of.employees.in.Group.Interviewed == "n/a")] <- NA

# Since we are dealing with test data, so need some data cleaning to get rid of the rows that does not have request and ??
rm_nodes <- links[links$request.used.for.visual == "" | links$request.used.for.visual == "??",]$Group.Interviewed
links = links[links$request.used.for.visual != "" & links$request.used.for.visual != "??",]
# Under the assumption of plotting the most valuable connections,
# We are gettin grid of the 'Does not matter' adjacency preference and they will not be shown on the graph (Tentative)
rm_nodes <- c(rm_nodes, links[links$Adjacency == "Does Not Matter", ]$Group.Interviewed)
rm_nodes <- rm_nodes[!duplicated(rm_nodes)]
links = links[links$Adjacency != "Does Not Matter", ]

# Remove the nodes in the rm_nodes
nodes <- nodes[!nodes$Group.Interviewed %in% rm_nodes,]

# Create the function of getting weight for adjacency preferences, which will be passed to edge.width 
# (control the thickness of the edges)
get_weight <- function(adjacency){
  weight = 1
  if(adjacency == "Primary"){
    weight = 10
  }
  else if(adjacency == "Secondary"){
    weight = 2
  }
  return(weight)
}

links["weight"] = apply(as.matrix(links$Adjacency), 1, get_weight)

# Create the distance (later called 'weight' in layout) to deal with the 'cannot be by' problem, we want their distance as far as possible on the plot
links["distance"] = ifelse(links$Adjacency == "Primary", 10, 15)
links["distance"][links["weight"] == 1] <- max(links["weight"])*100

# Create the net
net <- graph_from_data_frame(d = links, vertices = nodes, directed = T) 

# Apply different color for nodes by department
# department_id <- ifelse(V(net)$Department_name == "Engine Engineering", 1, ifelse(V(net)$Department_name == "Manufacturing", 2, ifelse(V(net)$Department_name == "", 3, 4)))
# colrs <- c("light blue", "tomato", "gray50", "gold")
# V(net)$color <- colrs[department_id]

# Or we can set the color of vertices as the same of the client's color
V(net)$color <- "#99CAFF"

# Apply the size of the bubble to stands for the number of people interviewed
# Replace the NA to 10, which is tentative to change
V(net)$Number.of.employees.in.Group.Interviewed[is.na(V(net)$Number.of.employees.in.Group.Interviewed)]<- 15
V(net)$size <- as.numeric(ifelse(as.numeric(V(net)$Number.of.employees.in.Group.Interviewed) <= 50, 
                                 V(net)$Number.of.employees.in.Group.Interviewed,
                                 50))

## Function to wrap long strings
# Source: http://stackoverflow.com/a/7367534/496488
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}
V(net)$label = wrap_strings(V(net)$name, 12)

# Shrink label font
V(net)$label.cex = 0.8

# Apply different adjacency preference on edge.width
# e.g. Primary has a thicker edge
E(net)$width <- E(net)$weight
# Make the edge color the same as the interviewed department
# edge.start <- ends(net, es=E(net), names=F)[,1]
# edge.col <- V(net)$color[edge.start]
colrs <- c("tomato", "yellow")
adj_id <- ifelse(links$Adjacency == "Primary", 1, 2)
edge.col <- colrs[adj_id]

# save the layout in l and set random seed will help us to get the same layout each time
set.seed(2018)
# We can have different layouts, we are using Kamada-Kawai layout here
# If two groups are cannot be by, we will set the weight in the Kamada-Kawai layout a very high number
l <- layout_with_kk(net, weights = links$distance)
#l <- layout_with_kk(net)
#l <- layout.fruchterman.reingold(net, niter=10000, area=30*vcount(net)^2)
# removing the loops in the graph
net <- simplify(net, remove.multiple = F, remove.loops = T)

# If we want tot change the font of the labels
# library(extrafont)
# font_import()
# loadfonts(device = "win")

# Plot the static and store in the .png
# png("plot1.png", height=24, width=24, units="in", res=720) 

plot(canvas.width = 800, canvas.height = 800, 
     net, 
     layout = l, 
     edge.arrow.size = 1, 
     edge.color=edge.col, 
     vertex.label.family = "Arial", vertex.label.font = 2, vertex.label.cex = 1,
     margin = -0.3)

# dev.off()
# # Create the legend of the graph
# legend(x=-1.5, y=-1.1,
#        c("Engine Engineering", "Manufacturing", "", "Strategy"),
#        pch=21, col="#777777", pt.bg=colrs,
#        pt.cex=2, cex=.8, bty="n", ncol=1)

# Interactive plotting
set.seed(2018)
tk_plot <- tkplot(canvas.width = 800, canvas.height = 800, 
                  net, 
                  layout = l, 
                  edge.arrow.size = 1, 
                  edge.color = edge.col, 
                  vertex.label.family = "Arial", vertex.label.font = 2, vertex.label.cex = 1)

# It is possible to increase the space between vertices of the graph, but for the moment it is no need to implement it yet
# Reference https://stackoverflow.com/questions/38999656/increasing-spaces-between-vertices-for-r-igraph
# Function to increase node separation (for explanatory details, see the link below)
# Source: http://stackoverflow.com/a/28722680/496488
# layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout_with_kk) {  
#   g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
#   E(g)$weight <- 1
#   
#   attr <- cbind(id=1:vcount(g), val=wc)
#   g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
#   
#   l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
#   return(l)
# }
