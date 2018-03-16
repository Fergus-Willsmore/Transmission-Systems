# Test topology algorithm using Fake Data with name of 'A-B' format
# Created by F.R. Willsmore 6/2/2018

# Import packages

library(tidyverse)
library(stringr)
library(igraph)
library(statnet)

components <- igraph::components

# Simple small check
name <- c('Armidale-Avon', 'Armidale-Bannaby','Avon-Burrinjuck','Armidale-Burrinjuck')
name.ls <- strsplit(name,'-')
el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)
net <- graph_from_edgelist(el,directed = F)
adj <- get.adjacency(net)

# Slightly larger check

# Read csv data with headers and one intro line

outage.df <- read.csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', 
                      header = TRUE, sep = ",", skip = 1)

bus <- outage.df$Station.Name
bus <- as.character(bus[-which(duplicated(bus))])
bus <- unlist(list(lapply(bus,function(x) gsub("[0-9]{3}kV","",x))))

lines <- scan('~/Documents/GitHub/Transmission-Systems/topology/top_test.txt', character(), quote = "")
name.ls <- strsplit(lines,'-')
el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)
net <- graph_from_edgelist(el,directed = F)
adj <- get.adjacency(net)

plot(net,layout=layout_with_fr, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red",vertex.label=NA)

####### Connectivity ##########

set.seed(344)
count = 0

repeat{
  
  # Decompose network into a list of network components
  
  components <- decompose(net, min.vertices=2)
  
  # Random order of components
  
  rcomp <- sample(1:length(components),length(components))
  
  # Size of each component
  
  len <- sapply(rcomp, function(x) length(vertex_attr(components[[x]])$name))
  
  # choose a random bus in each component
  
  rbus <- sapply(len, function(x) sample(1:x,1))
  
  # Combine components and bus into a list
  
  rnum <- cbind(rcomp, rbus)
  rls<-split(rnum, row(rnum))
  
  # obtain the random bus name for each component
  
  busnames <- sapply(rls, function(x) vertex_attr(components[[x[1]]])$name[x[2]]) %>% matrix(ncol=2)
  
  # create an edgelist of paths between components
  
  paths <- graph_from_edgelist(busnames,directed = F)
  
  # Update the network to be the union of the network and the paths
  
  net <- net %u% paths
  
  # Stop if network is connected
  
  if (igraph::components(net)$no==1|count==30){
    break
  }
  count = count+1
}

plot(net,layout=layout_with_fr, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red",vertex.label=NA)







