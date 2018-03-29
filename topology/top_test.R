# Create a Connected Network
# Test topology algorithm using line data in 'bus-bus' format
# The line data is from planned outage data in PUBLIC_NOSDAILY_2018030515000037 copy.csv, 
# processed via python script topology.py to obtain 'bus-bus' format.
# Created by F.R. Willsmore 6/2/2018

# Import packages

library(tidyverse)
library(stringr)
library(igraph)
library(ggplot2)
library(ggraph)
library(statnet)

components <- igraph::components

# Read in outage data if needed

outage.df <- read.csv('~/Documents/GitHub/Transmission-Systems/topology/PUBLIC_NOSDAILY_2018030515000037 copy.csv', 
                      header = TRUE, sep = ",", skip = 1)

# Read .txt file containing the line names processed by topology.py

line.df <- read.delim('~/Documents/GitHub/Transmission-Systems/topology/top_test.txt', header = TRUE, sep = '\t', character(), quote = "")

# Create edge-list for network

name.ls <- strsplit(as.character(line.df$Equipment),'-')
name.length <- lapply(name.ls, function(x) length(x)) %>% as.matrix()
name.ls <- name.ls[-which(name.length>2)]

el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)

# Create network from edglist

net <- graph_from_edgelist(el,directed = F)

# Plot the initial network

net.graph <- ggraph(net) + 
  geom_edge_link() + 
  geom_node_point(col = "indianred1", size = 1) +
  theme_graph()

plot(net.graph)

ggsave('net.pdf', plot = net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

#### Connect the network: By Region ####

set_vertex_attr(net, 'Region', index = V(net), value = 272)

ls <- sapply(V(net)$name, function(x) as.character(line.df[line.df$Station == x,1])[1]) %>% as.matrix()

V(net)$Region = ls

net.comps <- decompose(net, min.vertices=2)

i<-1
for (comp in net.comps){
  tmp = V(comp)$Region
  region = names(which.max(table(tmp)))
  if (is.null(region)){
    region = 'NSW1'
  }
  tmp[is.na(tmp)] = region 
  V(net.comps[[i]])$Region = tmp
  print(V(net.comps[[i]])$Region)
  i<-i+1
}

# Get the index of components from each region

s <- sapply(net.comps, function(x) names(which.max(table(V(x)$Region))))
regions <- levels(factor(s))
r.ind<-list()
i<-1
for (r in regions){
  r.ind[[i]]<-sapply(s, function(x) which(x==paste(r))) %>% as.logical %>% which()
  i<-i+1
}
names(r.ind)<-regions

r.ind <- sapply(r.ind, function(x) matrix(sample(x),ncol = 2))

# Size of each component

len <- sapply(net.comps, function(x) length(vertex_attr(x)$name))

# choose a random bus in each component

rbus <- sapply(len, function(x) sample(1:x,1))

rnum <- cbind(1:length(net.comps), rbus)
rls<-split(rnum, row(rnum))

# obtain the random bus name for each component

busnames <- sapply(rls, function(x) vertex_attr(net.comps[[x[1]]])$name[x[2]]) %>% matrix(ncol=2)

# create an edgelist of paths between components

paths <- graph_from_edgelist(busnames,directed = F)

net <- net %u% paths
 
#### Connect the network: Random algorithm ####

set.seed(560)

source('~/Documents/GitHub/Transmission-Systems/topology/rand.net.top.R')

con.net <- rand.net.top(net)

con.net.graph <- ggraph(con.net) + 
                    geom_edge_link() + 
                    geom_node_point(col = "indianred1", size = 1) +
                    theme_graph()

plot(con.net.graph)

ggsave('con_net.pdf', plot = con.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )

# plot(con.net,layout=layout_with_fr, vertex.size=3,
#      vertex.label.dist=0.5, vertex.color="indianred1",vertex.label=NA)







