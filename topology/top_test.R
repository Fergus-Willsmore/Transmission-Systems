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

#### Connect the network: By Region and Capacity ####

set_vertex_attr(net, 'Region', index = V(net), value = 272)
set_edge_attr(net, 'kv', index = E(net), value = rep(NA, nrow(line.df)))

E(net)$kv = line.df[,4]

ls <- sapply(V(net)$name, function(x) as.character(line.df[line.df$Station == x,1])[1]) %>% as.matrix()

V(net)$Region = ls

net.comps <- decompose(net, min.vertices=2)

for (comp in net.comps){
  tmp = V(comp)$Region
  reg = names(which.max(table(tmp)))
  if (is.null(reg)){
    reg = 'NSW1'
  }
  tmp[is.na(tmp)] = reg 
  V(comp)$Region = tmp
  print(V(comp)$Region)
}




# for (s in rownames(ls)){
#   if (is.null(V(net)$Region[V(net)$name == s])){
#     b <- el[which(el == s) %% nrow(el),2]
#     print(c(s,b))
#   }
#   # V(net)$Region[V(net)$name == b] = V(net)$Region[V(net)$name == s]
# }
# 
# el[which(el == 'Avon'),2] 
# V(net)$Region[V(net)$name == 'Marulan']

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







