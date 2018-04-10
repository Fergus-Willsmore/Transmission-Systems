

# Import packages

library(tidyverse)
library(stringr)
library(igraph)
library(ggplot2)
library(ggraph)

components <- igraph::components

# Import google earth file

test <- readLines('~/Documents/GitHub/Transmission-Systems/topology/kml.txt')
tmp <- matrix(test[12:length(test)],ncol = 15, byrow = TRUE)
sp <- data.frame(tmp)
colnames(sp) <- c('OBJECTID','FEATURETYPE','CLASS','FID','NAME','OPERATIONALSTATUS','CAPACITYKV','STATE','SPATIALCONFIDENCE','REVISED','SHAPE_Length','x1','y1','x2','y2')

line <- as.character(sp$NAME)

# Split names by the separator ' to ' to obtain bus names

test.ls <- strsplit(line, ' to ')

# Check entries that are of length greater than 2

fix.ls <- sp$NAME[which(lapply(test.ls, length)!=2)]

which(lapply(test.ls, length)!=2)

line[1677] = "Canning Vale to Canning Vale Tee"
line[1862] = "Duaringa to Baralaba"
sp[nrow(sp)+1,] = sp[1862,]
line[nrow(sp)] = "Baralaba to Blackwater Tee"
line[1876] = "Woden to Civic"
line[1894] = "Arndell Park to Blacktown"
sp[nrow(sp)+1,] = sp[1894,]
line[nrow(sp)] = "Blacktown to Quakers"
line[1897] = "Arndell Park to Blacktown"
sp[nrow(sp)+1,] = sp[1897,]
line[nrow(sp)] = "Blacktown to Rooty Hill"
line[2063] = "Blyth West to Para"
sp[nrow(sp)+1,] = sp[2063,]
line[nrow(sp)] = "Para to Bungama Tee"
line[2223] = "SOTO to TOCE"
line[2224] = "MORA to BROD"
line[2225] = "Bulli Creek to Waggamba"
line[2228] = "Mica Creek B to Duchess Road 1"
line[2229] = "Mica Creek B to Duchess Road 2"
line[2230] = "Gunpowder Mine to Century Mine 1"
line[2231] = "Mica Creek C to Ernesthenry/Chumvale"
line[2232] = "Mica Creek C to Gunpowder"
line[2233] = "BR to ER"
line[2375] = "Copeton to Borthwick"

# Update line information

sp$NAME = line

# Split names by the separator ' to ' to obtain bus names

name.ls <- strsplit(sp$NAME, ' to ')

# Remove those entries that are not complete

index <- which(lapply(name.ls, length)!=2)
name.ls <- name.ls[-index]
sp <- sp[-index,]

el <- matrix(unlist(name.ls), ncol = 2, byrow = TRUE)

# Create network from edglist

net <- graph_from_edgelist(el,directed = FALSE)

E(net)$Capacity = sp$CAPACITYKV
E(net)$Region = sp$STATE

# Map coordinates to the buses

coord <- as.matrix(cbind(el, sp[c('x1','y1','x2','y2')]))
colnames(coord) <- NULL
coord <- rbind(coord[,c(1,3,4)], coord[,c(2,5,6)])
coord <- data.frame(Name = coord[,1], x = as.numeric(coord[,2]), y = as.numeric(coord[,3]))

V(net)$Long <- rep(0,length(V(net)))
V(net)$Lat <- rep(0,length(V(net)))

i <- 1
for (bus in coord$Name){
  # print(coord[i,])
  # print(index)
  index <- which(sapply(V(net)$name, function(x) x==bus))
  V(net)$Long[index] <- coord[i,2]
  V(net)$Lat[index] <- coord[i,3]
  i <- i+1
}

lo <- layout.norm(as.matrix(cbind(V(net)$Long,V(net)$Lat)),-1, 1, -1, 1)

plot.igraph(net,layout = lo,vertex.size = 0.5, vertex.label = NULL)

# Plot the initial network

net.graph <- ggraph(net,layout = lo) + 
  geom_edge_link(aes(color = E(net)$Region)) + 
  theme_graph()

plot(net.graph)

# extract main component

main.net <- decompose(net)[[1]]

net.graph <- ggraph(main.net,layout = lo,) + 
  geom_edge_link(aes(color = factor(E(main.net)$Region))) + 
  theme_graph()

plot(net.graph)

#### Connect the network: Region algorithm ####

source('~/Documents/GitHub/Transmission-Systems/topology/reg.net.top.R')

reg.net <- reg.net.top(net,levels(sp$STATE))

reg.net.graph <- ggraph(reg.net, layout = 'kk') + 
  geom_edge_link(aes(color = sp$STATE)) + 
  theme_graph()

plot(reg.net.graph)

ggsave('spatial_net.png', plot = reg.net.graph, 
       path = '~/Documents/GitHub/Transmission-Systems/figures', 
       width = 8, height = 8 )
