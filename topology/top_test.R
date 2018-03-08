# Test topology algorithm using Fake Data with name of 'A-B' format
# Created by F.R. Willsmore 6/2/2018

# Import packages

library(tidyverse)
library(stringr)
library(igraph)

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
     vertex.label.dist=0.5, vertex.color="red", vertex.label=NA)
