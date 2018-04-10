# Random Network Topology (rand.net.top)
# A function that will randomly connect components of a network until a connected network is reached.
# This function works with the igraph library to manipulate the network structure.
# Input: An igraph network object (presumably not connected).
# Output: An igraph network object that is connected.
# Created by F.R. Willsmore 16/3/2018

rand.net.top <- function(net){
  
  # Check for a connected network
  
  if (components(net)$no==1) stop('The network is already connected.')
  
  # Precautionary measure for while loop
  
  count = 0
  
  repeat{
    
    # Decompose network into a list of network components
    
    net.components <- decompose(net, min.vertices=2)
    
    # Random order of components
    
    rcomp <- sample(1:length(net.components),length(net.components))
    
    # Size of each component
    
    len <- sapply(rcomp, function(x) length(vertex_attr(net.components[[x]])$name))
    
    # choose a random bus in each component
    
    rbus <- sapply(len, function(x) sample(1:x,1))
    
    # Combine components and bus into a list
    
    rnum <- cbind(rcomp, rbus)
    rls<-split(rnum, row(rnum))
    
    # obtain the random bus name for each component
    
    busnames <- sapply(rls, function(x) vertex_attr(net.components[[x[1]]])$name[x[2]]) %>% matrix(ncol=2)
    
    # create an edgelist of paths between components
    
    paths <- graph_from_edgelist(busnames,directed = F)
    
    # Update the network to be the union of the network and the paths
    
    net <- net %u% paths
    
    # Stop if network is connected or too many iterations
    
    if (components(net)$no==1|count==2000){
      break
    }
    
    # update count
    
    count = count+1
  }
  
  return(net)
  
}
