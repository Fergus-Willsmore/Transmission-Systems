# Network Topology by Region (reg.net.top)
# A function that will connect components based on the region of each bus. 
# This function works with the igraph library to manipulate the network structure.
# Input: An igraph network object (presumably not connected) with state edge attributes (i.e. SA, NSW, VIC).
# Output: An igraph network object that is connected.
# Created by F.R. Willsmore 29/3/2018

reg.net.top.E <- function(net, states){
  
  # Check for a connected network
  
  if (components(net)$no==1) stop('The network is already connected.')
  
  count = 0
  
  repeat{
    
    # Decompose network into a list of network components
    
    net.comps <- decompose(net, min.vertices=2)
    
    # Get the index of components from each state
    
    creg <- sapply(net.comps, function(x) names(which.max(table(E(x)$State))))
    
    r.ind<-list()
    
    i<-1
    for (r in regions){
      r.ind[[i]]<-sapply(creg, function(x) which(x==paste(r))) %>% as.logical %>% which()
      i<-i+1
    }
    
    r.ind <- sapply(r.ind, function(x) matrix(sample(x),ncol = 1))
    r.ind <- rbind.fill.matrix(ldply(r.ind))
    
    # Size of each component
    
    len <- sapply(r.ind, function(x) length(vertex_attr(net.comps[[x]])$name))
    
    # choose a random bus in each component
    
    rbus <- sapply(len, function(x) sample(1:x,1))
    
    rnum <- cbind(r.ind, rbus)
    rls<-split(rnum, row(rnum))
    
    # obtain the random bus name for each component
    
    busnames <- sapply(rls, function(x) vertex_attr(net.comps[[x[1]]])$name[x[2]]) %>% matrix(ncol=2, byrow = TRUE)
    
    # create an edgelist of paths between components
    
    paths <- graph_from_edgelist(busnames,directed = F)
    
    # add paths to the network
    
    net <- net %u% paths
    
    # Stop if network is connected or too many iterations
    
    if (components(net)$no==1|count==200){
      break
    }
    
    # update count
    
    count = count+1
  }
  
  return(net)
  
}
