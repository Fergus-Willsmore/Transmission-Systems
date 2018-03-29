# Network Topology by Region (reg.net.top)
# A function that will connect components based on the region of each bus. 
# This function works with the igraph library to manipulate the network structure.
# Input: An igraph network object (presumably not connected) with region vertex attributes (i.e. SA, NSW, VIC).
# Output: An igraph network object that is connected.
# Created by F.R. Willsmore 29/3/2018

reg.net.top <- function(net){
  
  # Check for a connected network
  
  if (components(net)$no==1) stop('The network is already connected.')
  
  # Decompose network into a list of network components
  
  net.comps <- decompose(net, min.vertices=2)
  
  # Fill in missing region data with the region of a connected bus
  
  i <- 1
  
  for (comp in net.comps){

    tmp = V(comp)$Region # vector of regions for the buses in the component

    region = names(which.max(table(tmp))) # the most common region
    
    if (is.null(region)){
      region = names(which.max(table(V(net)$Region))) # update region for component if no region present on any bus
    }
    
    tmp[is.na(tmp)] = region # update missing data with most common region in component
    
    V(net.comps[[i]])$Region = tmp
    
    i<-i+1
  }
  
  # Precautionary measure for while loop
  
  count = 0
  
  repeat{
    
    # Decompose network into a list of network components
    
    if (count!=0){
      net.components <- decompose(net, min.vertices=2)
    }
    
    # Get the index of components from each region
    
    creg <- sapply(net.comps, function(x) names(which.max(table(V(x)$Region))))
    
    regions <- levels(factor(s))
    
    r.ind<-list()
    
    i<-1
    for (r in regions){
      r.ind[[i]]<-sapply(creg, function(x) which(x==paste(r))) %>% as.logical %>% which()
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
    
    # add paths to the network
    
    net <- net %u% paths
    
    # Stop if network is connected or too many iterations
    
    if (components(net)$no==1|count==length(components)/2){
      break
    }
    
    # update count
    
    count = count+1
  }
  
  return(net)
  
}
