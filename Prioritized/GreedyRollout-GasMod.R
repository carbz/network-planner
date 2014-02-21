#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the proposed.grid network.  

#Start fresh
source('~/github/local/network-planner/Prioritized/NP_rollout_common_functions.R')

prioritized.grid.greedy.gasmod <- function(local_df, shape.file, proj_var = proj4) 
{

  ##Prepare NetworkPlanner outputs by identifying all network start nodes vs. downstream nodes
  preprocessed.dfs <- preprocess.MozambiqueStart.scenarios(local_df, shape.file, proj_var = proj4)#RUNTIME~00:23
  candidate.nodes <- preprocessed.dfs[[1]]
  non.candidate.nodes <- preprocessed.dfs[[2]]
    
  #***DEFINE SELECTION CRITERIA****
  ##use the unitized MV line required per annual kWh delivered to connect each vertex node as basis for subsequent selections
  candidate.nodes <- mutate(candidate.nodes,
                            MV.line.per.kwh = dist/Demand...Projected.nodal.demand.per.year)
  
  ##BRANCH_IDENTIFY IS THE CORE FUNCTION OF THE SORT PROCESS -- it takes in dataframes that together represent all vertices on a network of interest
  sorted.branches <- branch_identify(candidate.nodes, non.candidate.nodes, proj4)#RUNTIME ~ 07:28 
  ranked.vertices <- sorted.branches[[1]]
  unsorted.vertices <- sorted.branches[[2]]
  
  combined.networks <- sorted.branches
  ##Tag any unsorted networked vertices as sub-networks and sort per sub.network conditions
  if (dim(unsorted.vertices)[1]>0) #Check to see if any unsorted vertices remain
  {
  ranked.subnetworks <- sub.network.candidate(unsorted.vertices) #RUNTIME~00:04
  ##Combine sorted network vertices along with sorted subnetworks
  combined.networks <- rbind.fill(ranked.vertices, ranked.subnetworks)
  }
  
  return(combined.networks)
}