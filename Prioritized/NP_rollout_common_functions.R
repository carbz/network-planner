#libraries that all may or may not be used in script
library(plyr)
library(ggplot2)
library(sp)
library(rgeos)
library(geosphere)
library(maptools)
library(stringr)
library(PBSmapping)
require(gdata)


## DIST_FUN IS AN ESSENTIAL NETWORK EVALUATION FUNCTION -- we take in 2 vectors (1 & 2) of vertices on a network
## the distance from vertices 1 to the respective vertices 2 is calculated and returned as a single vector 
dist_fun <- function(points_1, points_2, projection_type = proj4)
{
  if(grepl("utm", projection_type[1,1]))
  {
    points_1 <- as.matrix(points_1)
    if (dim(points_1)[1] != dim(points_2)[1])
    {
      points_1 <- matrix(rep(points_1,dim(points_2)[1]),ncol=2,byrow=T)      
    }
    points_2 <- as.matrix(points_2)
    dist <- sqrt(rowSums((points_1 - points_2)^2))  
  }else{
    dist <- distCosine(points_1,points_2,r=6371010) 
    #r=6371010  --> radius of Earth Network Planner uses  
  } 
  return(dist)
}


## BRANCH_IDENTIFY IS THE CORE FUNCTION OF THE SORT PROCESS -- it takes in 2 similar dataframes that  together represent all vertices on a network of interest
## candidate_df represents all possible starting points or 'root' designation for the network
## non_candidate_df represents all vertices on the network that are downstream of starting points 
## output is a list of 2 dataframes (1) for ranked candidates and (2) unsorted candidates
branch_identify <- function(candidate_df, non_candidate_df, proj_var = proj_var)
  ###(1) An Example of a 'candidate_df' dataframe  
  ##long  lat	Name	Settlement.id	Metric...System	Demographics...Projected.household.count	Demand..household....Target.household.count	Demand...Projected.nodal.demand.per.year	System..grid....Transformer.cost	Demographics...Population.count	Demographics...Projected.population.count	order	piece	group	id	fake.node	branch	root
  ##635083.0	485995.0	Rock Town (Settlement)	2096	grid	166	117	1.619486e+05	5775	1002	1586	1	1	3041.1	3041	FALSE	184	184
  
  ##(2) An Example of a 'non_candidate_df' dataframe 
  ##  long	lat	Name	Settlement.id	Metric...System	Demographics...Projected.household.count	Demand..household....Target.household.count	Demand...Projected.nodal.demand.per.year	System..grid....Transformer.cost	Demographics...Population.count	Demographics...Projected.population.count	order	piece	group	id	fake.node	branch	root
  ## 227343.0	764241.6	Kru Town (Settlement)	7540	grid	47	33	43840.537	1575	273	446	2	1	325.1	325	FALSE	NA	NA
  {
  
  ## 8.1 select the observation from candidate.nodes that has the minimum grid length/HH connected <- best.candidate
  sequence <- 1
  ranked.settlements <- data.frame(NULL)
  
  #     non_candidate_df <- non.candidate.nodes
  #     candidate_df <- new.candidate.nodes
  
  # Build network until candidate_df is exhausted of options
  while (nrow(candidate_df) > 0) 
  {  
    candidate <- subset(candidate_df, MV.line.per.kwh == min(MV.line.per.kwh))[1,]
    candidate <- mutate(candidate, 
                        sequence = sequence)
    sequence <- sequence + 1
    ## 8.2 remove candidate observation from candidate.nodes
    candidate_df <- subset(candidate_df, !(Settlement.id %in% candidate$Settlement.id))
    ## place candidate in next order of ranked.settlements and assign incremental value for "sequence"
    ranked.settlements <- rbind(ranked.settlements, candidate)
    
    ## 8.3 re-build candidate.nodes by pulling in any settlements that share "group" with candidate observation now in ranked.settlements while assigning it the same "branch.id" as candidate    
    #identify all new lines associated with candidate settlement 
    new.segments <- non_candidate_df$id[which((non_candidate_df$Settlement.id == candidate$Settlement.id))] 
    if (length(new.segments)>0) 
    {
      #creates 12 variable dataframe such that lines connect to candidate node and settlements are not the candidate node themself
      new.candidate.nodes2 <- as.data.frame(subset(non_candidate_df, (id %in% new.segments) &
                                                     (Settlement.id != candidate$Settlement.id)))
      #ensure unique branch labeling scheme is in place showing ancestory 
      v1 <- 1:length(new.segments)
      new.candidate.nodes2$branch <- str_c(candidate$branch,"-",v1)
      new.candidate.nodes2$root <- (candidate$root)
      
      #remove non_candidate_df that are now candidates 
      non_candidate_df <- subset(non_candidate_df, !(id %in% new.segments))  
      
      #calculate the distance to the new.candidate.nodes
      #^. use the new distance function 
      new.dist <-  dist_fun(candidate[,c("long", "lat")],
                            new.candidate.nodes2[,c("long", "lat")], proj_var) #new distance function by Zaiming!
      
      new.dist <- data.frame(dist = new.dist, Settlement.id = new.candidate.nodes2[,"Settlement.id"])
      
      #makes compatible 15 variable dataframe for candidate.nodes
      new.candidate.nodes2 <- merge(new.candidate.nodes2, new.dist)    
      ##so here is is an issue where 2 disimilar types are being combined to make a list and not a dataframe
      new.candidate.nodes2 <- mutate(new.candidate.nodes2, 
                                     MV.line.per.kwh = dist/Demand...Projected.nodal.demand.per.year
      )                              
      candidate_df <- rbind(candidate_df, new.candidate.nodes2)
      ## 12.4 Return to step 12.1 until merged dataframe is completely sorted into ranked.settlement 
    }
    
  }  
  output <- vector("list",2)
  output[[1]] <- ranked.settlements
  output[[2]] <- non_candidate_df
  return(output)
}

##  SUB.NETWORK.CANDIDATE IS AN ESSENTIAL SORTING FUNCTION OF NP OUTPUTS
## there are known vertices related to sub-networks outputted from NP
## these have no root or designated origin point, so one is designated
## and then a sort is applied
## *requires FUNCTION BRANCH_IDENTIFY
sub.network.candidate <- function(subnetwork_candidate_df, proj_var = proj4)
{
  #identify the sub-networks
  non_candidate_df <- subnetwork_candidate_df
  
  ###here is something on top of the whole loop
  sub.network.groups <- vector("list")
  counter <- 1
  
  ##outer loop starts here to categorize unqiue contigious 'sub.network.groups' 
  while(length(unique(non_candidate_df$Settlement.id)) != 0)
  {
    candidate.groups <- unique(non_candidate_df$Settlement.id)
    #initialized with one node
    node.settlement.ids <- candidate.groups[1]
    exit <- F
    
    # Inner loop starts from here
    while(exit == F)
    {
      #pick all the segment id associated with that node
      connected.segments <- unique(non_candidate_df[which(non_candidate_df$Settlement.id %in% node.settlement.ids),"id"])
      if (length(connected.segments) == 0)
      {
        exit <- T    
      }
      #search all node Settlement.id associated with the segment
      connected.nodes <- unique(non_candidate_df[which(non_candidate_df$id %in% connected.segments),"Settlement.id"])
      #output the Settlement IDs 
      node.settlement.ids <- unique(c(node.settlement.ids, connected.nodes))
      #delete all the finded nodes from subnetwork
      non_candidate_df <- subset(non_candidate_df, !(id %in% connected.segments))
    }
    sub.network.groups[[counter]] <- subset(subnetwork_candidate_df, Settlement.id %in% node.settlement.ids)
    counter <- counter + 1
  }
  
  #define the function returns the max kWh demand served per node 
  max.demand <- function(df, demo_col_name="Demand...Projected.nodal.demand.per.year")
  {
    return(df[which(df[,demo_col_name] == max(df[,demo_col_name])),][1,])
  }
  candidates.nodes <- ldply(sub.network.groups, max.demand)
  candidates.nodes <- arrange(candidates.nodes, desc(Demand...Projected.nodal.demand.per.year))
  branch <- paste("S",(1:length(sub.network.groups)),sep="")
  candidates.nodes$branch <- branch
  #Let's define the Roots also once over
  candidates.nodes$root <- branch
  
  candidates.nodes <- subset(candidates.nodes, select=c("Settlement.id", "id", "branch","root"))
  
  get.branch <- function(df)
  {
    df$branch <- NULL
    df$root <- NULL
    df <- merge(df, candidates.nodes, by = c("Settlement.id", "id"), all.x = T)
    new.candidate.nodes <- subset(df, !is.na(branch))
    new.candidate.nodes <- mutate(new.candidate.nodes, 
                                  dist = 0,
                                  MV.line.per.kwh = 0)
    #         non.candidate.nodes <- subset(df, is.na(branch))
    non.candidate.nodes <- df
    output <- branch_identify(new.candidate.nodes, non.candidate.nodes, proj_var)
    return(output[[1]])
  }
  
  ranked.sub.networks <- ldply(sub.network.groups, get.branch)
  return(ranked.sub.networks)
}


##PREPROCESS.NETWORKPLANNER.REULTS IS A FUNCTION THAT SIMPLIFIES AND RELATED NetworkPlanner outputs 
## for analyses in subsequent functions by tagging all networked vertices and identifying root or starting vertices
##INPUT: NetworkPlanner outputs of metrics.local csv, proposed grid shape file and optional projection defined 
##OUTPUT: A two dataframe list representing the comprehensive list of network vertices by
##(1) starting network vertices AKA 'candidat_df' and (2) all downstream vertices AKA 'non_candidate_df'  
preprocess.networkplanner.results <- function(local_df, shape.file, proj_var = proj4) 
{ 
  #1.0 Formatting & truncating NetworkPlanner outputs
  #rename X & Y to long and lat so correspond exactly with proposed grid shape file
  names(local_df)[names(local_df)=="X"] <- "long"
  names(local_df)[names(local_df)=="Y"] <- "lat"
  #subset local file for segments designated for grid only
  local.grid <- subset(local_df, Metric...System=="grid")
  #truncate local file for most relevant values
  local.grid <- local.grid[,c("Name",
                              "Settlement.id",
                              "long",
                              "lat",
                              "Metric...System",
                              "Demographics...Projected.household.count",
                              "Demand..household....Target.household.count",
                              "Demand...Projected.nodal.demand.per.year",
                              "System..grid....Transformer.cost",
                              "Demographics...Population.count",
                              "Demographics...Projected.population.count")
                           ]
  
  ##Fortify (or flatten) proposed shapefile in order to evaluate each vertex as it relates to local grid points
  proposed.fortified <- fortify(shape.file)
  
  ##2.0 Merge local.grid along with proposed.fortified vertexe nodes 
  ##ensuring ALL non-matchimg proposed.fortified nodes are kept in the merge -> merged
  merged <- merge(local.grid, proposed.fortified, all.y = T)
  
  #3.0 Identify fake nodes as any vertex w/out corresponding local vertex
  merged$fake.node <- F
  merged[which(is.na(merged$Settlement.id)), "fake.node"] <- T
  
  ## 4.1 Each "fake.node" will define the start of a unique branch origin and should be given a 'root' ID from 1 to n
  branch <- (1:sum(as.numeric(merged$fake.node == T)))
  merged$root <- NA  #keep an account of which vertices are roots, AKA branche starts 
  merged$root[which(merged$fake.node == T)] <- branch
  
  #4.2 similiarly, a 'branch' value will be tracked for all vertices based on its place in minimum spanning tree
  merged$branch <- NA 
  merged$branch[which(merged$fake.node == T)] <- branch #start points' roots and branches are one in the same
  
  ## 5.0 Find all nodes that match with segments, aka in same "group",  of "fake.nodes"
  ## and label them as "start.nodes" preserving the "branch.id" and "distance" values
  ghost.nodes <- subset(merged, (fake.node == TRUE))
  
  ## 6.0 Identify all vertices that are starting nodes out of merged  to new dataframe -> candidate.nodes
  candidate.nodes <- subset(merged, 
                            group %in% ghost.nodes$group 
                            & fake.node==FALSE)
  
  ##7.0 Identify, tag and subset out candidate nodes 
  #7.1 use arrange function to order candidate & ghosts nodes by respective unique ID values
  candidate.nodes <- arrange(candidate.nodes , id)
  ghost.nodes <- arrange(ghost.nodes, id)
  
  #7.2 preserve unique branch & root codes originating from existing network 
  candidate.nodes$branch <- 1:length(candidate.nodes$root)
  candidate.nodes$root <- 1:length(candidate.nodes$root)
  
  #7.3 remove candidate nodes from non.candidate nodes dataframe
  non.candidate.nodes <- subset(merged, !(id %in% ghost.nodes$id))
  #7.4 remove unaccounted for ghost nodes
  ghost.nodes <- subset(ghost.nodes, (id %in% candidate.nodes$id))
  ghost.nodes.latlongs <- ghost.nodes[,1:2]
  row.names(ghost.nodes.latlongs) <- seq_len(nrow(ghost.nodes.latlongs))
  
  ##8.0 Determine distance from ghost nodes (on existing network) to corresponding candidate vertices
  dist <-  dist_fun(candidate.nodes[,1:2],
                    ghost.nodes.latlongs,
                    proj_var)
  
  #*it is important to use dummy variable 'Settlement.id' to relate calculated distances to correct vertices
  dist <- data.frame(dist, Settlement.id = candidate.nodes[,"Settlement.id"])
  
  #reassign distance to candidate.nodes dataframe attributing all length to destination or candidate nodes
  candidate.nodes <- merge(candidate.nodes, dist)
   
  #Return ID'd dataframes of candidates and non.candidates where candidates have distances defiend
  output <- list(candidate.nodes, non.candidate.nodes)
  return(output)
}