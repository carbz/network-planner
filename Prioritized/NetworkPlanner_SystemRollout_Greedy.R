#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the proposed.grid network.  

#Start fresh
source('~/github/network-planner/Prioritized/NP_rollout_common_functions.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')

prioritized.grid.greedy <- function(local_df, shape.file, 
                                    proj_var = "PROJ.4 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                                    output = 'lite') 
{

  ##Prepare NetworkPlanner outputs by identifying all network start nodes vs. downstream nodes
  preprocessed.dfs <- preprocess.networkplanner.results(local_df, shape.file, proj_var = proj4)#RUNTIME~00:23
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
  
  if (output == 'full'){
    # Identify which variable names are shared btw input dataframe and ranked dataset  
    shared_column_names <- colnames(local_df)[which(colnames(local_df) %in% colnames(combined.networks))]
    # Merge metrics_local and the ranked files 
    combined.networks <- merge(local_df, combined.networks, by = shared_column_names, all.y=T)
  }
  
  
  return(combined.networks)
}


## FARSIGHTED_SORT IS THE CORE FUNCTION OF THE SORT PROCESS -- it takes in 1 dataframe derivative of metrics-local
#This nodal file represent all vertices on a network of interest
#It is is expected that a branch ID has been established as well as total downstream values for kWh and installed grid
far_sighted_rollout <- function(greedy_grid_cumulatives){
  ###(1) An Example of a 'candidate_df' dataframe  
  ##long  lat  Name  Settlement.id  Metric...System	Demographics...Projected.household.count	Demand..household....Target.household.count	Demand...Projected.nodal.demand.per.year	System..grid....Transformer.cost	Demographics...Population.count	Demographics...Projected.population.count	order	piece	group	id	fake.node	branch	root
  ##635083.0	485995.0	Rock Town (Settlement)	2096	grid	166	117	1.619486e+05	5775	1002	1586	1	1	3041.1	3041	FALSE	184	184
  
  ##select the observation from candidate.nodes that has the minimum grid length/HH connected <- best.candidate
  count <- 1
  ranked_settlements <- data.frame(NULL)
  
  #Adjust selction criteria to be more informed
  #basically, selection criteria is a measure of cost corrected for benefit
  greedy_grid_cumulatives <- mutate(greedy_grid_cumulatives, 
                                     Selection.Criteria = Total.Downstream.Network.Extent.m/Total.Downstream.Demand.kWh)
  
  #separate out the near sighted ranked function with downstream cummulations 
  #want to know which are starting candidates vs. non-candidates
  candidate_df <- subset(greedy_grid_cumulatives, depth < 1)
  non_candidate_df <- subset(greedy_grid_cumulatives, depth >= 1)
  
  # Build network until candidate_df is exhausted of options
  while (nrow(candidate_df) > 0) 
    {  
    candidate <- subset(candidate_df, Selection.Criteria == min(Selection.Criteria))[1,]
    candidate <- mutate(candidate, 
                        far.sighted.sequence = count)
    count <- count + 1
    ##remove candidate observation from candidate.nodes
    candidate_df <- subset(candidate_df, !(Settlement.id %in% candidate$Settlement.id))
    ## place candidate in next order of ranked.settlements and assign incremental value for "sequence"
    ranked_settlements <- rbind(ranked_settlements, candidate)
    
    ## re-build candidate.nodes by pulling in any settlements that branch off  with candidate observation now in ranked.settlements while assigning it the same "branch.id" as candidate    
    #calculate the next depth level allowed from candidate node added
    new_depth <- candidate$depth+1 
    #identify all new lines associated with candidate settlement by sharing branch path ID
    old_root_branch <- paste0(candidate$branch,"-")
    branch_length <- str_length(candidate$branch)+1
        
    new_segments <- non_candidate_df[which((str_sub(non_candidate_df$branch, end=branch_length) == old_root_branch) & #branch origin is the  
                                             (non_candidate_df$depth == new_depth)) #depth allowed is next in order
                                     , ] #all columns 
    
    ##remove new candidate from non_candidate.nodes
    non_candidate_df <- subset(non_candidate_df, !(Settlement.id %in% new_segments$Settlement.id))
    
    #desigate the new segments connected to the candidate now as candidate nodes for consideration
    #updating candidate df
    candidate_df <- rbind(candidate_df, new_segments)
        
  }  
  #Explicitly define sequence columns
  ranked_settlements <- rename(ranked_settlements, replace=c('sequence'='near.sighted.sequence'))
  
  #Output an ordered, farsighted ranked grid now with cummulative downstream demand & distances for all ranked nodes
  ranked_settlements <- arrange(ranked_settlements, far.sighted.sequence)
  
  return(ranked_settlements)
}