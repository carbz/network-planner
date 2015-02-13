#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')
source('~/github/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')
source('~/github/network-planner/Prioritized/NP_rollout_common_functions.R')

source('~/github/network-planner/NGA-analysis/postprocessing/NetworkPlanner_SystemRollout_Greedy.R')

#Jonathan's Directory 
# path_name <-"~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/NPresults/Clustering-500m/"
path_name <-"~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/NPresults/"




directory_names <- c("AllPoints/")

setwd(path_name)
#shorten merged file
short_names <- c('Name',"X","Y", "Metric...System",
                 "Demographics...Projected.household.count",
                 "Demand..household....Target.household.count",
                 "Demand...Projected.nodal.demand.per.year",
                 "System..grid....Transformer.cost",
                 "Demographics...Population.count",
                 "Demographics...Projected.population.count",
                 'State')


#******** PART II ********** #
# Rollout 
#If all that stuff works, let's suggest a sequence in which to roll out the construction of grid-nodes.  This has been pre-developed and we're reapplying here 
#Importing proposed grid by itself, no existing lines as well

#proposed <- proposed_merged
proposed <- readShapeLines(paste0(path_name,directory_names[1],'/networks-proposed.shp'))
proposed$FID <- row.names(proposed) # ensure FID is unqiue

proj4 <- read.csv(paste0(path_name,directory_names[1],"/metrics-local.csv"), nrows=1,header=FALSE) #RUNTIME ~ 00:28 mins



#Establish unique IDs for metrics local file
local <- read.csv(paste0(path_name,directory_names[1],"/metrics-local.csv"), skip=1) #RUNTIME ~ 00:28 mins
local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point
# local$Name <- local$Cluster_id

local$long <- local$X
local$lat <- local$Y

#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~08:00***********
greedy_grid <- prioritized.grid.greedy(local,proposed)
# write.csv(greedy_grid, paste0(path_name,'merged_tests/ayeyarwady/metrics-local-nearsightedrank.csv'), row.names=F)
##***************************

#Explicitly define greedy grid output as a dataframe
#Sometimes I need to explicitly call the fataframe for greedy.grid - arghhhh
if (length(greedy_grid)==2){
  print("Houston, we have a problem with our dataframe")
  greedy_grid  <- as.data.frame(greedy_grid[1])
}

#Function to determine downstream summations for greedy grid
greedy_grid_cumulatives <- downstream.sum.calculator(greedy_grid)

#Far Sighted function to improve near-sighted greedy grid
#* **********************
farsighted_grid <- far_sighted_rollout(greedy_grid_cumulatives)
#******************************

##Phasing, Rollout and Costs
#Order the suggested grid path by optimal sequence
farsighted_grid$seq_fs <- farsighted_grid$far.sighted.sequence#shapefile chops longer names
farsighted_grid <- farsighted_grid[order(farsighted_grid$far.sighted.sequence),]

#Develop cummulative sum of network length metric
farsighted_grid <- mutate(farsighted_grid, 
                          CumulativeNetworkExtent.m = cumsum(dist),
                          CumulativeHousesConnected.qty = cumsum(Demand..household....Target.household.count))

#Scalar Values of region before expansion efforts began
percent_houses_connected_at_start <- 0
houses_connected_at_start <- 0
total_houses <- sum(local$Demand..household....Target.household.count, na.rm=T)
new_grid_connections <- max(farsighted_grid$CumulativeHousesConnected.qty)

#Establish some Castalia-specific Metrics 
farsighted_grid <- mutate(farsighted_grid, 
                          MVLinePerConnection = dist/Demand..household....Target.household.count,
                          TransformerCostPerConnection = System..grid....Transformer.cost/Demand..household....Target.household.count,
                          PercentOfNewGridConnections = CumulativeHousesConnected.qty/new_grid_connections)

#That lets us develop Phase bins
farsighted_grid$Phase_HH <- NA
total_phases <- 5
phase_increment_house <- sum(farsighted_grid$Demand..household....Target.household.count)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_house
  upper_cutoff <- j/total_phases*phase_increment_house
  
  farsighted_grid$Phase_HH[which((farsighted_grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                   (farsighted_grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
  
}

farsighted_grid$Phase_MV <- NA
total_phases <- 5
phase_increment_grid <- sum(farsighted_grid$dist)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_grid
  upper_cutoff <- j/total_phases*phase_increment_grid
  
  farsighted_grid$Phase_MV[which((farsighted_grid$CumulativeNetworkExtent.m >= lower_cutoff) &
                                   (farsighted_grid$CumulativeNetworkExtent.m <= upper_cutoff))] <- j
}

##Output The Good stuff
metrics_local_with_sequence <- (farsighted_grid[which(!(duplicated(farsighted_grid$id))),])
proposed_with_rollout <- merge(proposed, metrics_local_with_sequence, by.x = "FID", by.y = "id")
writeLinesShape(proposed_with_rollout, 
                paste0(path_name,directory_names[1],
                       'networks-proposed-with-rollout-20150120.shp'))

#output the flat csv files
shared_col_names <- intersect(names(standalones),names(farsighted_grid)) 

standalones <- subset(local_all, Metric...System != 'grid')
standalones<-local_all
names(standalones)[names(standalones) == 'X'] <- 'long'
names(standalones)[names(standalones) == 'Y'] <- 'lat'

local_all_nodes <- merge(farsighted_grid, standalones, by = shared_col_names, all=T)
local_all_nodes <- rbind.fill(farsighted_grid, standalones)
write.csv(proposed_with_rollout, paste0(path_name,directory_names[1],
                                  'metrics-local-all-nodes-rollout_sequence-20150120.csv'))

#Inside 'Ayeyawady' State?
coordinates(local_all_nodes) = ~long+lat

MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")

InMMR <- over(local_all_nodes,MMR_polygon)[2]  

local_all_nodes <- cbind(local_all_nodes, InMMR)
# local_Ayeyarwady <- subset(local_all_nodes, ST %in% c("Ayeyarwady Region")) #Subset by spatial query instead
# write.csv(local_Ayeyarwady, paste0(path_name,
#                                   'merged_tests/ayeyarwady/metrics-local-all-nodes-rollout_sequence-clipped-20140502.csv'))


write.csv(subset(local_all_nodes, State=='Ayeyarwady'), 
          paste0(path_name,'merged_tests/ayeyarwady/metrics-local-all-nodes-rollout_sequence-clipped-20140502.csv'))


#Polygon data too, why not!
MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")
#     #now let's make it more ggplottable and keep any attribute data 
MMR_polygon@data$id <- rownames(MMR_polygon@data)
MMR_polygon <- merge(MMR_polygon@data, fortify(MMR_polygon), by = 'id')
MMR_polygon<- malukuutara_polygon
MMR_polygon$State <- MMR_polygon$ST

