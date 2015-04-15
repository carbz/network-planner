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
#path_name <-"~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/NPresults/"
path_name <-"~/Dropbox/Nigeria-NEAP-GIS/preprocessing/modeling/"





directory_names <- c("757-760-KEDCO-ALL_HHDemand600-JOSM_lines/")

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
#proposed <- readShapeLines(paste0(path_name,directory_names[1],'/networks-proposed.shp'))
proposed1 <- readShapeLines(paste0('~/Dropbox/Nigeria-NEAP-GIS/preprocessing/modeling/757-KEDCO-Katsina_HHDemand600-JOSM_lines/',
                                   '/networks-proposed.shp'))
proposed2 <- readShapeLines(paste0('~/Dropbox/Nigeria-NEAP-GIS/preprocessing/modeling/758-KEDCO-Kano-South_HHDemand600-JOSM_lines/',
                                   '/networks-proposed.shp'))
proposed3 <- readShapeLines(paste0('~/Dropbox/Nigeria-NEAP-GIS/preprocessing/modeling/759-KEDCO-Kano-North_HHDemand600-JOSM_lines/',
                                   '/networks-proposed.shp'))
proposed4 <- readShapeLines(paste0('~/Dropbox/Nigeria-NEAP-GIS/preprocessing/modeling/760-KEDCO-Jigawa_HHDemand600-JOSM_lines/',
                                   '/networks-proposed.shp'))


proposed <- combine.line.shapefiles(proposed1, proposed2)
proposed <- combine.line.shapefiles(proposed, proposed3)
proposed <- combine.line.shapefiles(proposed, proposed4)

proposed$FID <- row.names(proposed) # ensure FID is unqiue

proj4 <- read.csv(paste0(path_name,directory_names[1],"/metrics-local.csv"), nrows=1,header=FALSE) #RUNTIME ~ 00:28 mins

#Establish unique IDs for metrics local file
local <- read.csv(paste0(path_name,directory_names[1],"/metrics-local.csv"), skip=1) #RUNTIME ~ 00:28 mins
local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point
local$Name <- local$Puid

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
                       'networks-proposed-with-rollout-20150415.shp'))

write.csv(farsighted_grid, paste0(path_name,directory_names[1],
                                        'metrics-local-grid-rollout_sequence-20150415.csv'))


#output the flat csv files



standalones <- subset(local, Metric...System != 'grid')
# standalones<-local
names(standalones)[names(standalones) == 'X'] <- 'long'
names(standalones)[names(standalones) == 'Y'] <- 'lat'

shared_col_names <- intersect(names(standalones),names(farsighted_grid)) 

local_all_nodes <- merge(farsighted_grid, standalones, by = shared_col_names, all=T)
local_all_nodes <- rbind.fill(farsighted_grid, standalones)
write.csv(proposed_with_rollout, paste0(path_name,directory_names[1],
                                  'metrics-local-all-nodes-rollout_sequence-20150415.csv'))

#Inside 'Ayeyawady' State?
library(maptools)


# Merge two polyline shape files containing different collections 
combine.line.shapefiles <- function(Phase1Proposed, Phase2Proposed, proj_var = proj4) 
{ 
  # The SpatialLineDataFrame contains two main components: 
  #
  #    Spatial Line: A list in which each element is a set of parameters defining one
  #             spatial object (in this case, a single polygon)
  #    Data:    A two-dimensional attribute table with one row for each spatial feature
  #
  # Combine the two data components from each file in two steps.
  #                       
  # First, 'stack' the attribute list rows using rbind() 
  #
  # Note: This method only works if the two Shape Files have the same spatial data type 
  # and the IDENTICAL (in type, format, number) attribute table (Data component). If this is
  # not the case, you will need to create a NEW 'merged' attribute table from the Data components
  # of each input file.
  #
  mergeData <- rbind.fill(Phase1Proposed@data, Phase2Proposed@data)
  # 
  # Next, combine the two polygon lists into a single list using c()
  #
  mergedLines <- c(Phase1Proposed@lines, Phase2Proposed@lines)
  #
  # Next, generate a new polygon ID for the new SpatialPolygonDataFrame object,
  # 
  offset = length(mergedLines)
  for (i in 1: offset)
  {
    sNew =  as.character(i)
    mergedLines[[i]]@ID = sNew
  }
  #
  # Create an identical ID field and append it to the merged Data component 
  #
  ID = c(as.character(1:length(mergedLines)))
  mergeDataWithID = cbind(ID,mergeData)
  colnames(mergeDataWithID)[2] <- "original_FID"
  
  #  Promote the merged list to a SpatialPolygons data object
  #  
  mergeLinesSP = SpatialLines(mergedLines,proj4string=CRS(proj4string(Phase1Proposed)))
  #
  #  Combine the merged Data and Polygon components into a new SpatialPolygonsDataFrame.
  #
  mySPDF = SpatialLinesDataFrame(mergeLinesSP,data = mergeDataWithID,match.ID = FALSE)
  #
  # Finally, write the new Polygon Shape File
  #
  #writeLinesShape(mySPDF,"MergedNetworks")
  #
  output = mySPDF
  return(output)
}


