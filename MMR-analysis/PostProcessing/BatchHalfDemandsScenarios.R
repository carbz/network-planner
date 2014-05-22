library(tools)
library(rgdal)
library(geosphere) # must be v1.3-8 or greater
library(stringr)
library(maptools)

#Jonathan's Directory 
path_name <-"~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/500kWh_Scenarios/"

directory_names <-c('158-Mon+Tanintharyi+Kayin/',
                  '160-Chin/',
                  '161-Nyapitaw/',
                  '168-Ayewardy_All/',
                  '169-Sagaing/',
                  '170-Bago_All/',
                  '171-Kachin/',
                  '172-Kayah/',
                  '707-Shan_All/',
                  '708-Mandalay/',
                  '709-Rakhine/',
                  '710-Yangon/',
                  '711-Magway/')

#Pulling most 'common' variable from datasets for easier handling##
short_names <- c('Name',"X","Y", "Metric...System","State","Demographics...Projected.household.count",
                 "Demand..household....Target.household.count","Demand...Projected.nodal.demand.per.year",
                 "System..grid....Transformer.cost","Demographics...Population.count",
                 "Demographics...Projected.population.count","Scenario","Scenario_name")
admin_codes <- c('District','District_c','Gr_ml_cm','Ho_size_r','Ho_size_u',
                 'Mg_fl_cl','Pop_2001','Pop_2011','Pop_2013','Pop_g_r','Pop_g_u',
                 'Population','Source','State','State_code','Township','Township_c',
                 'Village','Village_co','Village_fa','Village_hh','Village_po','Villagetra',
                 'Vt_code','Vt_hh','Vt_pop')
EA_codes <- c('order','piece','group','id','root','branch','dist','depth',
              'Total.Downstream.Demand.kWh','Total.Downstream.Network.Extent.m',
              'far.sighted.sequence','CumulHH','PhaseByHHQuintile','PhaseByHHQuintRnd',
              'CumulDist','PhaseByMVQuintile','PhaseByMVQuintRnd','BinsBySett.Size') 


### 0. MERGE SCENARIOS TOGETHER ####
local_all_orig <- as.data.frame(NULL)
for (i in 1:length(directory_names)){
  print(i) 
  
  ##****************************************************##
  ##*********Step 1: Combine metrics-local.csv**********##
  ##****************************************************##
  local <- read.csv(paste0(path_name,directory_names[i],'/metrics-local.csv'), 
                    skip=1, stringsAsFactors = FALSE) #RUNTIME ~ 00:28 mins
  scenario <- substr(directory_names[i],0,3)
  #local_lite <- local[,c(short_names)]
  #local_lite <- local
  
  #Merge 1,2 & 3
  shared_col_names <- intersect(names(local),names(local_all_orig))
  
  #Preserve original scenario field
  if(i==1) {
    local_all_orig <- local
    local_all_orig$Scenario <- scenario
    } else {
    ##New guys get latest scenario designation
    local_all_orig <- merge(local_all_orig, local, by=shared_col_names, all=T)
    local_all_orig[which(is.na(local_all_orig$Scenario)),'Scenario'] <- scenario
    } 
  ##********************************************************##
  ##*********Step 2: Combine networks-proposed.shp**********##
  ##********************************************************##
#   
#   proposed_i <- readShapeLines(paste0(path_name,directory_names[i],'/networks-proposed.shp'))
#   # change their IDs so they don't conflict
#   proposed_i <- spChFIDs(proposed_i, as.character(paste0(scenario,'.', proposed_i$FID)))
#   proposed_i$FID <- row.names(proposed_i)
#   
#   if(i==1) {
#     #Designate merged file
#     all_lines <- proposed_i
#     }else {
#       # bind to previous dataset 'MVLineType' attribute
#       all_lines <- rbind(proposed_i, all_lines) 
#     }
} 

##************************************************************##
##*********Step 3: Eliminate duplicate settlement ************##
##**************** vertices in metrics-local composite********##
##************************************************************##


local_all_orig$XYID <- paste0(str_sub(as.character(local_all_orig$X*100000),end=7L),str_sub(as.character(local_all_orig$Y*100000),end=7L))

##Remove conflicting electrification types(Metric...System) for same settlements
local_all_orig <- local_all_orig[order(local_all_orig$Metric...System),]#get grids to the top
duplicates <- local_all_orig[which(duplicated(local_all_orig$XYID, 
                                              fromLast=FALSE)),]#minimize grid nodes being removed
uniques <- local_all_orig[which(!(duplicated(local_all_orig$XYID, 
                                             fromLast=FALSE))),]#minimize grid nodes being removed
local_all_orig <- uniques

##********************************************************##
##*********Step 4: OUTPUT the composite Datasets**********##
##********************************************************##
write.csv(local_all_orig, 
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/metrics-local-All-500kWh.csv', row.names=F)
writeLinesShape(all_lines, 
                '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networks-proposed-ALL-500kWh.shp')



#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')
source('~/github/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')
source('~/github/network-planner/Prioritized/NP_rollout_common_functions.R')


#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~08:00***********
local_all_orig$Settlement.id <- row.names(local_all_orig)
proj4 <- read.csv(paste0(path_name,directory_names[i],"/metrics-local.csv"), nrows=1, header = FALSE)[1]

all_MMR_nearsighted <- prioritized.grid.greedy(local_all_orig,all_lines,proj4)

write.csv(all_MMR_nearsighted, 
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/metrics-local-All-500kWh-greedyrank.csv', row.names=F)

##***************************
#Explicitly define greedy grid output as a dataframe
#Sometimes I need to explicitly call the fataframe for greedy.grid - arghhhh
if (length(all_MMR_nearsighted)==2){
  print("Houston, we have a problem with our dataframe")
  all_MMR_nearsighted  <- as.data.frame(all_MMR_nearsighted[1])
}

#Function to determine downstream summations for greedy grid
MMR_grid_cumulatives <- downstream.sum.calculator(all_MMR_nearsighted)

write.csv(MMR_grid_cumulatives, 
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/metrics-local-All-500kWh-greedyrank-cums.csv', row.names=F)


##Output The intermediates
metrics_local_with_sequence <- (MMR_grid_cumulatives[which(!(duplicated(MMR_grid_cumulatives$id))),])
proposed_with_rollout <- merge(all_lines, metrics_local_with_sequence, by.x = "FID", by.y = "id")
writeLinesShape(proposed_with_rollout, 
                paste0(paste0(path_name,
                              'merged_tests/master_merged/networks-proposed-with-Near-rollout-20140506.shp'), 
                       row.names=F))


#Far Sighted function to improve near-sighted greedy grid
#* **********************
farsighted_grid <- far_sighted_rollout(MMR_grid_cumulatives)
#******************************
write.csv(farsighted_grid,
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/metrics-local-All-500kWh-farsighted.csv', 
          row.names=F)
          

#####*************************************#########
######Define some typical Useful Values ###########
#####*************************************#########


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
#####*************************************#########
#####*************************************#########


##Output The Good stuff
metrics_local_with_sequence <- (farsighted_grid[which(!(duplicated(farsighted_grid$id))),])
proposed_with_rollout <- merge(all_lines, 
                               metrics_local_with_sequence, 
                               by.x = "FID", by.y = "id", all=TRUE)
writeLinesShape(proposed_with_rollout, 
                paste0(path_name,"merged_all_states/networks-proposed-with-rollout-20140523.shp"))


##Edwin wants all settlements 
farsighted_grid$XYID <- paste0(str_sub(as.character(farsighted_grid$long*100000),end=7L),str_sub(as.character(farsighted_grid$lat*100000),end=7L))
farsighted_grid_all_settlements <- merge(local_all_orig, farsighted_grid, by='XYID', all=T)
farsighted_grid_all_settlements <- farsighted_grid_all_settlements[order(farsighted_grid_all_settlements$far.sighted.sequence),]
write.csv(farsighted_grid_all_settlements, 
          paste0(path_name,'merged_all_states/metrics-local-All-500kWh-farsighted-allsettlements.csv'), 
          row.names=F)
          

