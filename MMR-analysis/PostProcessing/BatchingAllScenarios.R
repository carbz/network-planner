#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')
source('~/github/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')
source('~/github/network-planner/Prioritized/NP_rollout_common_functions.R')

#Jonathan's Directory 
path_name <-"Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/All_Ayewardy/"

# directory_names <- c("Ayeyar_Without_Maubin&Hinthada_8kmBuf/151-Ayer+25km-TEST/",
#                      "BagoWest_Hinthada(Ayeyar)/152-Bago_West_Hinthada_25KMBuf",
#                      "Yongan_Maubin(Ayeyar)_24KM/700-Yangon＋Maubin(Ayeyar)24kmBuffer-GAD+MIMU")

all_scenarios <-c('Rakhine/701-w25kmBuf-GAD+MIMU/',
                  'Mandalay/702/',
                  'Yongon/700-Yangon＋Maubin(Ayeyar)24kmBuffer-GAD+MIMU/',
                  'Kachin/703/',
                  'Kayah/704/',
                  'Magway/150/',
                  'Ayeyar_Without_Maubin&Hinthada_8kmBuf/151-Ayer+25km-TEST/',
                  'BagoWest_Hinthada(Ayeyar)/152-Bago_West_Hinthada_25KMBuf/',
                  'Naypitaw/154-Nyapitaw/',
                  'Sagaing/165/',
                  'Chin//156-Chin/',
                  'All_Bago/164-Bago-All-1000kWh-AmznTest/',
                  'Sagaing/165/',
                  'All_Shan/706/')
directory_names <- all_scenarios

setwd(path_name)
#shorten merged file
short_names <- c('Name',"X","Y", "Metric...System",
                 "Demographics...Projected.household.count",
                 "Demand..household....Target.household.count",
                 "Demand...Projected.nodal.demand.per.year",
                 "System..grid....Transformer.cost",
                 "Demographics...Population.count",
                 "Demographics...Projected.population.count")

i=1
#Import Phase 1 Data
for (i in 1:length(directory_names)){
  print(i) 
  
  proj4 <- read.csv(paste0(path_name,directory_names[i],"/metrics-local.csv"), nrows=1, header = FALSE)[1]
  
  local <- read.csv(paste0(path_name,directory_names[i],"metrics-local.csv"), skip=1) #RUNTIME ~ 00:28 mins
  
  existing <- readShapeLines(paste0(path_name,directory_names[i],"networks-proposed.shp"))
  
  # Determine the nearest point on a line for a set of points
  #
  # input:  
  #  output_dir:  where result files will be placed
  #  point_csv_file:  csv of points (with Longitude, Latitude columns) 
  #  line_shape_file:  shapefile of lines
  # 
  # output (will all be placed in output_dir): 
  #  points_on_line.csv:  for each point in point_csv_file, the closest point to
  #                       a line in line_shape_file
  #  points_distances.csv:  point_csv_file with additional "distance" field
  #                         representing the distance to the nearest line in
  #                         line_shape_file
  #  shortest_lines.shp:  shapefile representing shortest lines from input point
  #                       to closest point on a line in line_shape_file
  # 
  # Run via command line:
  # Rscript --vanilla point_to_segment_dists.R output_dir point_csv_file line_shape_file
  
  library(tools)
  library(rgdal)
  library(geosphere) # must be v1.3-8 or greater
  
  output_dir_name <- paste0(path_name,directory_names[i])
  input_point_csv_name <- local
  input_line_shapefile_name <- 'networks-existing'
  # 
  # input_point_csv_name <- "tmp/demographicsLL.csv"
  # input_line_shapefile_name <- "tmp/LeonaNetworks.shp"
  
  # make points a SpatialPointsDataFrame
  lonlat <- CRS("+proj=longlat +datum=WGS84")
  point_df <- local#read.csv(input_point_csv_name)
  coordinates(point_df) <- ~X+Y
  proj4string(point_df) <- lonlat
  
  # make network a SpatialLinesDataFrame
  shape_dir <- paste0(output_dir_name)
  shape_base <- file_path_sans_ext(basename(input_line_shapefile_name))
  
  # use readOGR b/c it captures the projection
  net_df <- readOGR(dsn=shape_dir, layer=input_line_shapefile_name)
  net_df <- spTransform(net_df, lonlat)
  
  # calculate the dists to lines
  dists <- dist2Line(point_df[1,], net_df)
  dists_df <- as.data.frame(dists)
  # add the distance to the original point dataset
  point_df$distance <- dists_df$distance
  
  # create lines representing shortest distance to network
  from_mat <- as.matrix(coordinates(point_df))
  to_mat <- as.matrix(dists_df[,c("lon", "lat")])
  sp_lines_list <- lapply(1:nrow(from_mat), function(i) {
    Lines(Line(rbind(from_mat[i,], to_mat[i,])),i)
  })
  
  sp_lines <- SpatialLines(sp_lines_list, lonlat)
  lines_data <- data.frame(distance=dists_df$distance, 
                           line_id=dists_df$ID)
  sp_lines_df <- SpatialLinesDataFrame(sp_lines, lines_data)
  
  writeOGR(sp_lines_df, output_dir_name, "shortest_lines", "ESRI Shapefile")
  write.csv(dists_df, file.path(output_dir_name, "points_on_line.csv"))
  write.csv(point_df, file.path(output_dir_name, "points_distances.csv"))
  
  
  
  
  
  }
  

local <- local[,short_names]

proj4 <- read.csv(paste0(path_name,directory_names[i],"/metrics-local.csv"), nrows=1, header = FALSE)

#There are some problem variables inconsistent btw two datasets, let's pretend they exist in both
local1$Villagetr1 <- NULL
local2$Villagetr1 <- NULL
local3$Villagetr1 <- NULL

local1$Vt_code <- NULL
local2$Vt_code <- NULL
local3$Vt_code <- NULL

#Merge 1 & 2
shared_col_names <- intersect(names(local1),names(local2)) #c('Village_co', 'X','Y','Metric...System')
local_all <- merge(local1, local2, by = shared_col_names, all = T)
#Merge 1,2 & 3
shared_col_names <- intersect(names(local_all),names(local3)) #c('Village_co', 'X','Y','Metric...System')
local_all <- merge(local_all, local3, by = shared_col_names, all = T)


#Subset States on interest to match Shaky's joined subset
# States <- c("Chin", "Magway")
# local_all <- subset(local_all, State %in% States) #Unreliable because ST attribute is not consistent

#Coerce points to spatial dataframe
coordinates(local_all) = ~X+Y
#Inside 'Chin' State?
MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")

InMMR <- over(local_all,MMR_polygon)[2]  

local_all <- cbind(local_all, InMMR)
#local_Chin_Magway <- subset(local_all, ST %in% c("Magway Region","Chin State")) #Subset by spatial query instead

local_all_lite <- local_all[,short_names]



#Output Results
write.csv(local_all_lite, paste0(path_name,'merged_tests/ayeyarwady/metrics-local-lite20140428.csv'), row.names=F)
write.csv(local_all, paste0(path_name,'merged_tests/ayeyarwady/metrics-local.csv'), row.names=F)

#Pulling in Shaky's join
proposed_merged <- readShapeLines(paste0(path_name,
                                     "merged_tests/ayeyarwady/AY_Network_Proposed_Merged_from_3_Scenarios/AY_Network_Proposed_Merged_from_3_Scenarios.shp"))

proposed1 <- readShapeLines(paste0(path_name,directory_names[1],'/networks-proposed.shp'))
proposed2 <- readShapeLines(paste0(path_name,directory_names[2],'/networks-proposed.shp'))
proposed3 <- readShapeLines(paste0(path_name,directory_names[3],'/networks-proposed.shp'))

# change their IDs so they don't conflict
proposed1 <- spChFIDs(proposed1, paste0('1.', proposed1$FID))
proposed2 <- spChFIDs(proposed2, paste0('2.', proposed2$FID))
proposed3 <- spChFIDs(proposed3, paste0('3.', proposed3$FID))

# add a 'MVLineType' attribute
lines <- rbind(proposed1, proposed2)
lines <- rbind(lines, proposed3)

writeLinesShape(lines, paste0(path_name,'merged_tests/ayeyarwady/networks-proposed.shp'))


###Castalia's version of metrics-local stuff

castalia_path <-"~/Dropbox/Myanmar NEP sharebox/from Columbia/OutputsByState-1000kWhDemand/"

castalia_scenarios <- c('150-Magway-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '154-Nyapitaw-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '156-Chin-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '157A-Kayin-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '157B-Mon-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '157C-Tanintharyi-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '164-Bago-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '165-Sagaing-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     'HoldOnAnalysis-Rechecking/167-Ayerarwady-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '701-Rakhine-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '702-Mandalay-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '703-Kachin-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '704-Kayah-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv',
                     '706-All-Shan-metrics-local-all-nodes-rollout_sequence-clipped-ForCastalia.csv')
                     
directory_names <- castalia_scenarios

setwd(path_name)
#shorten merged file
short_names <- c('Name',"X","Y", "Metric...System","State","Demographics...Projected.household.count",
                 "Demand..household....Target.household.count","Demand...Projected.nodal.demand.per.year",
                 "System..grid....Transformer.cost","Demographics...Population.count",
                 "Demographics...Projected.population.count","Scenario")

admin_codes <- c('District','District_c','Gr_ml_cm','Ho_size_r','Ho_size_u',
                 'Mg_fl_cl','Pop_2001','Pop_2011','Pop_2013','Pop_g_r','Pop_g_u',
                 'Population','Source','State','State_code','Township','Township_c',
                 'Village','Village_co','Village_fa','Village_hh','Village_po','Villagetra',
                 'Vt_code','Vt_hh','Vt_pop')

EA_codes <- c('order','piece','group','id','root','branch','dist','depth',
              'Total.Downstream.Demand.kWh','Total.Downstream.Network.Extent.m',
              'far.sighted.sequence','CumulHH','PhaseByHHQuintile','PhaseByHHQuintRnd',
              'CumulDist','PhaseByMVQuintile','PhaseByMVQuintRnd','BinsBySett.Size') 

i=1
local_all_MMR <- as.data.frame(NULL)
#Import Phase 1 Data
for (i in 1:length(castalia_scenarios)){
  print(i) 
  
  local <- read.csv(paste0(castalia_path,directory_names[i])) #RUNTIME ~ 00:28 mins
  scenario <- substr(directory_names[i],0,3)
  local$Scenario <- scenario
  local_lite <- local[,short_names]
  
  local_all_MMR<- rbind.fill(local_lite,local_all_MMR)
}
write.csv(local_all_MMR, '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/local-AllStates-1000kWhDemand.csv', row.names=F)

##Develop a composite view of Proposed lines

#original scenarios live here:
path_name <-"~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/"
all_scenarios <-c('Rakhine/701-w25kmBuf-GAD+MIMU/',
                  'Mandalay/702/',
                  'Yongon/700-Yangon＋Maubin(Ayeyar)24kmBuffer-GAD+MIMU/',
                  'Kachin/703/',
                  'Kayah/704/',
                  'Magway/150/',
                  'Ayeyar_Without_Maubin&Hinthada_8kmBuf/151-Ayer+25km-TEST/',
                  'BagoWest_Hinthada(Ayeyar)/152-Bago_West_Hinthada_25KMBuf/',
                  'Naypitaw/154-Nyapitaw/',
                  'Sagaing/165/',
                  'Chin//156-Chin/',
                  'All_Bago/164-Bago-All-1000kWh-AmznTest/',
                  'Sagaing/165/',
                  'All_Shan/706/',
                  '/Mon+Tanintharyi+Kayin/157/')
directory_names <- all_scenarios


proposed_i <- readShapeLines(paste0(path_name,directory_names[1],'/networks-proposed.shp'))
proposed_i <- spChFIDs(proposed_i, paste0('1', proposed_i$FID))
all_lines <- proposed_i
i=2
#Import Phase 1 Data
for (i in 2:length(directory_names)){
  print(i) 
  
  proposed_i <- readShapeLines(paste0(path_name,directory_names[i],'/networks-proposed.shp'))
  
  # change their IDs so they don't conflict
  proposed_i <- spChFIDs(proposed_i, as.character(paste0(i,'.', proposed_i$FID)))
  
  # bind to previous dataset 'MVLineType' attribute
  all_lines <- rbind(proposed_i, all_lines)  
}
writeLinesShape(all_lines, 
                '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networks-proposed_allMMR1000kWh.shp')

##Go back to the basic raw outputs 
path_name <-"~/Desktop/MMR_scenarios/"
orig_scenarios <-c("150","154","156","157","164","165","167","700",
                  "701","702","703","704","706")
directory_names<- orig_scenarios

short_names <- c('Name',"X","Y", "Metric...System",
                 "Demographics...Projected.household.count",
                 "Demand..household....Target.household.count",
                 "Demand...Projected.nodal.demand.per.year",
                 "System..grid....Transformer.cost",
                 "Demographics...Population.count",
                 "Demographics...Projected.population.count")

local_all_orig <- as.data.frame(NULL)
local_all_orig <- read.csv('Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/local_orig-AllStates-1000kWhDemand.csv')
all_lines <- readShapeLines('Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/networks-proposed-orig_allMMR1000kWh.shp')

i=1
for (i in 1:length(directory_names)){
  print(i) 
  
  local <- read.csv(paste0(path_name,directory_names[i],'/metrics-local.csv'), skip=1) #RUNTIME ~ 00:28 mins
  scenario <- substr(directory_names[i],0,3)
  #local$Scenario <- scenario
  local_lite <- local[,short_names]
  
  #Merge 1,2 & 3
  shared_col_names <- intersect(names(local_lite),names(local_all_orig)) #c('Village_co', 'X','Y','Metric...System')
  local_all_orig <- merge(local_all_orig, local_lite, all=T)
  
  #Merge all proposed shapefiles together
  proposed_i <- readShapeLines(paste0(path_name,directory_names[i],'/networks-proposed.shp'))
  
  # change their IDs so they don't conflict
  proposed_i <- spChFIDs(proposed_i, as.character(paste0(scenario,'.', proposed_i$FID)))
  
  # bind to previous dataset 'MVLineType' attribute
  if (i>1){
  all_lines <- rbind(proposed_i, all_lines) 
  }else {
    all_lines <- proposed_i
  }

} 
write.csv(local_all_orig, 
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/local_orig-AllStates-1000kWhDemand.csv', row.names=F)
writeLinesShape(all_lines, 
                '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networks-proposed-orig_allMMR1000kWh.shp')

  



#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~08:00***********
local_all_orig$Settlement.id <- row.names(local_all_orig)
all_MMR_nearsighted <- prioritized.grid.greedy(local_all_orig,all_lines)

write.csv(all_MMR_nearsighted, paste0(path_name,'merged_tests/ayeyarwady/metrics-local-nearsightedrank.csv'), 
          row.names=F)
##***************************

#Explicitly define greedy grid output as a dataframe
#Sometimes I need to explicitly call the fataframe for greedy.grid - arghhhh
if (length(all_MMR_nearsighted)==2){
  print("Houston, we have a problem with our dataframe")
  all_MMR_nearsighted  <- as.data.frame(all_MMR_nearsighted[1])
}

#Function to determine downstream summations for greedy grid
MMR_grid_cumulatives <- downstream.sum.calculator(all_MMR_nearsighted)

write.csv(MMR_grid_cumulatives, paste0(path_name,'merged_tests/ayeyarwady/metrics-local-nearsightedrank+cumulatives.csv'), 
          row.names=F)

#Far Sighted function to improve near-sighted greedy grid
#* **********************
farsighted_grid <- far_sighted_rollout(MMR_grid_cumulatives)
#******************************
 