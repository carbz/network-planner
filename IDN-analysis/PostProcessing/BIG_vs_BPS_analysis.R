#Evaluate how BPS settlements perform against BIG settlements 
require(stringr)
require(plyr)

desa_characteristics <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Feb2014-Preprocessing/Shapefiles/DesaDemographicsandGeographics.csv")
desa_characteristics <- desa_characteristics[c("VILLAGE_CO","PLN_HHOLD","NO_PLN","AREA_KM2","TOT_POP","TOT_HHOLD","POP_DEN","HHOLD_DEN")]
colnames(desa_characteristics)[1] <- "Desa"

#measure performance of output scenarios 

#path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/"
# directory_names_orig <- c("65-FloresBarat-MV5",
#                           "64-FloresTimor-MV5",
#                           "63-SumbaArea-MV5",
#                           "59-KupanArea-MV5",
#                           "62-TernateArea-MV5",
#                           "61-TualArea-MV5",
#                           "60-AmbonArea-MV5")

path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/"
directory_names <- c("98-Ternate-1km",
                     "97-Ambon-1kmBuffer",
                     "96-Taul-PulauDoboi-1km",
                     "103-TualSaumlaki-1km",
                     "106-TualBarat-1km",
                     "105-Tual-1km",
                     "99-Kupang-1kmBuffer",
                     "100-FloresBarat-1km",
                     "101-FloresTimur-1km",
                     "102-Sumba-1km")

all_metrics_local <- data.frame(NULL)
for (i in 1:length(directory_names)){
  
  directory_name <- paste0(path_name, directory_names[i])
  #directory_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/64-FloresTimor-MV5")
  
  #import the metrics local file with additionals grid attributes like MV length
  local <- read.csv(paste0(directory_name,paste0("/",
                                                 str_sub(directory_names[i], end = str_locate(directory_names[i], "-")[1]),
                                                 "metrics-local-rollout_sequence.csv")), stringsAsFactors=F)
  
  if ("Full_popul" %in% names(local)){
    local$Full_population <- local$Full_popul 
    local$Target_household_penetration_rate <- local$Target_hou}
  
  local <- local[c("Name","dist",
                   "Demand..household....Target.household.count",
                   "Ei_subarea",
                   "Pln_cabang",
                   "Full_population",
                   "Target_household_penetration_rate",
                   "depth",
                   "Demographics...Mean.household.size",
                   "Demographics...Projected.population.count")]
  
  # #Import metrics local for each island areas in the analysis
  local_all <- read.csv(paste0(directory_name,"/metrics-local.csv"), skip=1, stringsAsFactors=F)
  
  if ("Full_popul" %in% names(local_all)){
    local_all$Full_population <- local_all$Full_popul 
    local_all$Target_household_penetration_rate <- local_all$Target_hou}
  
  
  #local_all$Desa <- str_sub(local_all$Name, end=10L)
  #local_all$cluster_count <- str_sub(local_all$Name, start=11L, end=12L)
  
  metrics_local_general_summary <- ddply(local_all, .(Name, Metric...System), summarize,
                                         Demand..household....Target.household.count = sum(Demand..household....Target.household.count),
                                         Demographics...Projected.population.count = sum(Demographics...Projected.population.count),
                                         Full_population = sum(Full_population)
  )
  #Aggregate populations for minigrid 
  minigrid_stats <- subset(metrics_local_general_summary, Metric...System=="mini-grid")
  colnames(minigrid_stats)[3:5] <- str_c(colnames(minigrid_stats[3:5]),"_minigrid")
  minigrid_stats$Metric...System <-NULL
  
  #Aggregate populations for offgrid 
  offgrid_stats <- subset(metrics_local_general_summary, Metric...System=="off-grid")
  colnames(offgrid_stats)[3:5] <- str_c(colnames(offgrid_stats[3:5]),"_offgrid")
  offgrid_stats$Metric...System <-NULL
  
  #Aggregate populations for unelectrified 
  unelectrified_stats <- subset(metrics_local_general_summary, Metric...System=="unelectrified")
  colnames(unelectrified_stats)[3:5] <- str_c(colnames(unelectrified_stats[3:5]),"_unelectrified")
  unelectrified_stats$Metric...System <-NULL
  
  #combine all the results into one long summary
  metrics_local_general_summary_long <- merge(minigrid_stats, offgrid_stats, by = "Name", all =TRUE)
  metrics_local_general_summary_long <- merge(metrics_local_general_summary_long, unelectrified_stats, by = "Name", all =TRUE)
  
  #combine all the metrics local with grid stats as well
  metrics_local_general_summary_long <- merge(metrics_local_general_summary_long, local, by = "Name", all =TRUE)
  
  all_metrics_local <- rbind.fill(all_metrics_local, metrics_local_general_summary_long)
}

all_metrics_local$Desa <- str_sub(all_metrics_local$Name, end=10L)
all_metrics_local$cluster_count <- str_sub(all_metrics_local$Name, start=11L, end=12L)

#NA's mess up the summations i'm about to do 
all_metrics_local[is.na(all_metrics_local)] <- 0

desa_MV_lengths <- ddply(all_metrics_local, .(Desa), summarize,
                         Total.MV.Needed = sum(dist),
                         Settlements = max(cluster_count),
                         Demand..household....Target.household.count.grid = sum(Demand..household....Target.household.count),
                         Demand..household....Target.household.count.minigrid = sum(Demand..household....Target.household.count_minigrid),
                         Demand..household....Target.household.count.offgrid = sum(Demand..household....Target.household.count_offgrid),
                         
                         Avg_HHold_Size = mean(Demographics...Mean.household.size),
                         
                         Demographics...Projected.population.count.GridOnly = sum(Demographics...Projected.population.count),
                         Demographics...Projected.population.count.MiniGridOnly = sum(Demographics...Projected.population.count_minigrid),
                         Demographics...Projected.population.count.OffGridOnly = sum(Demographics...Projected.population.count_offgrid),
                         
                         Full_population = sum(Full_population + Full_population_minigrid + Full_population_offgrid),
                         
                         Target_household_penetration_rate = mean(Target_household_penetration_rate),
                         Ei_subarea = Ei_subarea[1],
                         Pln_cabang =Pln_cabang[1],
                         Maximum_MV_Depth = max(depth)
)

#Now Combine the two
desa_results <- merge(desa_MV_lengths, desa_characteristics, by = "Desa", all =TRUE)
desa_results$XY_Source <- NA

for (i in 1:dim(desa_results)[1]){
  if (desa_results$Settlements[i] == 99){
    desa_results$Settlements[i] = 1
    desa_results$XY_Source[i] = "BPS"
  } else {
    desa_results$XY_Source[i] = "BIG"
  }
}

#WE dont like them NA's, Excel cant sum 'em
desa_results[is.na(desa_results)] <- 0

setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/")
write.csv(desa_results, "DesaLevelCharacteristics_from1kmBuffer-StandalonePopIncluded-20140211.csv", row.names=F)



#Needed to aggregate all outputs of simulations to enable above analysis  
# 
#

# ##Develop input data for grid lenghts
# #path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/"
# path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/"
# 
# directory_names <- c("65-FloresBarat-MV5",
#                      "64-FloresTimor-MV5",
#                      "63-SumbaArea-MV5",
#                      "59-KupanArea-MV5",
#                      "62-TernateArea-MV5",
#                      "61-TualArea-MV5",
#                      "60-AmbonArea-MV5")
# for (i in 1:length(directory_names)){
#   
#   directory_name <- paste0(path_name, directory_names[i])
#   #directory_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/564-MalukuUtara-900HHD/"
#   
#   # #Import metrics local for each island areas in the analysis
#   # #metrics local is the key output file of each Network Planner scenario capturing nodal level information
#   local <- read.csv(paste0(directory_name,"/metrics-local.csv"), skip=1, stringsAsFactors=F)
#   
#   proj4 <- read.csv(paste0(directory_name,"/metrics-local.csv"), nrows=1, header = FALSE)
#   
#   proposed <- readShapeLines(paste0(directory_name,"/networks-proposed.shp")) #RUNTIME ~ 00:08 mins
#   
#   #Process metrics-local to sugggest a rollout sequence
#   
#   local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point
#   
#   ## ensure FID is unqiue
#   proposed$FID <- row.names(proposed)
#   
#   #Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#   #takes a shapefile (network) and csv (nodal descriptions and weights) 
#   #and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#   #***RUNTIME ~08:00***********
#   greedy.grid <- prioritized.grid.greedy(local,proposed,proj4)
#   ##***************************
#   
#   #Explicitly define greedy grid output as a dataframe
#   #Sometimes I need to explicitly call the fataframe for greedy.grid - arghhhh
#   #greedy.grid <- as.data.frame(greedy.grid)
#   
#   if (length(greedy.grid)==2){
#     print("Houston, we have a problem with our dataframe")
#     greedy.grid  <- as.data.frame(greedy.grid[1])
#   }
#   
#   #Function to determine downstream summations for greedy grid
#   greedy.grid.cummulatives <- downstream.sum.calculator(greedy.grid)
#   
#   #Far Sighted function to improve near-sighted greedy grid
#   #farsighted.grid <- far_sighted_rollout(greedy.grid.cummulatives)
#   
#   ##Phasing, Rollout and Costs
#   
#   ##Edwin likes to keep all the metrics local data together, so reconnect it by unique Name field
#   #mu_ranked <- merge(x=malukuutara, y=greedy.grid.cummulatives, all.x=T, by = "Name")
#   
#   #Order the suggested grid path by optimal sequence
#   #Broken!
#   farsighted.grid <- greedy.grid.cummulatives
#   farsighted.grid$far.sighted.sequence <- farsighted.grid$sequence
#   farsighted.grid <- farsighted.grid[order(farsighted.grid$far.sighted.sequence),]
#   
#   #Develop cummulative sum of network length metric
#   farsighted.grid$CummulativeNetworkExtent.m <- cumsum(farsighted.grid$dist)
#   farsighted.grid$CummulativeHousesConnected.qty <- cumsum(farsighted.grid$Demand..household....Target.household.count)
#   
#   #Scalar Values of region before expansion efforts began
#   percent_houses_connected_at_start <- 1-sum(local$Demographics...Projected.household.count)/(sum(local$Full_population/local$Ho_size))
#   houses_connected_at_start <- (sum(local$Full_population/local$Ho_size) - sum(local$Demographics...Projected.household.count))
#   total_houses <- sum(local$Full_population/local$Ho_size)
#   
#   #Track cummulative electrification ratio 
#   farsighted.grid <- mutate(farsighted.grid, PercentElectrification = (houses_connected_at_start + CummulativeHousesConnected.qty)/total_houses )
#   
#   #That lets us develop Phase bins
#   farsighted.grid$Phase <- NA
#   total_phases <- 5
#   phase_increment_house <- sum(farsighted.grid$Demand..household....Target.household.count)
#   
#   for (i in 1:total_phases){
#     
#     lower_cutoff <- (i-1)/total_phases*phase_increment_house
#     upper_cutoff <- i/total_phases*phase_increment_house
#     
#     farsighted.grid$Phase[which((farsighted.grid$CummulativeHousesConnected.qty >= lower_cutoff) &
#                                   (farsighted.grid$CummulativeHousesConnected.qty <= upper_cutoff))] <- i
#     
#   }
#   
#   #Ensure Phase is considered a factor
#   farsighted.grid$Phase <- as.factor(farsighted.grid$Phase)
#   
#   #Reverse factor order of phase so color corresponds better 
#   farsighted.grid$PhasePlot = factor(farsighted.grid$Phase)
#   #farsighted.grid$PhasePlot = factor(farsighted.grid$PhasePlot, levels = rev(farsighted.grid$PhasePlot))
#   write.csv(farsighted.grid, paste0(directory_name,"/metrics-local-grid-only-rollout_sequence.csv"), row.names=F)
#   
#   
#   # #Output 'lite' version of metrics local
#   
#   shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted.grid))]
#   farsighted_all <- merge(local, farsighted.grid, by = shared_column_names, all.x=F, all.y=T)
#   
#   
#   #Order the suggested grid path by optimal sequence
#   farsighted_all <- farsighted_all[order(farsighted_all$far.sighted.sequence),]
#   
#   #Develop cummulative sum of network length metric
#   
#   farsighted_all$CummulativeNetworkExtent.m <- cumsum(farsighted_all$dist)
#   farsighted_all$CummulativeHousesConnected.qty <- cumsum(farsighted_all$Demand..household....Target.household.count)
#   
#   #Scalar Values of region before expansion efforts began
#   percent_houses_connected_at_start <- 1-sum(farsighted_all$Demographics...Projected.household.count)/(sum(farsighted_all$Full_population/farsighted_all$Ho_size))
#   houses_connected_at_start <- (sum(farsighted_all$Full_population/farsighted_all$Ho_size) - sum(farsighted_all$Demographics...Projected.household.count))
#   total_houses <- sum(farsighted_all$Full_population/farsighted_all$Ho_size)
#   
#   #Track cummulative electrification ratio 
#   farsighted_all <- mutate(farsighted_all, PercentElectrification = (houses_connected_at_start + CummulativeHousesConnected.qty)/total_houses )
#   
#   ##Output csv of 
#   write.csv(farsighted_all, paste0(directory_name,"/metrics-local-grid-only-rollout_sequence-full.csv"), row.names=F)
# }
