#Evaluate how BPS settlements perform against BIG settlements 
require(stringr)
require(plyr)

desa_characteristics <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Feb2014-Preprocessing/Shapefiles/DesaDemographicsandGeographics.csv")
desa_characteristics <- desa_characteristics[c("VILLAGE_CO","PLN_HHOLD","NO_PLN","AREA_KM2","TOT_POP","TOT_HHOLD","POP_DEN","HHOLD_DEN")]
colnames(desa_characteristics)[1] <- "Desa"

#measure performance of output scenarios 
#path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/"
path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/"

directory_names <- c("65-FloresBarat-MV5",
                     "64-FloresTimor-MV5",
                     "63-SumbaArea-MV5",
                     "59-KupanArea-MV5",
                     "62-TernateArea-MV5",
                     "61-TualArea-MV5",
                     "60-AmbonArea-MV5")

all_metrics_local <- data.frame(NULL)
for (i in 1:length(directory_names)){
  
  directory_name <- paste0(path_name, directory_names[i])
  #directory_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/64-FloresTimor-MV5")

  # #Import metrics local for each island areas in the analysis
  local <- read.csv(paste0(directory_name,"/metrics-local-grid-only-rollout_sequence-full.csv"), stringsAsFactors=F)
  local <- local[c("Name","dist",
                   "Demand..household....Target.household.count",
                   "Ei_subarea",
                   "Pln_cabang",
                   "Full_population",
                   "Target_household_penetration_rate",
                   "depth",
                   "Demographics...Mean.household.size",
                   "Demographics...Projected.population.count"
                   
                   )]
  
  all_metrics_local <- rbind.fill(all_metrics_local, local)
}

all_metrics_local$Desa <- str_sub(all_metrics_local$Name, end=10L)
all_metrics_local$cluster_count <- str_sub(all_metrics_local$Name, start=11L, end=12L)

desa_MV_lengths <- ddply(all_metrics_local, .(Desa), summarize,
                         Total.MV.Needed = sum(dist),
                         Settlements = max(cluster_count),
                         Demand..household....Target.household.count = sum(Demand..household....Target.household.count),
                         Avg_HHold_Size = mean(Demographics...Mean.household.size),
                         Demographics...Projected.population.count.GridOnly = sum(Demographics...Projected.population.count),
                         Full_population.GridOnly = sum(Full_population),
                         Target_household_penetration_rate = mean(Target_household_penetration_rate),
                         Ei_subarea = Ei_subarea[1],
                         Pln_cabang =Pln_cabang[1],
                         Maximum_MV_Depth = max(depth)
                         )

#Now Combine the two
desa_results <- merge(desa_MV_lengths, desa_characteristics, by = "Desa", all.x =TRUE)
desa_results$XY_Source <- NA

for (i in 1:dim(desa_results)[1]){
  if (desa_results$Settlements[i] == 99){
    desa_results$Settlements[i] = 1
    desa_results$XY_Source[i] = "BPS"
  } else {
    desa_results$XY_Source[i] = "BIG"
  }
}


setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/")
write.csv(desa_results, "DesaLevelCharacteristics-20140207.csv", row.names=F)



# 
# 
# #Evaluating 375m buffer process to subset settlements already grid connected
# #in Maluku Utara
# #how much does this agree with Desa level stastics?
# 
# buffered_pooints_EA <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/83-Ternate-EA_pop_Buffered/demographics.csv")
# BPS_pooints <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/84-Ternate-JC_orig_pops/demographics.csv")
# 
# buffered_pooints_EA <- buffered_pooints_EA[c("Name","pop")]
# colnames(buffered_pooints_EA)[2] <- "pop_outside_375m_only"
# 
# merged_datasets <- merge(BPS_pooints, buffered_pooints_EA, by="Name", all.x=T)
# 
# desa_comparison <- ddply(merged_datasets, summarise)


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
