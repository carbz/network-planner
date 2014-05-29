setwd('~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/TransitionalNetworks/')


LIB <- read.csv('Ranked-Liberia-Settlement-Nodes.csv', stringsAsFactors=F)
LIB$settlement_id <- row.names(LIB)
LIB <- downstream.sum.calculator(LIB)
farsighted_grid <- far_sighted_rollout(MMR_grid_cumulatives)


LIB <- mutate(LIB,
              CumulativeNetworkExtent.m = cumsum(dist),
              CumulativeHousesConnected.qty = cumsum(Demographics...Projected.household.count))

ggplot(data=LIB, aes(x=CumulativeHousesConnected.qty, y=CumulativeNetworkExtent.m/CumulativeHousesConnected.qty)) +
  geom_line()
  

common_values_post_sequence <- function(farsighted_grid, total_phases=5){
  
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
}




read.csv('/Volumes/Archived Projects/Liberia_Geospatial_Analysis/Liberia_Modeling/2013-03/NPOutputs/273-JC working'
        file.choose(new=)