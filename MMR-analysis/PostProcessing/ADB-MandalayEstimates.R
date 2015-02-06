#Feb 5, 2015
#ADB project requires specific look at Manadalay electrification needs data

require(WriteXLS)
require(ggplot2)
require(plyr)


##1. INPUT: read in the population dataset
pop_path_name <-'~/Dropbox/Myanmar-ADB/07-Analysis//0-mandalay_data_subset/ForPanosV-NatlMl-LiteVarList.csv'
pop_pts <- read.csv(pop_path_name)

##2. Convert csv pts to spatial object
# coordinates(pop_pts) = ~x+y
coordinates(pop_pts) = ~X+Y

##3. INPUT Load in Admin shapefiles
states_path <- '/Users/carbz/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp'
States <- readShapePoly(states_path) ##akin to Delhi sub-division

# twps_path <- '/Users/carbz/Dropbox/Myanmar_GIS/Admin_Boundaries/5_adm3_townships1_250k_mimu/adm3_townships1_250k_mimu.shp'
# Townships <- readShapePoly(twps_path) ##akin to voronois

##4. Which polygon are the pts inside?
# State_GIS <- over(pop_pts,MMR_polygon)
Twps_GIS <- over(pop_pts, Townships)

##5. Assign Polygon attribute data to Points
pop_pts$State_GIS=Twps_GIS$ST
pop_pts$District_GIS=Twps_GIS$DT
pop_pts$Township_GIS=Twps_GIS$TS

##6. Making dataset Mandalay relevant 

col_names <- c('Village','Township_GIS', 'District_GIS','State_GIS', 
               'Population','ProjHHCount',
               'Metric...System.x','LVLineLength',
               'GridInternalInitialCost','GridExtrenalInitialCost','TransformerCapacity',
               'MiniGridInitialCost','System..mini.grid....System.nodal.levelized.cost',
               'System..off.grid....System.initial.cost','System..off.grid....System.nodal.levelized.cost',
               'far.sighted.sequence','MVDistance', 
               'Phase_MV','Phase_ProjHH')

pop_pts_mand <- pop_pts[which(pop_pts$State == 'Mandalay'),
                    col_names]

#Convert back to df
df <- data.frame(pop_pts_mand)

#Develop Manadalay Cummulative counters
df <- arrange(df, desc(-df$far.sighted.sequence))
#Develop cummulative sum of network length metric
df <- mutate(df,
             Man.CumulativeNetworkExtent.m = as.integer(cumsum(MVDistance)),
             Man.CumulativeHousesConnected.qty = cumsum(ProjHHCount),
             Man.CumulativePersonsConnected.qty = cumsum(Population),
             Man.Sequence = 1:nrow(df)
             )

#That lets us develop MV Phase bins
df$Man.Phase.MV <- NA

df$Man.MV_investment_per_HH_by_MVPhase.m <- NA


total_phases <- 5
phase_increment_grid <- sum(df$MVDistance, na.rm=T)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_grid
  upper_cutoff <- j/total_phases*phase_increment_grid
  
  ##Who are we talking about here
    
  df$Man.Phase.MV[which((df$Man.CumulativeNetworkExtent.m >= lower_cutoff) &
                          (df$Man.CumulativeNetworkExtent.m <= upper_cutoff))] <- j
  
  df$Man.MV_investment_per_HH.by_MVPhase.m_per_HH[which((df$Man.CumulativeNetworkExtent.m >= lower_cutoff) &
                          (df$Man.CumulativeNetworkExtent.m <= upper_cutoff))]
  
  phase_hhs = sum(df$ProjHHCount[which(df$Man.Phase.MV==j)], na.rm=T)
  phase_mv = sum(df$MVDistance[which(df$Man.Phase.MV==j)], na.rm=T)
  
  df$Man.MV_investment_per_HH_by_MVPhase.m[which(df$Man.Phase.MV==j)] <- phase_mv/phase_hhs
}

#That lets us develop HH Phase bins
df$Man.Phase.HH <- NA
total_phases <- 5
phase_increment_grid <- sum(df$ProjHHCount, na.rm=T)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_grid
  upper_cutoff <- j/total_phases*phase_increment_grid
  
  df$Man.Phase.HH[which((df$Man.CumulativeHousesConnected.qty >= lower_cutoff) &
                          (df$Man.CumulativeHousesConnected.qty <= upper_cutoff))] <- j
  
}
  

##7. OUTPUT: Write csv's to directory of choice 
directory_name <- '~/Dropbox/Myanmar-ADB/07-Analysis//0-mandalay_data_subset/'

write.csv(df, paste0(directory_name,
                          'Mandalay_AllPopPlaces_20150205.csv'),
          row.names=F)