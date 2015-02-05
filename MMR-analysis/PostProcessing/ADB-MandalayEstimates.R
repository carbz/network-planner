#April 7, 2014
#Vijay wants to wrap up core messages from IDN work in a Geospatial Overlay

require(WriteXLS)
require(ggplot2)
require(plyr)


##1. INPUT: We want to read in the population dataset
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

shahn <- c('Shan (North)','Shan (East)', 'Shan (South)')
col_names <- c('Name', 
               'Township_GIS', 'District_GIS','State_GIS', 'Population')


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
total_phases <- 5
phase_increment_grid <- sum(df$MVDistance)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_grid
  upper_cutoff <- j/total_phases*phase_increment_grid
  
  df$Man.Phase.MV[which((df$Man.CumulativeNetworkExtent.m >= lower_cutoff) &
                          (df$Man.CumulativeNetworkExtent.m <= upper_cutoff))] <- j
  
}

#That lets us develop Phase bins
df$Man.Phase.HH <- NA
total_phases <- 5
phase_increment_grid <- sum(df$ProjHHCount)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_grid
  upper_cutoff <- j/total_phases*phase_increment_grid
  
  df$Man.Phase.MV[which((df$Man.Phase.HH >= lower_cutoff) &
                          (df$Man.Phase.HH <= upper_cutoff))] <- j
  
}
  

##7. OUTPUT: Write csv's now
directory_name <- '~/Dropbox/Myanmar-ADB/06-InceptionReport/'

write.csv(pop_pts, paste0(directory_name,
                          'ALL_States_MMR_AllPopPlaces_Jan23-GeoOverlayAdmins.csv'),
          row.names=F)