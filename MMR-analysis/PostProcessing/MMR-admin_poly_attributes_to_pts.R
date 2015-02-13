##Geospatial Overlay Join to attribute correct admin unit names to population points
##Input: georeferenced population points in csv
##       administrative area polygons with desired attirbute information as a .shp

##Output: a georeferenced population point csv sharing the attributes of the polygons they fall within as a .csv

##1. INPUT: We want to read in the population dataset
pop_path_name <-"/Users/carbz/Downloads/ALL_States_MMR_AllPopPlaces_Jan22.csv"
pop_pts <- read.csv(pop_path_name)
##error caused by some NAs, so get rid of them
pop_pts <- pop_pts[which(!is.na(pop_pts$Longitude)), #keep these non NA rows
                   ] #and all columns...

##2. Convert csv pts to spatial object
coordinates(pop_pts) = ~Longitude+Latitude
#coordinates(pop_pts) = ~X+Y
                   
##3. INPUT Load in Admin shapefiles
states_path <- '/Users/carbz/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp'
States <- readShapePoly(states_path) ##akin to Delhi sub-division

twps_path <- '/Users/carbz/Dropbox/Myanmar_GIS/Admin_Boundaries/5_adm3_townships1_250k_mimu/adm3_townships1_250k_mimu.shp'
Townships <- readShapePoly(twps_path) ##akin to voronois

##4. Which polygon are the pts inside?
# State_GIS <- over(pop_pts,MMR_polygon)
Twps_GIS <- over(pop_pts, Townships)

##5. Assign Polygon attribute data to Points
pop_pts$State_GIS=Twps_GIS$ST
pop_pts$District_GIS=Twps_GIS$DT
pop_pts$Township_GIS=Twps_GIS$TS

##5a. For Joe Woo, pull Shahn only

shahn <- c('Shan (North)','Shan (East)', 'Shan (South)')
col_names <- c('Name', 
               'Village'',Township_GIS', 'District_GIS','State_GIS', 
               'Population',
               'Metric...System.x','LVLineLength',
               'GridInternalInitialCost','GridExtrenalInitialCost',
               'System..mini.grid....System.nodal.levelized.cost'
               
               )

shahn_df <- pop_pts[which(pop_pts$State %in% shahn),
                    col_names]
#trim it up

##6. OUTPUT: Write csv's now
directory_name <- '~/Downloads/'

write.csv(pop_pts, paste0(directory_name,
                          'ALL_States_MMR_AllPopPlaces_Jan23-GeoOverlayAdmins.csv'),
          row.names=F)

write.csv(shahn_df, paste0(directory_name,
                          'Shahn_States_all-MMR_PopPlaces_20150204.csv'),
          row.names=F)