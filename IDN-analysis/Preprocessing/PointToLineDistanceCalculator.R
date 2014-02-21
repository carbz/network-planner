
require(sp)
require(maptools)
require(plyr)
#Given a points matrix with Longitude and Latitude fields and
#a foritifed line file with "long" and "lat" values specified
#this function returns a minimum the points file but with an additional variable denoting the 
#minimum distance to a vertex on the line considered 
#minimum_distance_to_line_vertices <- function(points_full, lines){
  #Cluster Composite Poitns
  points_full <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Jan2014-Preprocessing/CompositeDemographicFile.csv")
  row.names(points_full) <- points_full$Name
  points <- points_full[c("Longitude","Latitude")]
  
  #Import Shapelines of region 
  setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Feb2014-Preprocessing/Shapefiles")    
  existing_MMU <- readShapeLines("MMU_Existing_20140205-JCedits/MMU_updated20140205-JCmodified.shp") #RUNTIME ~ 00:08 mins  
  existing_NTT <- readShapeLines("NTT-20140206/NTT_AllExisting.shp") 
  existing_MMU <- fortify(existing_MMU)
  existing_NTT <- fortify(existing_NTT)
  existing <- rbind(existing_MMU, existing_NTT)
  lines <- existing
  
# fortified_existing <- fortify(existing)
#   lines<-fortified_existing
  useful_existing <- as.matrix(lines[c("long","lat")])
  
  distance_to_nearest_line <- NULL 
  
  #i=1
  for (i in 1:dim(points)[1]){
    
    nearest_dist <- min(spDistsN1(useful_existing,
                                  as.numeric(points[i,]),
                                  longlat = TRUE))
    distance_to_nearest_line[i] <- nearest_dist
  }
  
  points_full$distance_to_nearest_line <- distance_to_nearest_line
  
  return(points_full)
  
  write.csv(points_full, "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Feb2014-Preprocessing/CompositeDemographicFile_withDistToGrid.csv", row.names=F)
  
}


points_full <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Feb2014-Preprocessing/CompositeDemographicFile_withDistToGrid.csv")
points_full$Desa <- str_sub(points_full$Name, end =10L)

#Develop a new population
points_full$population_buffer_corrected <- NA

points_full_updatedPop <- data.frame(t(rep(NA,dim(points_full)[2]))) #create dataframe with same variable count as points_full
names(points_full_updatedPop) <- names(points_full) #Match column names to points_full
points_full_updatedPop <- points_full_updatedPop[-1,] #Remove NAs

desa_list <- unique(points_full$Desa)
for (i in 1:length(desa_list)){
  clusters <- points_full[which(points_full$Desa == desa_list[i]),]
  #Order clusters within Desa by their proximity to MV Line, closest to furthest
  clusters <- clusters[with(clusters, order(distance_to_nearest_line)), ]
  #Determine persons already connected to grid
  persons_electrified <- sum(clusters$full_population) - sum(clusters$pop)
  for (j in 1:dim(clusters)[1]){
    clusters$population_buffer_corrected[j] <- max((clusters$full_population[j] - persons_electrified),0)
    #update remaining population for which we need to account for grid connections 
    persons_electrified <- max((persons_electrified - clusters$full_population[j]),0)
  }
  points_full_updatedPop <- rbind.fill(points_full_updatedPop, clusters)
  points_full_updatedPop = mutate(points_full_updatedPop, 
                                  SettlementTargetElectrificationRate=population_buffer_corrected/full_population)
  points_full_updatedPop$SettlementTargetElectrificationRate[is.nan(points_full_updatedPop$SettlementTargetElectrificationRate)] <- 0
  
  }


#Resorting by cluster and adding in corrupted region classifications
points_full <- points_full[with(points_full, order(Name)),]
points_full_updatedPop <- points_full_updatedPop[with(points_full_updatedPop, order(Name)),]
points_full_updatedPop$EI_SubArea <- points_full$EI_SubArea
points_full_updatedPop$PLN_Cabang <- points_full$PLN_Cabang
points_full_updatedPop$PROVINSI <- points_full$PROVINSI
points_full_updatedPop$XY_Source <- points_full$XY_Source

write.csv(points_full_updatedPop, "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Feb2014-Preprocessing/CompositeDemographicFile_withDistToGrid_BufferAdjustedPop-20140206.csv", row.names=F)





#}
