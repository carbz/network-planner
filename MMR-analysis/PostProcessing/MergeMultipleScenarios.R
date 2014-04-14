#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')
source('~/github/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')

#Jonathan's Directory 
path_name <-"~/Dropbox/Myanmar_GIS/Modeling/Tests/carbajal_putzing/"

directory_names <- c("666-Chin50Buf_1000KWh_SN500",
                     "680")
                 
setwd(path_name)

#Import Phase 1 Data
localA <- read.csv(paste0(directory_names[1],"/metrics-local.csv"), skip=1) #RUNTIME ~ 00:28 mins
localA$Settlement.id <- rownames(localA) #use generic row names for unique ID of each unique settlement point
proposedA <- readShapeLines(paste0(directory_names[1],"/networks-proposed.shp"))

proj4 <- read.csv(paste0(directory_names[1],"/metrics-local.csv"), nrows=1, header = FALSE)


localB <- read.csv(paste0(directory_names[2],"/metrics-local.csv"), skip=1) #RUNTIME ~ 00:28 mins
localB$Settlement.id <- rownames(localB) #use generic row names for unique ID of each unique settlement point
proposedB <- readShapeLines(paste0(directory_names[2],"/networks-proposed.shp"))

#There are some problem variables inconsistent btw two datasets, let's pretend they exist in both
localA$Villagetr1 <- NA
localB$Vt_code <- NA

#Merge
shared_col_names <- intersect(names(localA),names(localB)) #c('Village_co', 'X','Y','Metric...System')
local_all <- merge(localA, localB, by = shared_col_names, all = T)

#Subset States on interest to match Shaky's joined subset
States <- c("Chin", "Magway")
local_all <- subset(local_all, State %in% States) #Unreliable because ST attribute is not consistent








#Coerce points to spatial dataframe
coordinates(local_all) = ~X+Y
#Inside 'Chin' State?
MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")

InMMR <- over(local_all,MMR_polygon)[2]  

local_all <- cbind(local_all, InMMR)
local_Chin_Magway <- subset(local_all, ST %in% c("Magway Region","Chin State")) #Subset by spatial query instead

#Output Results
local_all_lite <- local_all[c('X','Y','Metric...System', 'State', 'Name')]
write.csv(local_all_lite, '~/Dropbox/Myanmar_GIS/Modeling/Tests/carbajal_putzing/666_and_680_Proposed_Points_Merged-20140412.csv', row.names=F)
write.csv(local_Chin_Magway, '~/Dropbox/Myanmar_GIS/Modeling/Tests/carbajal_putzing/MergedScenarios/metrics-local.csv', row.names=F)


write.csv(localA, '~/Dropbox/Myanmar_GIS/Modeling/Tests/carbajal_putzing/666_Proposed_Points-20140412.csv', row.names=F)
write.csv(localB, '~/Dropbox/Myanmar_GIS/Modeling/Tests/carbajal_putzing/680_Proposed_Points-20140412.csv', row.names=F)

#Pulling in Shaky's join
proposed_AB <- readShapeLines("~/Dropbox/Myanmar_GIS/Modeling/Tests/666_and_680_Proposed_Grid_Merged.shp")

## Test Rollout Script 
#require(networkplanner)
#2. Read NetworkPlanner scenario assuming directory is on local machine
base_dir <- "~/Dropbox/Myanmar_GIS/Modeling/Tests/carbajal_putzing/MergedScenarios"
# np <- read_networkplan(base_dir)

source('~/github/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')
source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')

# Rollout 
#If all that stuff works, let's suggest a sequence in which to roll out the construction of grid-nodes.  This has been pre-developed and we're reapplying here 
#Importing proposed grid by itself, no existing lines as well

proposed <- proposed_AB
proposed$FID <- row.names(proposed) # ensure FID is unqiue

#Establish unique IDs for metrics local file
local <- local_Chin_Magway
local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point

proj4 <- read.csv("~/Dropbox/Myanmar_GIS/Modeling/Tests/680/metrics-local.csv", nrows=1, header = FALSE)[1]


#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~08:00***********
greedy_grid <- prioritized.grid.greedy(local,proposed, proj4)
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
writeLinesShape(proposed_with_rollout, "carbajal_putzing/MergedScenarios/networks-proposed-with-rollout.shp")




#Polygon data too, why not!
MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")
#     #now let's make it more ggplottable and keep any attribute data 
    MMR_polygon@data$id <- rownames(MMR_polygon@data)
    MMR_polygon <- merge(MMR_polygon@data, fortify(MMR_polygon), by = 'id')
    MMR_polygon<- malukuutara_polygon
MMR_polygon$State <- MMR_polygon$ST


google_earth_plot <- function(path, points) {
  
  ##This returns the left/bottom/right/top bounding box points 
  #of a given X, Y point set
  #names(location) <- c("left","bottom","right","top")
  loc <- c(min(points$X)-1, #left 
           min(points$Y)-1, #bottom
           max(points$X)+1, #right
           max(points$Y)+1) #top
  map <- get_map(location= loc)
  
  p<- ggmap(map, legend = "topleft") + 
    geom_path(data=path, aes(x=long, y=lat, group=group), color='black') + 
    scale_size_manual(values=c(.5,1.5)) + 
    scale_linetype_manual(values=c("solid", "dotdash")) + 
    geom_point(data=points, aes(x = X, y = Y, colour = Metric...System)) +
    #scale_shape_manual(values=c(20, 11), labels=c("BIG", "BPS")) +
    scale_color_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Unelectrified")) + 
    labs(title = "NetworkPlanner Outputs", x = "Longitude", y="Latitude", color = "Electrification Tech.", shape = "Settlement Data Source") +
    coord_equal(xlim=c(min(points$X),max(points$X)),ylim=c(min(points$Y),max(points$Y)))
  
  return(p)
}
#Develop map with Google Background for better reference
proposed_GE_background <- google_earth_plot(proposed_AB, local_all)
proposed_GE_background

##My favorite plot
tiff(filename="carbajal_putzing/Output-Overview-Map-GEbackground.tiff")
plot(proposed_GE_background)
dev.off()

ggsave(plot=proposed_GE_background, filename="carbajal_putzing/proposed_GE_background.png")

comprehensive_plot <- function(polygon, path, points) {
  
  ggplot() + 
    geom_polygon(data = polygon, aes(x=long,y=lat, group=group, fill=ST), alpha=0.3) +
    geom_path(data=path, aes(x=long, y=lat, group=group), color='black') + 
    scale_size_manual(values=c(.5,1.5)) + 
    scale_linetype_manual(values=c("solid", "dotdash")) + 
    geom_point(data=points, aes(x = X, y = Y, colour = Metric...System)) +
    scale_color_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Unelectrified")) + 
    labs(title = "NetworkPlanner Outputs", x = "Longitude", y="Latitude", color = "Electrification Tech.", shape = "Settlement Data Source") +
    coord_equal(xlim=c(min(points$X),max(points$X)),ylim=c(min(points$Y),max(points$Y)))
}

  #Explicitly define the plot regions of interest based on NP outputs and BPS Polygon data
  big_picture_plot <- comprehensive_plot(MMR_polygon, proposed_AB, local_all) + blank_theme() 

big_picture_plot
ggsave(plot=big_picture_plot, filename="carbajal_putzing/OutputOverView-Map.png")

#Summarize outputs by technology type (ie Off-Grid, Mini-Grid and Grid systems)
