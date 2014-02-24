#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

source('~/github/local/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/local/network-planner/Prioritized/Custom_Rollout_Functions.R')
source('~/github/local/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')


## SET DIRECTORY 
##Edwin's directory
#setwd("C:/Dropbox/Myanmar_GIS/Modeling/Tests/594-Kayin-1000HHDem-NoGr-LV22MV30-13c/")  

#Jonathan's Directory 
directory_name <-"~/Dropbox/Myanmar_GIS/Modeling/Tests/610-Kayin-1000HHDem-NoGr-LV22MV30-13c-SubNet7/"
setwd("~/Dropbox/Myanmar_GIS/Modeling/Tests/610-Kayin-1000HHDem-NoGr-LV22MV30-13c-SubNet7/")

#Import Phase 1 Data

local <- read.csv("metrics-local.csv", skip=1) #RUNTIME ~ 00:28 mins
local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point
proj4 <- read.csv("metrics-local.csv", nrows=1, header = FALSE)
proposed <- readShapeLines("networks-proposed.shp")

#Polygon data too, why not!

MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/MMR_adm/MMR_adm3.shp")
#     #now let's make it more ggplottable and keep any attribute data 
    MMR_polygon@data$id <- rownames(MMR_polygon@data)
    MMR_polygon <- merge(MMR_polygon@data, fortify(MMR_polygon), by = 'id')
    MMR_polygon<- malukuutara_polygon

MMR_polygon$State <- MMR_polygon$NAME_1


#1. Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~00:30 @ 1300 pts***********
greedy.grid <- prioritized.grid.greedy(local,proposed, proj4, output='lite')
##***************************

if (length(greedy.grid)==2){
  print("Houston, we have a problem with our dataframe")
  greedy.grid  <- as.data.frame(greedy.grid[1])
}

#2. Function to determine downstream summations for greedy grid
#***RUNTIME ~01:20 @ 1300 pts***********
greedy.grid.cumulatives <- downstream.sum.calculator(greedy.grid)
##***************************

#3. Function to resequence the greedy grid but now by downstream summations instead of a one step look-ahead
##Run time for 1,327 points ~ 02:20 

#***RUNTIME ~02:20 for 1,300 points***********
#farsighted.grid <- far_sighted_rollout(greedy.grid.cummulatives)
farsighted.grid <- greedy.grid.cumulatives
##***************************


#Output this file right to working directory as a csv 
write.csv(farsighted.grid, "metric-local-ranked-grid.csv", row.names=F)

# #Output a new shapefile with all the attirbute data of interest
#remove multiple nodes on line segments
metrics_local_with_sequence <- (farsighted.grid[which(!(duplicated(farsighted.grid$id))),])
proposed_with_rollout <- merge(proposed, metrics_local_with_sequence, by.x = "FID", by.y = "id")
writeLinesShape(proposed_with_rollout, "networks-proposed-with-rollout.shp")


##Develop some key tabular summary outputs and plots

grid_lines <- load.polylines(directory_name)
# #Import Metrics Gloabl stuff too, 
global <- load.global(read.csv("metrics-global.csv", stringsAsFactors=F))

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
    geom_path(data=path, 
              aes(x=long, y=lat, group=group, linetype=MVLineType), color='black') + 
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
proposed_GE_background <- google_earth_plot(grid_lines, local)
proposed_GE_background

##My favorite plot
tiff(filename=paste0(directory_name,"/Output-Overview-Map-GEbackground.tiff"), width = width, height=width*aspect_ratio)
plot(proposed_GE_background)
dev.off()


comprehensive_plot <- function(polygon, path, points) {
  
  ggplot() + 
    geom_polygon(data = polygon, aes(x=long,y=lat, group=group, fill=State), alpha=0.3) +
    geom_path(data=path, 
              aes(x=long, y=lat, group=group, linetype=MVLineType), color='black') + 
    scale_size_manual(values=c(.5,1.5)) + 
    scale_linetype_manual(values=c("solid", "dotdash")) + 
    geom_point(data=points, aes(x = X, y = Y, colour = Metric...System)) +
    scale_color_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Unelectrified")) + 
    labs(title = "NetworkPlanner Outputs", x = "Longitude", y="Latitude", color = "Electrification Tech.", shape = "Settlement Data Source") +
    coord_equal(xlim=c(min(points$X),max(points$X)),ylim=c(min(points$Y),max(points$Y)))
}

  #Explicitly define the plot regions of interest based on NP outputs and BPS Polygon data
  big_picture_plot <- comprehensive_plot(MMR_polygon, grid_lines, local) + blank_theme() 

big_picture_plot
 
#My favorite plot
  tiff("Output-Overview-Map.tiff")
  plot(big_picture_plot)
  dev.off()

#Summarize outputs by technology type (ie Off-Grid, Mini-Grid and Grid systems)

local <- mutate(local, Pop=Population) #nomenclature
summary <- summarize_metrics_local_MV4(local)

#Grid Summary
grid<-grid.summary(summary, global)
mg <- mini.grid.summary(summary)
og <- off.grid.summary(summary)

options("scipen"=100, "digits"=2)
  
  all_systems_summary <- rbind(grid, mg, og) 
require(WriteXLS)  
WriteXLS("all_systems_summary", "MetricsLocal-MVMax5-SingleSheetSummary.xls")
  
#order farsighted grid by sequence
farsighted.grid$far.sighted.sequence <- farsighted.grid$sequence
farsighted.grid <- farsighted.grid[order(farsighted.grid$far.sighted.sequence),]
new_grid_connections <- sum(farsighted.grid$Demand..household....Target.household.count)

#Establish some Castalia Metrics 
#Develop cummulative sum of network length metric
farsighted.grid <- mutate(farsighted.grid, 
                          CumulativeNetworkExtent.m = cumsum(dist),
                          CumulativeHousesConnected.qty = cumsum(Demand..household....Target.household.count),
                          MVLinePerConnection = dist/Demand..household....Target.household.count,
                          TransformerCostPerConnection = System..grid....Transformer.cost/Demand..household....Target.household.count,
                          PercentOfNewGridConnections = CumulativeHousesConnected.qty/new_grid_connections)


#That lets us develop Phase bins
farsighted.grid$Phase <- NA
total_phases <- 5
phase_increment_house <- sum(farsighted.grid$Demand..household....Target.household.count)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_house
  upper_cutoff <- j/total_phases*phase_increment_house
  
  farsighted.grid$Phase[which((farsighted.grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                (farsighted.grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
  
}

#Reverse factor order of phase so color corresponds better 
farsighted.grid$PhasePlot = as.factor(farsighted.grid$Phase)
farsighted.grid$PhasePlot = factor(farsighted.grid$PhasePlot, levels = rev(farsighted.grid$PhasePlot))
  
#Now develop ranked plot
ranked_plot <- function(polygon, farsighted.grid, path){
  ggplot() + 
    geom_polygon(data = polygon, aes(x=long,y=lat, group=group, fill=State), alpha=0.35) +
    geom_path(data=path, 
              aes(x=long, y=lat, group=group, linetype=MVLineType), color='black') + 
    scale_size_manual(values=c(.5,1.5)) + 
     scale_linetype_manual(values=c("solid", "dotdash")) + 
    geom_point(data=farsighted.grid, aes(x = long, y = lat, colour = PhasePlot)) +
      scale_color_brewer(palette="YlGnBu") + 
      labs(title = "Proposed Grid Rollout", x = "Longitude", y="Latitude", color = "Electrification Sequence") +
    coord_equal(xlim=c(min(farsighted.grid$long),
                       max(farsighted.grid$long)), 
                ylim=c(min(farsighted.grid$lat),
                       max(farsighted.grid$lat)))
}
rollout_plot <- ranked_plot(MMR_polygon, farsighted.grid, grid_lines) + blank_theme()
rollout_plot 

  #Output Map png
  tiff("Rollout-Overview-Map.tiff")
  plot(rollout_plot)
  dev.off()

#Binning Categories
#It can be useful to sort the NP results of settlement data by bins.  Ex, phases for construction, technology by settlement size, cost buckets, etc. 
#Output 'lite' version of metrics local

#shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted.grid))]
#grid_settlements_ranked <- merge(local, farsighted.grid, by = shared_column_names, all.x=F, all.y=T)

#Order the suggested grid path by optimal sequence
#grid_settlements_ranked <- arrange(grid_settlements_ranked, sequence)
grid_settlements_ranked <- farsighted.grid

##Output XLS of 2 worksheets, 1 for standalones and one for grid nodes
standalone_systems <- subset(local, ((Metric...System == "mini-grid") |
                                       Metric...System == "off-grid"))

WriteXLS(c("grid_settlements_ranked", "standalone_systems"), "ForCastalia-metrics-local-rollout-sequenced.xls")

#Now pass through everything, grid and non grid alike'
shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted.grid))]
#shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted.grid))]
#grid_settlements_ranked <- merge(local, farsighted.grid, by = shared_column_names, all.x=F, all.y=T)

farsighted_all_combined <- merge(local, farsighted.grid, by = shared_column_names, all.x=T, all.y=T)
farsighted_all_combined$Phase[which(farsighted_all_combined$Metric...System != 'grid')] <- 'StandaloneSystems' 


#   #Phase Bins already developed in the rollout sections, so let's aggregate by that
#   #Now we can summarize key metrics by phase
  phase_summary <- ddply(farsighted_all_combined, .(Phase), summarise, 
                         sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
                         sum_of_Demand...Projected.nodal.discounted.demand = sum(Demand...Projected.nodal.discounted.demand, na.rm=T),
                         sum_of_Demand..peak....Projected.peak.nodal.demand = sum(Demand..peak....Projected.peak.nodal.demand, na.rm=T),
                         sum_of_Demand..household....Projected.household.demand.per.year = sum(Demand..household....Projected.household.demand.per.year, na.rm=T),
                         sum_of_System..grid....Internal.system.initial.cost = sum(System..grid....Internal.system.initial.cost, na.rm=T),
                         sum_of_System..grid....Installation.cost = sum(System..grid....Installation.cost,na.rm=T),
                         sum_of_System..grid....Low.voltage.line.equipment.cost = sum(System..grid....Low.voltage.line.equipment.cost, na.rm=T),
                         sum_of_System..grid....Transformer.cost = sum(System..grid....Transformer.cost, na.rm=T),
                         sum_of_System..grid....Internal.system.recurring.cost.per.year = sum(System..grid....Internal.system.recurring.cost.per.year, na.rm=T),
                         sum_of_System..grid....Electricity.cost.per.year = sum(System..grid....Electricity.cost.per.year, na.rm=T),
                         sum_of_System..grid....Transformer.operations.and.maintenance.cost.per.year = sum(System..grid....Transformer.operations.and.maintenance.cost.per.year, na.rm=T),
                         sum_of_System..grid....Transformer.replacement.cost.per.year = sum(System..grid....Transformer.replacement.cost.per.year, na.rm=T),
                         sum_of_System..grid....Internal.system.nodal.discounted.cost = sum(System..grid....Internal.system.nodal.discounted.cost, na.rm=T),
                         avg_of_System..grid....Internal.system.nodal.levelized.cost = mean(System..grid....Internal.system.nodal.levelized.cost, na.rm=T),
                         sum_of_Population = sum(Pop, na.rm=T), 
                         sum_of_Target_Households = sum(Demand..household....Target.household.count, na.rm=T), 
                         sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
                         sum_of_installed_MV_network.m = sum(dist),
                         electrification_achieved = max(PercentOfNewGridConnections)
  )
  write.csv(phase_summary, "PhaseSummaryofAnticipatedGridRollout.csv", row.names=F)
  
  #Develop Phase summaries on a per household level to come up with "average" metrics
  phase_summary_perHH <- ddply(farsighted_all, .(Phase), summarise,
                               sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year) / sum(Demand..household....Target.household.count),
                               sum_of_Demand...Projected.nodal.discounted.demand = sum(Demand...Projected.nodal.discounted.demand) / sum(Demand..household....Target.household.count),
                               sum_of_Demand..peak....Projected.peak.nodal.demand = sum(Demand..peak....Projected.peak.nodal.demand) / sum(Demand..household....Target.household.count),
                               sum_of_Demand..household....Projected.household.demand.per.year = sum(Demand..household....Projected.household.demand.per.year) / 
                                 sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Internal.system.initial.cost = sum(System..grid....Internal.system.initial.cost) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Installation.cost = sum(System..grid....Installation.cost) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Low.voltage.line.equipment.cost = sum(System..grid....Low.voltage.line.equipment.cost) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Transformer.cost = sum(System..grid....Transformer.cost) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Internal.system.recurring.cost.per.year = sum(System..grid....Internal.system.recurring.cost.per.year) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Electricity.cost.per.year = sum(System..grid....Electricity.cost.per.year) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Transformer.operations.and.maintenance.cost.per.year = sum(System..grid....Transformer.operations.and.maintenance.cost.per.year) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Transformer.replacement.cost.per.year = sum(System..grid....Transformer.replacement.cost.per.year) / sum(Demand..household....Target.household.count),
                               sum_of_System..grid....Internal.system.nodal.discounted.cost = sum(System..grid....Internal.system.nodal.discounted.cost) / sum(Demand..household....Target.household.count),
                               avg_of_System..grid....Internal.system.nodal.levelized.cost = mean(System..grid....Internal.system.nodal.levelized.cost) / sum(Demand..household....Target.household.count),
                               
                               sum_of_Target_Households = sum(Demand..household....Target.household.count) / sum(Demand..household....Target.household.count), 
                               
                               sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year) / sum(Demand..household....Target.household.count),
                               
                               sum_of_installed_MV_network.m = sum(dist) / sum(Demand..household....Target.household.count)
  )
  
  write.csv(phase_summary_perHH, paste0(directory_name,
                                        "/",
                                        str_sub(directory_names[i], end = str_locate(directory_names[i], "-")[1]),
                                        "PhaseSummaryofAnticipatedGridRollout-perHH.csv"), row.names=F)
#   
#   ##Prabhas' tips on bar graphs
#   require(reshape2); require(stringr)
#   
#   
#   farsighted_some <- subset(farsighted_all, select=c("Phase", 
#                                                      "System..grid....Transformer.cost", 
#                                                      "System..grid....Installation.cost", 
#                                                      "System..grid....Low.voltage.line.equipment.cost", 
#                                                      "System..grid....Electricity.cost.per.year",
#                                                      "dist"))
#   
#   farsighted_some$MV.Wire.Costs <- farsighted_some$dist * 30
#   
#   farsighted_some_tall <- melt(farsighted_some, id.vars=c("Phase"),
#                                measure.vars=c("System..grid....Installation.cost", 
#                                               "System..grid....Low.voltage.line.equipment.cost", 
#                                               "System..grid....Electricity.cost.per.year",
#                                               "System..grid....Transformer.cost",
#                                               "MV.Wire.Costs"))
#   
#   # clean up names
#   names(farsighted_some_tall) <- c("Phase", "Type", "Cost") 
#   # clean up values
#   
#   levels(farsighted_some_tall$Type) <- str_replace_all(str_replace_all(levels(farsighted_some_tall$Type), "System..grid....", ""), "[.]", " ")  
#   
#   # PLOT!
#   phase_costs <- ggplot(farsighted_some_tall, aes(x=Phase, y=Cost, fill=Type)) + geom_bar(stat='identity')
#   
#   #Output Bar charts 
#   tiff(filename=paste0(directory_name,"/TotalCost-Summary-Phased.tiff"))
#   plot(phase_costs)
#   dev.off()
#   
#   
#   #What does the rollout look like on Terrain map 
#   rollout_plot_GE <- ranked_plot_GE(farsighted_all_combined) + blank_theme()
#   ##My favorite plot
#   tiff(filename=paste0(directory_name,
#                        "/",
#                        str_sub(directory_names[i], end = str_locate(directory_names[i], "-")[1]),                
#                        "Output-Overview-Map-GE.tiff"), 
#        width = width, height=width*aspect_ratio)
#   plot(rollout_plot_GE)
#   dev.off()





