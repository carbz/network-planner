##Let's Make a script to quickly generate township level maps for MMR 
##better way to convey results of NP quickly and repeatedly
##Written in NayPyiTaw, Myanmar in response to urgent response of Edwin to MOEP office here
##Date: 9-16-14


##Drawing from past IDN work

require(gdata)
require(plyr)
require(stringr)
require(data.table)
require(ggplot2)
require(grid)
require(maptools)
require(stats)
require(ggmap)
require(WriteXLS)

source('~/github/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')


### KEY MAP ELEMENTS ####
#1 Point Data
metrics_local <- read.csv('/Users/carbz/Dropbox/Myanmar_GIS/Modeling/GAD-MIMU_Scenarios_docs/merged_tests/master_merged/local_lite-AllStates-1000kWhDemand.csv')

#2 Polygon Data
MMR_polygon <- readShapePoly(
  "~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")
MMR_polygon_twps <- readShapePoly(
  "~/Dropbox/Myanmar_GIS/Admin_Boundaries/5_adm3_townships1_250k_mimu/adm3_townships1_250k_mimu.shp")

#3 Line Data from Existing Grid
existing <- importShapefile(
  "~/Dropbox/Myanmar_GIS/Modeling/GAD-MIMU_Scenarios_docs/Mandalay/702/networks-existing")

#4 Line Data for Propose Grid
proposed <- importShapefile(
  "~/Desktop/MapboxShapefiles-MMR/networks-proposed-1000kWh")

#5Road and River Data later 


### Now Prep metrics_local per township ### 

#Coerce points to spatial dataframe
coordinates(metrics_local) = ~X+Y

#Subset States on interest to match Shaky's joined subset
# States <- c("Chin", "Magway")
# local_all <- subset(local_all, State %in% States) #Unreliable because ST attribute is not consistent

InMMR <- over(metrics_local, MMR_polygon_twps) 
InMMR <- cbind(metrics_local, InMMR)
table(metrics_local$Metric...System)
dim(metrics_local)

#unique townships to plot by
twps <- unique(InMMR$TS)

##General Plot Function Defined ##
comprehensive_plot <- function(polygon, proposed, existing, points) {
  
  ggplot() + 
    geom_polygon(data = polygon, aes(x=long,y=lat, group=group), alpha=0.5) +
    geom_path(data=existing, 
              aes(x=X, y=Y, group=PID), color='black') + 
    geom_path(data=proposed, 
              aes(x=X, y=Y, group=PID), color='blue') + 
    scale_size_manual(values=c(.5,1.5)) + 
    #scale_linetype_manual(values=c("solid", "dotdash")) + 
    geom_point(data=points, aes(x = X, y = Y, colour = Metric...System)) +
    
    #scale_shape_manual(values=c(20, 11), labels=c("BIG", "BPS")) +
    #scale_color_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) + 
    labs(title = paste0("NetworkPlanner Outputs: ","directory_names[i] for custom name"), 
         x = "Longitude", y="Latitude", 
         color = "Electrification Tech.", shape = "Settlement Data Source")+
    #coord_map()
    coord_equal(xlim=c(min(points$X),max(points$X)),ylim=c(min(points$Y),max(points$Y)))
}

#Subset huge files

#Mainly interested in polylines only within demographic dataset
polyline.within <- function(nodes, lines) {  
  test_lines <- lines
  
  proposed_lines_subset <- (test_lines[ which((test_lines$X > min(nodes$X-0.2)) & 
                                               (test_lines$X < max(nodes$X+0.2)) & 
                                               (test_lines$Y > min(nodes$Y-0.2)) & 
                                               (test_lines$Y < max(nodes$Y+0.2)) ),])
  return(proposed_lines_subset)
}


##test it
nodes <- (subset(InMMR, TS==twps[1]))

proposed_subset <- polyline.within(nodes, proposed)
existing_subset <- polyline.within(nodes, existing)

comprehensive_plot(MMR_polygon_twps,
                   proposed_subset,
                   existing_subset,
                   nodes)



#Subset the hell out of metrics local
local_lite <- metrics_local[,c("Name",
                               "State","District","Township",
                               "X","Y",
                               "Metric...System",
                               "far.sighted.sequence",
                               "Demand..household....Target.household.count",
                               "Demand...Projected.nodal.demand.per.year",
                               "System..grid....Transformer.cost",
                               "Demographics...Population.count",
                               "Demographics...Projected.population.count",
                               "Total.Downstream.Demand.kWh",
                               "Total.Downstream.Network.Extent.m",
                               "CumulDist","CumulHH",
                               "PhaseByMVQuintile",
                               "PhaseByHHQuintile")]
                               

write.csv(local_lite,
          '~/Desktop/MapboxShapefiles-MMR/metrics-local-AllStates-1000kWhDemand-lite.csv',
          row.names=F)
                               
                               
                               
                               
                               
                               )
                            ]


