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
polygon <- MMR_polygon_twps

#3 Line Data from Existing Grid
existing <- importShapefile(
  "~/Dropbox/Myanmar_GIS/Modeling/GAD-MIMU_Scenarios_docs/Mandalay/702/networks-existing")

#4 Line Data for Propose Grid
proposed <- importShapefile(
  "~/Desktop/MapboxShapefiles-MMR/networks-proposed-1000kWh")

proposed_attributes <- readShapeLines(
  "~/Desktop/MapboxShapefiles-MMR/networks-proposed-1000kWh.shp")

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
twps <- unique(InMMR$TS_PCODE)

##General Plot Function Defined ##
comprehensive_plot <- function(polygon, proposed, existing, nodes, bounding_box) {
  
  ggplot() + 
    coord_equal(xlim=bounding_box[1:2],ylim=bounding_box[3:4])+
    
    geom_polygon(data = polygon, aes(x=long,y=lat, group=group), 
                 colour="grey",
                 size=2.5,
                 alpha=1) +
      scale_fill_brewer(type="seq") +
    
    geom_path(data=existing, 
                aes(x=X, y=Y, group=PID), 
                size=3, color='black') + 
    
    geom_path(data=proposed, 
              aes(x=X, y=Y, group=PID), 
              size=3, color='blue') + 
      scale_size_manual(values=c(.5,1.5)) + 
      scale_linetype_manual(values=c("solid", "dotdash")) +

    geom_point(data=nodes, aes(x = X, y = Y, colour = Metric...System),
               size = 20) +
      geom_text(data=nodes, aes(x = X, y = Y,label=Name), 
                size = 20, 
                fontface=3,
                position=position_jitter(w = 0.03, h = 0.03),
                colour = "white",vjust = 0, hjust=0) + 
  
    #scale_shape_manual(values=c(20, 11), labels=c("BIG", "BPS")) +
    #scale_color_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) + 
    labs(title = paste0("NEP Outputs: ",
                        polygon[[6]][i],
                        " Township"), 
         x = "Longitude", y="Latitude", 
         color = "Electrification Tech.", shape = "Settlement Data Source")+
    #coord_map()
    uglify_theme()
}


#Subset huge files

#Mainly interested in polylines only within demographic dataset
polyline.within <- function(nodes, lines) {  
  test_lines <- lines
  
  proposed_lines_subset <- (test_lines[ which((test_lines$X > min(nodes$X-2.0)) & 
                                               (test_lines$X < max(nodes$X+2.0)) & 
                                               (test_lines$Y > min(nodes$Y-2.0)) & 
                                               (test_lines$Y < max(nodes$Y+2.0)) ),])
  return(proposed_lines_subset)
}

##Pull the extents of a single township
#i represents the observation within the polygon
#polygon can be adminstrative polygon dataset


polygon.bounds <- function(polygon, i) {  
  
  twp <- subset(polygon,TS_PCODE==twps[i])
  
  xy_polygon <- fortify(twp)
  
  x_min <- min(xy_polygon[1])
  x_max <- max(xy_polygon[1])
  y_min <- min(xy_polygon[2])
  y_max <- max(xy_polygon[2])
  
  xlim <- c(x_min, x_max)
  ylim <- c(y_min, y_max)
  
  return(c(xlim,ylim))
}


##test it
  
  
#Output Directory##
output_directory <- '~/Dropbox/Myanmar/6-FinalReport+Training/NPTMission/WorkForMadameMimi/R-MapImages/TownshipMaps/'
i=1

##tripped up at i=27 ???, 50 seems to be okay
for (i in 1:length(twps)){
  
  nodes <- (subset(InMMR, TS_PCODE==twps[i]))
 
  proposed_subset <- polyline.within(nodes, proposed)  
#   existing_subset <- polyline.within(nodes, existing)
# 
# proposed_subset <- proposed
existing_subset <- existing


  bounding_box <- polygon.bounds(polygon,i)
  
  
  twp_plot <- comprehensive_plot(MMR_polygon_twps,
                                 proposed_subset,
                                 existing_subset,
                                 nodes,
                                 bounding_box)
  
  #Output
  #My favorite plot
  
  #Aspect Ratio: height to width
  aspect_ratio <- (max(nodes$Y)-min(nodes$Y))/(max(nodes$X)-min(nodes$X))
  width <- 3500 #desired pixel width of image outputs
  
  png(filename=paste0(output_directory,
                      i,
                      '-',
                      polygon[[2]][i],
                      '-',
                      polygon[[6]][i],
                      '.png'), width = width, height=width*aspect_ratio)
  plot(twp_plot)
  dev.off()
}


#Also establish a blank_theme template from Prabhas' recommendations 
blank_theme <- function() {
  theme(#axis.text=element_blank(), axis.title=element_blank(), 
    axis.ticks=element_blank(),
    panel.grid=element_blank(),
    panel.background=element_blank())
}

uglify_theme <- function() 
  {theme(text=element_text(size=80),
      legend.text = element_text(size=60),
      axis.text = element_text(size=50),
      axis.ticks=element_blank(), 
      panel.grid=element_blank(),
      panel.background=element_blank())
      #legend.position=c(1,1), #x=0=left, y=0=top
      #legend.justification=c(0,1))
}



# #Subset the hell out of metrics local
# local_lite <- metrics_local[,c("Name",
#                                "State","District","Township",
#                                "X","Y",
#                                "Metric...System",
#                                "far.sighted.sequence",
#                                "Demand..household....Target.household.count",
#                                "Demand...Projected.nodal.demand.per.year",
#                                "System..grid....Transformer.cost",
#                                "Demographics...Population.count",
#                                "Demographics...Projected.population.count",
#                                "Total.Downstream.Demand.kWh",
#                                "Total.Downstream.Network.Extent.m",
#                                "CumulDist","CumulHH",
#                                "PhaseByMVQuintile",
#                                "PhaseByHHQuintile")]
#                                
# 
# write.csv(local_lite,
#           '~/Desktop/MapboxShapefiles-MMR/metrics-local-AllStates-1000kWhDemand-lite.csv',
#           row.names=F)
#                                
                               
                               
                               
                           