##Let's Make a script to quickly generate township level maps for MMR 
##better way to convey results of NP quickly and repeatedly
##Written in NayPyiTaw, Myanmar in response to urgent response of Edwin to MOEP office here
##Date: 9-16-14
#
#helpful troubleshooting, performance increasing bits
devtools::install_github("hadley/lineprof")
library(lineprof)

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
  ##Prepare the polygon to be mapped by color
  data <- data.frame(id=rownames(polygon@data), polygon@data, stringsAsFactors=F)
  map.df <- fortify(polygon)
  map.df <- join(map.df,data, by="id")
  polygon <- map.df


#3 Line Data from Existing Grid
existing <- importShapefile(
  "~/Dropbox/Myanmar_GIS/Modeling/GAD-MIMU_Scenarios_docs/Mandalay/702/networks-existing")

#4 Line Data for Propose Grid
proposed <- importShapefile(
  "~/Desktop/MapboxShapefiles-MMR/networks-proposed-1000kWh")

proposed_attributes <- readShapeLines(
  "~/Desktop/MapboxShapefiles-MMR/networks-proposed-1000kWh.shp")

#5Road and River Data later 
MMR_roads <- readShapeLines(
  "~/Dropbox/MMR-Training-docs/data/other_shapefiles/MMR_roads.shp")
MMR_roads_prime <- subset(MMR_roads, F_CODE_DES=='Road') #pick out primary roads only

#3-5 Load All Lines together

##load.polylines <- function(directory_name) {
  
   lines_existing <- readShapeLines("~/Dropbox/Myanmar_GIS/Modeling/GAD-MIMU_Scenarios_docs/Mandalay/702/networks-existing")
  lines_proposed <- readShapeLines("~/Desktop/MapboxShapefiles-MMR/networks-proposed-1000kWh")
  MMR_roads <- readShapeLines(
    "~/Dropbox/MMR-Training-docs/data/other_shapefiles/MMR_roads")
  # change their IDs so they don't conflict
  lines_existing <- spChFIDs(lines_existing, paste0('E.', lines_existing$FID))
  lines_proposed <- spChFIDs(lines_proposed, paste0('P.', lines_proposed$SL_ID_1))
  MMR_roads <- spChFIDs(MMR_roads, paste0('Roads.', row.names(MMR_roads)))
  
  # add a 'MVLineType' attribute
  lines_existing$MVLineType <- "Existing"
  lines_proposed$MVLineType <- lines_proposed$Phase_MV
  MMR_roads$MVLineType <- MMR_roads$F_CODE_DES

  
  #Assess which fields are common between them
  shared_col_names <- intersect(names(lines_existing),names(lines_proposed))
  lines <- rbind(lines_existing[c(shared_col_names)], 
                 lines_proposed[c(shared_col_names)])
  lines <- rbind(lines[c(shared_col_names)], 
                 MMR_roads[c(shared_col_names)])
    
#   #coerce to dataframes
#   lines_existing <- fortify(lines_existing)
#   lines_proposed <- fortify(lines_proposed)
#   MMR_roads <- fortify(MMR_roads)
#   
#   lines_existing2 <- lines_existing
#   lines_proposed2 <- lines_proposed
#   MMR_roads2<- MMR_roads
#   
#   
#   lines <- rbind(lines_existing[c(shared_col_names)], 
#                  lines_proposed[c(shared_col_names)])
#   lines <- rbind(lines, MMR_roads)
#   lines <- rbind.fill(lines_proposed2, lines_existing2)
#   lines <- rbind.fill(lines, MMR_roads2)
  
  #Develop ID field to map attributes to fortified shapeline 
  lines@data$id <- rownames(lines@data)
  lines2 <- join(fortify(lines, region="id"), lines@data, by = "id")
lines2$X <- lines2$long
lines2$Y <- lines2$lat

#   #output the dataframe type file "lines"
#   return(lines)
#   
# }



#6 Road Data
MMR_water <- readShapePoly(
  "~/Dropbox/MMR-Training-docs/data/other_shapefiles/MMR_water_Areas.shp")

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
twps <- unique(InMMR$TS_PCODE, na.rm=T)


##General Plot Function Defined ##
comprehensive_plot <- function(polygon, proposed_subset, nodes, bounding_box, text) {
  
    ggplot() + 
      coord_equal(xlim=bounding_box[1:2],ylim=bounding_box[3:4])+
      
      guides(colour = guide_legend(keywidth = 5, keyheight = 3),
             shape = guide_legend(override.aes = list(size=10),
             size = guide_legend(override.aes = list(size=10)),
             symbol = guide_legend(override.aes = list(size=10)))) + 
      
      labs(title = paste0("NEP Outputs: ",
                          polygon[['TS']][i], ###!!!!!!!!!!!! Wrong way to name the chart. Must draw from nodes, not the polygon datai
                          " Township"), 
           x = "Longitude", y="Latitude", 
           shape = 'Electrification Tech.')+ 
           #color = "Electrification Tech.", linetype = "Settlement Data Source")+
      
      
      geom_polygon(data = map.df, aes(x=long,y=lat, group=group, fill = ST), 
                   colour="black",
                   fill = 'gray',
                   size=1.25,
                   alpha=1) +
       # scale_fill_grey()
      
      geom_polygon(data = MMR_water, aes(x=long,y=lat, group=group, colour = group), 
                   colour="blue",
                   size=0.5,
                   fill='#7fcdbb',
                   alpha=1) +
      
      geom_path(data=proposed_subset, 
                aes(x=long, y=lat, group=group, colour = MVLineType, size = MVLineType)) + 
        scale_color_manual(name = 'Line Type',
                           values = c('#d7191c',
                                      '#fdae61', 
                                      '#ffffbf',
                                      '#abdda4',
                                      '#2b83ba',
                                      '#ffffff', #Existing grid
                                      '#dfc27d', #Trail
                                      '#543005'),
                           breaks=c('1','2','3','4','5','Existing','Road','Trail'),
                           labels = c('Phase 1', 'Phase 2', 'Phase 3', 'Phase 4', 'Phase 5',
                                      'Existing Grid', 
                                      'Secondary Road', 'Primary Road')) + #ROAD 
        scale_size_manual(name = 'Line Type',
                          values =c(3,3,3,3,3, #proposed phases
                                    8,#existing grid 
                                   2,1),
                          breaks=c('1','2','3','4','5','Existing','Road','Trail'),
                          labels = c('Phase 1', 'Phase 2', 'Phase 3', 'Phase 4', 'Phase 5',
                                     'Existing Grid', 
                                     'Secondary Road', 'Primary Road')) +
      geom_point(data=nodes, aes(x = X, y = Y, shape = Metric...System),
                   size = 6, colour = '#000000') +
      
      geom_text(data=text, 
                aes(x = X, y = Y,label=Name), 
                size = 15, 
                fontface=3,
                position=position_jitter(w = 0.03, h = 0.03),
                colour = "#404040",vjust = 0, hjust=0) + 
      uglify_theme()

}

}


#Subset huge files

#Mainly interested in polylines only within demographic dataset
polyline.within <- function(nodes, lines) {  
  test_lines <- lines
  
  proposed_lines_subset <- (test_lines[ which((test_lines$X > min(nodes$X-3.0)) & 
                                               (test_lines$X < max(nodes$X+3.0)) & 
                                               (test_lines$Y > min(nodes$Y-3.0)) & 
                                               (test_lines$Y < max(nodes$Y+3.0)) ),])
  return(proposed_lines_subset)
}

##Pull the extents of a single township
#i represents the observation within the polygon
#polygon can be adminstrative polygon dataset


polygon.bounds <- function(polygon, i) {  
  
  twp <- subset(polygon,TS_PCODE==twps[i])
  
  #xy_polygon <- fortify(twp)
  xy_polygon <- twp
  
  x_min <- min(xy_polygon['long'])
  x_max <- max(xy_polygon['long'])
  y_min <- min(xy_polygon['lat'])
  y_max <- max(xy_polygon['lat'])
  
  xlim <- c(x_min, x_max)
  ylim <- c(y_min, y_max)
  
  return(c(xlim,ylim))
}


##test it
  
  
#Output Directory##
output_directory <- '~/Dropbox/Myanmar/6-FinalReport+Training/NPTMission/WorkForMadameMimi/R-MapImages/TownshipMaps/'
i=1

##tripped up at i=27 ???, 50 seems to be okay
for (i in 218:length(twps)){
  
  nodes <- (subset(InMMR, TS_PCODE==twps[i]))
  
  lines_subset <- polyline.within(nodes, lines2)  
  bounding_box <- polygon.bounds(map.df,i)
  
  #Text labels are subset of 
  if (dim(nodes[which(nodes$Population >
                                 mean(nodes$Population)),])[1] >2) {
    text <- nodes[which(nodes$Population>
                          mean(nodes$Population)),]
    } else {
    text <- nodes
  }
  
  
  twp_plot <- comprehensive_plot(map.df,
                                 lines2,
                                 nodes,
                                 bounding_box,
                                 text)
  
  #Output
  #My favorite plot
  
  #Aspect Ratio: height toi width
  aspect_ratio <- (max(nodes$Y)-min(nodes$Y))/(max(nodes$X)-min(nodes$X))
  if (aspect_ratio == 'NaN' | aspect_ratio == 0){
    aspect_ratio <- 1.0
  }
  
  width <- 3500 #desired pixel width of image outputs
  
  png(filename=paste0(output_directory,
                      i,
                      '-',
                      nodes[['TS']][i],
                      '-',
                      nodes[['ST']][i],
                      '-20140925.png'), width = width, height=width*aspect_ratio)
  plot(twp_plot)
  dev.off()
} 


#Also establish a blank_theme template from Prabhas' recommendations 
blank_theme <- function() {
  theme(#axis.text=element_blank(), axis.title=element_blank(), 
    axis.ticks=element_blank(),
    panel.grid=element_blank(),
    panel.background=element_rect(fill= '#7fcdbb', colour = '#7fcdbb'),
    plot.background=element_blank())
}

uglify_theme <- function() 
  {theme(text=element_text(size=80),
      legend.text = element_text(size=60),
      axis.text = element_text(size=50),
      axis.ticks=element_blank(), 
      panel.grid=element_blank(),
      panel.background=element_rect(fill= '#7fcdbb', colour = '#7fcdbb'),
      plot.background=element_blank())
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
                               
                               
                               
                           