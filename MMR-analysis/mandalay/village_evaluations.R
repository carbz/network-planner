require(plyr); require(igraph)

# (0) set working directory to repo
setwd('~/Dropbox/Myanmar-ADB/10-draft_report/')
# (1) load in points called sequenced-results.csv
pts_path <-'VillageSurveyAnalyses/AllFormhubSurveys-Composite-withMetaData.csv'
pts <- read.csv(pts_path)

#(2) Determine basic aggregate information on pts
pt_summary <- plyr::ddply(pts, .(VILLAGE_1), summarize, 
                          households_surveyed = length(HHs_TOTAL),
                          HHs_TOTAL = HHs_TOTAL[1],
                          x_centroid = mean(household._gps_longitude, na.rm=T),
                          y_centroid = mean(household._gps_latitude, na.rm=T))


# # (3) relative urban 
# 
# ##Who is Urban?? 
# 
# #pts <- read.csv(pts_path)
# #convert to spatial object
# #sp::coordinates(pts) = ~X+Y
# sp::coordinates(pts) = ~Candidate.Longitude+Candidate.Lattitude
# 
# 
# urban_poly_path <- '~/Dropbox/Myanmar-ADB/10-draft_report/data/GRUMP/mmr_urextent/mmr_urextents/mmr_urextents.shp'
# urban_poly <- maptools::readShapePoly(urban_poly_path) #per night lights mostly
# urban_places <- over(pts,urban_poly)
# 
# 
# write.csv(as.data.frame(cbind(pts,urban_places)),
#           '~/Dropbox/Myanmar-ADB/10-draft_report/data/pts_w_urban_status.csv',
#           row.names=F)


## (5) What would some distribution networks looks like?
villages <- unique(pt_summary$VILLAGE_1)
pt_summary$LV_length <- NA
mst_sp_df <- SpatialLinesDataFrame

for (i in 1:length(villages)){
  pts_subset <- subset(pts, VILLAGE_1==villages[i])
  print(nrow(pts_subset))
  print(villages[i])
  
  if (nrow(pts_subset) > 1){ #more than one household only
  #calc igraph features
  pts_subset$Y <- pts_subset$household._gps_latitude                      
  pts_subset$X <- pts_subset$household._gps_longitude    
  
  #function to generate i graph
  g_mst <- sp_df2igraph(pts_subset)
  
  #convert to spatial datafame to
  mst_sp_df_i <- igraph2spatial_df(g_mst)
  
  #   #Make master LV lines file
  #   if (i > 1){
  #     mst_sp_df <- merge(mst_sp_df,mst_sp_df)
  #   } else if {  }
  ### failed with spRbind, merge, rbind
  #output it 
  filename = paste0("~/Dropbox/Myanmar-ADB/10-draft_report/data/MST/",
                    i,
                    "-LV-networks")
  
  writeLinesShape(mst_sp_df_i, fn=filename)

  print(sum(E(g_mst)$weight))
  pt_summary$LV_length[i] <- sum(E(g_mst)$weight)
  }
  
}


            
# (*) Output results
output_dir = 'data/village_summaries-w_LV_updated.csv'
write.csv(pt_summary, file = output_dir, row.names=F)


###~~~~~~~~~~~functions of use ~~~~~~~~~~~~~~~~~

igraph2spatial_df <- function(mst_g){
  ## Create SpatialLinesDataFrame object describing edges
  
  edges <- get.edgelist(mst_g)
  class(edges) <- "numeric"
  #edge <- get.edgelist(x)+1
  edges <- cbind(edgeNum=1:nrow(edges), v1=edges[,1], v2=edges[,2])
  xE <- apply(edges, 1, function(i) Lines(Line(cbind(c(V(mst_g)$X[i["v1"]], V(mst_g)$X[i["v2"]]), 
                                                     c(V(mst_g)$Y[i["v1"]], V(mst_g)$Y[i["v2"]]))),
                                          ID=as.character(i["edgeNum"])))
  
  xE <- SpatialLinesDataFrame(SpatialLines(xE), data=data.frame(edgeNum=1:nrow(edges)))
  
  ## Write edges to a shapefile
  return(xE)
}


sp_df2igraph <- function(hh_sp_df){
  # Original content by Chris Natali
  # adopted by carbz
  
  # Estimating the cost of an LV Network for a set of Households
  # This example is related to the networkplanner library and uses minor functions from it, 
  #but is more broad than the library as it demonstrates the utility of igraph and SP packages
  #as well.
  
  #hh_sp_df is a dataframe that must have two fields as below
  #   "X"        "Y"      
  
  coordinates(hh_sp_df) <- ~X + Y
  
  # and set the projection
  proj <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
  hh_sp_df@proj4string <- CRS(proj)
  
  require(igraph); require(geosphere)
  
  # create the full graph
  edge_df <- data.frame(t(combn(1:nrow(hh_sp_df), 2)))
  names(edge_df) <- c("from", "to")
  
  # assign distances and create graph
  edge_df$weight <- geosphere::distHaversine(hh_sp_df[edge_df$from,], hh_sp_df[edge_df$to,])
  g <- igraph::graph.data.frame(edge_df, directed=F)
  V(g)$X <- hh_sp_df@coords[,1]
  V(g)$Y <- hh_sp_df@coords[,2]
  
  # compute the MST
  mst_g <- igraph::minimum.spanning.tree(g)
  return(mst_g)
}
