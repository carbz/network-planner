# Original author: Chris Natali
# adapted by carbz

# Estimating the cost of an LV Network for a set of Households
# This example is related to the networkplanner library and uses minor functions from it, but is more broad than the library as it demonstrates the utility of igraph and SP packages as well.
# 
# Load point dataset from csv and map it. These points represent households clustered in several villages.

require(sp)
require(methods)
require(RColorBrewer)
csv_file <- "~/Downloads//mmr_hh_spatial.csv"
hh_df <- read.csv(csv_file)

mapping_from <- c("household._gps_longitude", 
                  "household._gps_latitude", 
                  "Household_geocharacterization.hh_size", 
                  "Household_geocharacterization.hh_rooms")
mapping_to <- c("X", "Y", "hh_size", "hh_rooms")
hh_df[,mapping_to] <- hh_df[,mapping_from]
hh_sp_df <- hh_df[,mapping_to]

mst_igraph <- function(hh_sp_df){
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

plot(mst_g, vertex.size=4, vertex.label="")

#Calculate mean edge length and costs associated with the MST based network
network_span <- sum(E(mst_g)$weight)
