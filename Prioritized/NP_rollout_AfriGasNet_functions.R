# load the required R library: maptools (which in turn loads the 'sp' library as a dependency)

library(maptools)


# Merge two polyline shape files containing different collections 
combine.line.shapefiles <- function(Phase1Proposed, Phase2Proposed, proj_var = proj4) 
{ 
  # The SpatialLineDataFrame contains two main components: 
  #
  #    Spatial Line: A list in which each element is a set of parameters defining one
  #             spatial object (in this case, a single polygon)
  #    Data:    A two-dimensional attribute table with one row for each spatial feature
  #
  # Combine the two data components from each file in two steps.
  #                       
  # First, 'stack' the attribute list rows using rbind() 
  #
  # Note: This method only works if the two Shape Files have the same spatial data type 
  # and the IDENTICAL (in type, format, number) attribute table (Data component). If this is
  # not the case, you will need to create a NEW 'merged' attribute table from the Data components
  # of each input file.
  #
  mergeData <- rbind(Phase1Proposed@data, Phase2Proposed@data)
  # 
  # Next, combine the two polygon lists into a single list using c()
  #
  mergedLines <- c(Phase1Proposed@lines, Phase2Proposed@lines)
  #
  # Next, generate a new polygon ID for the new SpatialPolygonDataFrame object,
  # 
  offset = length(mergedLines)
  for (i in 1: offset)
  {
    sNew =  as.character(i)
    mergedLines[[i]]@ID = sNew
  }
  #
  # Create an identical ID field and append it to the merged Data component 
  #
  ID = c(as.character(1:length(mergedLines)))
  mergeDataWithID = cbind(ID,mergeData)
  colnames(mergeDataWithID)[2] <- "original_FID"
  
  #  Promote the merged list to a SpatialPolygons data object
  #  
  mergeLinesSP = SpatialLines(mergedLines,proj4string=CRS(proj4string(Phase1Proposed)))
  #
  #  Combine the merged Data and Polygon components into a new SpatialPolygonsDataFrame.
  #
  mySPDF = SpatialLinesDataFrame(mergeLinesSP,data = mergeDataWithID,match.ID = FALSE)
  #
  # Finally, write the new Polygon Shape File
  #
  #writeLinesShape(mySPDF,"MergedNetworks")
  #
  output = mySPDF
  return(output)
}




##We explicitly identify the starting 'ghost' node that originated in MOZambique for AfriGasNet
##PREPROCESS.NETWORKPLANNER.REULTS IS A FUNCTION THAT SIMPLIFIES AND RELATED NetworkPlanner outputs 
## for analyses in subsequent functions by tagging all networked vertices and identifying root or starting vertices
##INPUT: NetworkPlanner outputs of metrics.local csv, proposed grid shape file and optional projection defined 
##OUTPUT: A two dataframe list representing the comprehensive list of network vertices by
##(1) starting network vertices AKA 'candidat_df' and (2) all downstream vertices AKA 'non_candidate_df'  
preprocess.MozambiqueStart.scenarios <- function(local_df, shape.file, proj_var = proj4) 
{ 
  #1.0 Formatting & truncating NetworkPlanner outputs
  #rename X & Y to long and lat so correspond exactly with proposed grid shape file
  names(local_df)[names(local_df)=="X"] <- "long"
  names(local_df)[names(local_df)=="Y"] <- "lat"
  #subset local file for segments designated for grid only
  local.grid <- subset(local_df, Metric...System=="grid")
  #truncate local file for most relevant values
  local.grid <- local.grid[,c("Name",
                              "Settlement.id",
                              "long",
                              "lat",
                              "Metric...System",
                              "Demographics...Projected.household.count",
                              "Demand..household....Target.household.count",
                              "Demand...Projected.nodal.demand.per.year",
                              "System..grid....Transformer.cost",
                              "Demographics...Population.count",
                              "Demographics...Projected.population.count",
                              "phase")
                           ]
  
  ##Fortify (or flatten) proposed shapefile in order to evaluate each vertex as it relates to local grid points
  proposed.fortified <- fortify(shape.file)
  
  ##2.0 Merge local.grid along with proposed.fortified vertexe nodes 
  ##ensuring ALL non-matchimg proposed.fortified nodes are kept in the merge -> merged
  merged <- merge(local.grid, proposed.fortified, all.y = T)
  
  #3.0 Identify fake nodes as any vertex w/out corresponding local vertex
  merged$fake.node <- F
  merged$fake.node[which(round(merged$long,2)==40.48,)] <- T
  
  ## 4.1 Each "fake.node" will define the start of a unique branch origin and should be given a 'root' ID from 1 to n
  branch <- (1:sum(as.numeric(merged$fake.node == T)))
  merged$root <- NA  #keep an account of which vertices are roots, AKA branche starts 
  merged$root[which(merged$fake.node == T)] <- branch
  
  #4.2 similiarly, a 'branch' value will be tracked for all vertices based on its place in minimum spanning tree
  merged$branch <- NA 
  merged$branch[which(merged$fake.node == T)] <- branch #start points' roots and branches are one in the same
  
  ## 5.0 Find all nodes that match with segments, aka in same "group",  of "fake.nodes"
  ## and label them as "start.nodes" preserving the "branch.id" and "distance" values
  ghost.nodes <- subset(merged, (fake.node == TRUE))
  
  ## 6.0 Identify all vertices that are starting nodes out of merged  to new dataframe -> candidate.nodes
  candidate.nodes <- subset(merged, 
                            group %in% ghost.nodes$group 
                            & fake.node==FALSE)
  
  ##7.0 Identify, tag and subset out candidate nodes 
  #7.1 use arrange function to order candidate & ghosts nodes by respective unique ID values
  candidate.nodes <- arrange(candidate.nodes , id)
  ghost.nodes <- arrange(ghost.nodes, id)
  
  #7.2 preserve unique branch & root codes originating from existing network 
  candidate.nodes$branch <- ghost.nodes$branch
  candidate.nodes$root <- ghost.nodes$branch
  
  #7.3 remove candidate nodes from non.candidate nodes dataframe
  non.candidate.nodes <- subset(merged, !(id %in% ghost.nodes$id))
  
  ##8.0 Determine distance from ghost nodes (on existing network) to corresponding candidate vertices
  dist <-  dist_fun(candidate.nodes[,1:2],
                    ghost.nodes[,1:2],
                    proj_var)
  
  #*it is important to use dummy variable 'Settlement.id' to relate calculated distances to correct vertices
  dist <- data.frame(dist, Settlement.id = candidate.nodes[,"Settlement.id"])
  
  #reassign distance to candidate.nodes dataframe attributing all length to destination or candidate nodes
  candidate.nodes <- merge(candidate.nodes, dist)
  
  #Return ID'd dataframes of candidates and non.candidates where candidates have distances defiend
  output <- list(candidate.nodes, non.candidate.nodes)
  return(output)
}