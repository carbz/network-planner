#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

rm(list=ls())

source('~/github/local/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/local/network-planner/Prioritized/GreedyRollout-GasMod.R')
source('~/github/local/network-planner/Prioritized/Custom_Rollout_Functions.R')
source('~/github/local/network-planner/Prioritized/NP_rollout_AfriGasNet_functions.R')

#Import Phase 1 Data
setwd("~/Dropbox/ENI/Natural Gas Network Modeling/Network Planner Simulations/475-7USD_BTU_Cutoff_MozambiqueOnly/")
local_MOZ <- read.csv("metrics-local.csv", skip=1) #RUNTIME ~ 00:28 mins
local_MOZ$phase <- 1 
Phase1Proposed <- readShapeLines("networks-proposed.shp")


###Import Phase 2
setwd("~/Dropbox/ENI/Natural Gas Network Modeling/Network Planner Simulations/477-20USD_BTU_Cutoff-LessPerimeterCountries-MOZ-ExistingNetwork/") 
local <- read.csv("metrics-local.csv", skip=1) #RUNTIME ~ 00:28 mins
local$phase <- 2
Phase2Proposed <- readShapeLines("To_Merge/networks-existing-proposed.shp")
proj4 <- read.csv("metrics-local.csv", nrows=1, header = FALSE)

#Combine the two 'local' phased datasets
local.joined <- rbind.fill(local_1, local_2)
local.joined$Settlement.id <- rownames(local.joined)

#Combine the two proposed datasets
proposed.merged <-  combine.line.shapefiles(Phase1Proposed, Phase2Proposed, proj_var = proj4) 


#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~08:00***********
greedy.grid <- prioritized.grid.greedy.gasmod(local.joined,proposed.merged, proj4)
##***************************

#Explicitly define greedy grid output as a dataframe
greedy.grid <- as.data.frame(greedy.grid[1])
greedy.grid$MV.line.per.kwh <- greedy.grid$MV.line.per.kwh*10#correcting for 10 derating factor

#Function to determine downstream summations for greedy grid
greedy.grid.cummulatives <- downstream.sum.calculator(greedy.grid)


# ##Determine starting vertices vs. downstream vertices based on greedy results
# ##All downstream demand and network extents are also calculated for candidate.nodes
# seperated.candidates <- seperate.greedy.starts(prioritized.grid.greedy)
# candidate.nodes <- seperated.candidates[[1]]
# non.candidated.nodes <- seperated.candidates[[2]]

write.csv(greedy.grid.cummulatives, "PipelineNodesRanked-CombineScenario475.csv", row.names=F)

#Output a new shapefile with all the attirbute data of interest
proposed2 <- merge(proposed, greedy.grid.cummulatives, by.x = "FID", by.y = "id")
writeLinesShape(proposed2, "PipelineNodesRanked-CombineScenario475")
