#sort NetworkPlanner's "grid" settlements nodes based on a global priority scheme 
#while denoting each unique branch they're originating from 
#using an implementation of Prim's algorithm on the priortized grid network resulting from Zaimings script.  

rm(list=ls())

source('~/github/local/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/local/network-planner/Prioritized/Custom_Rollout_Functions.R')

# ##Github directory
#setwd("~/github/local//network-planner/Prioritized/TestData/46/")
## eni Gas director
#setwd("~/Dropbox/ENI/Natural Gas Network Modeling/Network Planner Simulations/483-7USD_BTU_Cutoff-ReducedPoliticalParticipation-PalmaStart/") 
setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2013/230-BasewHHDem480/")
setwd("~/Dropbox/ENI/Natural Gas Network Modeling/Network Planner Simulations/492-10USD_BTU_Cutoff-TZA-MWI-RWA-BDI-UGA-KEN-MOZ-ETH-ZAF-PalmaStart/")

##1.0 - Import metrics.local for only grid-proposed nodes -> local.grid
#load metrics.local to associated settlement points with proposed grid data
local <- read.csv("metrics-local.csv", skip=1) #RUNTIME ~ 00:28 mins
local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point
#proposed <- readShapeLines("networks-proposed.shp") #RUNTIME ~ 00:08 mins
proj4 <- read.csv("metrics-local.csv", nrows=1, header = FALSE)

#Merge existing and proposed networks for interpeting pipeline network
proposed <- readShapeLines("networks-proposed.shp")
## ensure FID is unqiue
proposed$FID <- row.names(proposed)
# #merge back in subsetted MOZ dem
# setwd("~/Dropbox/ENI/Natural Gas Network Modeling/Network Planner Simulations/
# local_MOZ <- read.csv("metrics-local.csv", skip=1) #RUNTIME ~ 00:28 mins
# local_MOZ <- subset(local_MOZ, Metric...System=="grid")
# local_MOZ$Settlement.id <- rownames(local_MOZ) #use generic row names for unique ID of each unique settlement point
# local <- rbind.fill(local_MOZ, local)
# local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point

#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~08:00***********
greedy.grid <- prioritized.grid.greedy(local,proposed, proj4)
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
writeLinesShape(proposed2, "PipelineNodesRankedRollout.shp")
