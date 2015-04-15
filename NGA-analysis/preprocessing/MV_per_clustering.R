# Apr 10, 2015
# NGA NEAP project to characterize polling unit data 
# Objective is to run a NP scenario of visual inspected clusters (500m)
# assign pop such that it is 2x voters
# voters proportioned out in accordance to polling unit  
# end result, MV/HH vs. density 

require(WriteXLS)
require(ggplot2)
require(plyr)
library(sp)
library(rgdal)
library(maptools)

##1. INPUT: read in the population dataset
pts2000_path <-'~/Downloads/4181-NGA-KedcoRegion-2000mClusteredCompounds-600kWh/output/sequenced-results.csv'
pts100_path <-'~/Downloads/4183-NGA-KedcoRegion-500mClusteredCompounds-600kWh/output/sequenced-results.csv'
pts500_path <-'~/Downloads/4184-NGA-KedcoRegion-100mClusteredCompounds-600kWh/output/sequenced-results.csv'

pts1 <- read.csv(pts2000_path)
pts2 <- read.csv(pts100_path)
pts3 <- read.csv(pts500_path)

#truncate dataset to vital parameters 
field_names <- c("Sequence..Upstream.segment.distance.m","Pop","Cluster.group.500m",
                 "Pu.500m.density.voter.per.sqm","Mst.length.m","Voter.est","Compounds.qty",
                 "Metric...System",'Demand..household....Target.household.count')

pts1 <- pts1[,field_names]
pts2 <- pts2[,field_names]
pts3 <- pts3[,field_names]

#preserve soem meta
pts1$clustering_criteria <- 2000
pts2$clustering_criteria <- 100
pts3$clustering_criteria <- 500

#combine together
pts <- rbind.fill(pts1, pts2)
pts <- rbind.fill(pts, pts3)

#deaggregate HH aallocations

pts$Demand..household....Target.household.count_offgrid <- 0
pts$Demand..household....Target.household.count_grid <- 0

pts$Demand..household....Target.household.count_offgrid[which(pts$Metric...System=="off-grid",)] <- 
  pts$Demand..household....Target.household.count[which(pts$Metric...System=="off-grid",)]

pts$Demand..household....Target.household.count_grid[which(pts$Metric...System=="grid",)] <- 
  pts$Demand..household....Target.household.count[which(pts$Metric...System=="grid",)]

#Important summary
df <- ddply(pts, .(Cluster.group.500m, clustering_criteria), summarise,
   all_compounds = sum(Compounds.qty, na.rm=T),
   medium_voltage <- sum(Sequence..Upstream.segment.distance.m, na.rm=T),
   density_PU_group = Pu.500m.density.voter.per.sqm[1],
   Target.household.count_grid = sum(Demand..household....Target.household.count_grid, na.rm=T),
   Target.household.count_offgrid = sum(Demand..household....Target.household.count_offgrid, na.rm=T),
   MV_total = sum(Sequence..Upstream.segment.distance.m, na.rm=T)
  )

df$MV_per_HH <- df$..2/df$Target.household.count_grid

#get the right PU-group density factor in 

PU_500m_density_voter.per.sqm <- ddply(PU_500m_density_voter.per.sqmv2, .(cluster_group_500m), summarise,
                                       PU_500m_density = (100^2* sum(Sum.of.reg, na.rm=T) /sum(PU_Area_m, na.rm=T)))

df$PU_500m_density_voter.per.sqm <- NA
for (k in 1:dim(PU_500m_density_voter.per.sqm)[1]) {
  df$PU_500m_density_voter.per.sqm[which(df$Cluster.group.500m == PU_500m_density_voter.per.sqm$cluster_group_500m[k])] <- 
    PU_500m_density_voter.per.sqm$PU_500m_density[k]
}




##Plot it out 


df$clustering_criteria <- as.factor(df$clustering_criteria)
ggplot(df, aes(x=PU_500m_density_voter.per.sqm, y=MV_per_HH, color=clustering_criteria)) + geom_point(shape=1)

ggplot(df, aes(x=PU_500m_density_voter.per.sqm, y=MV_total, color=clustering_criteria)) + geom_point(shape=1)
