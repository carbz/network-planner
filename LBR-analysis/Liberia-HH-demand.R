#sort NetworkPlanner's Liberia output scenarios for 50, 75 & 100 kWh/yr demand
#AKA Scenarios 271, 272 and 273 respectively
#distinguish the effective demand between urban (=>5000 pop) and rural (<5000 pop)
#average of all HH demands and % difference from 444/175 kWh/yr of Kris' urban/rural demand research 


rm(list=ls())
#libraries that all may or may not be used in script
library(plyr)
library(ggplot2)

library(stringr)


# ##Jonathan's directory
# #specify directory that shapefiles sits within 

##Edwin's directory
#setwd("C:/Dropbox/WB/Liberia/Modeling/March2013/NPOutputs/271")  



setwd("~/Dropbox/WB/Liberia/Modeling/March2013/NPOutputs/271")
low.demand <- read.csv("metrics-local.csv", skip=1)

setwd("~/Dropbox/WB/Liberia/Modeling/March2013/NPOutputs/272")
med.demand <- read.csv("metrics-local.csv", skip=1)

setwd("~/Dropbox/WB/Liberia/Modeling/March2013/NPOutputs/273")
high.demand <- read.csv("metrics-local.csv", skip=1)


#determine if urban or not? by using criteria "Demographics...Is.rural


####Low Demand - 50kWh per year
LowDemandSummary <- ddply(low.demand, .(low.demand$Demographics...Is.rural), summarize,
                          TOTAL.Demand = sum(Demand...Projected.nodal.demand.per.year,  na.rm=T),
                          TOTAL.HHolds = sum(Demographics...Projected.household.count, na.rm=T), 
                          Grid = sum(Demographics...Projected.household.count[which(Metric...System=="grid")]), 
                          Mini.Grid = sum(Demographics...Projected.household.count[which(Metric...System=="mini-grid")]), 
                          SHS = sum(Demographics...Projected.household.count[which(Metric...System=="off-grid")])
)

##demand is projected to year 30 with some growth tied to elasticity and GDP
##let's unproject demand to understand what it is in current day demands 

electricity.multiplier <- mean(low.demand$Finance...Electricity.demand.multiplier)
##Average Demand is total nodal demand divided by total number of households
LowDemandSummary <- mutate(LowDemandSummary, Average.Demand = TOTAL.Demand/TOTAL.HHolds,
                           Average.Demand.Unprojected = Average.Demand/electricity.multiplier
)


####Medium Demand - 75kWh per year
MedDemandSummary <- ddply(med.demand, .(med.demand$Demographics...Is.rural), summarize,
                       TOTAL.Demand = sum(Demand...Projected.nodal.demand.per.year,  na.rm=T),
                       TOTAL.HHolds = sum(Demographics...Projected.household.count, na.rm=T), 
                       Grid = sum(Demographics...Projected.household.count[which(Metric...System=="grid")]), 
                       Mini.Grid = sum(Demographics...Projected.household.count[which(Metric...System=="mini-grid")]), 
                       SHS = sum(Demographics...Projected.household.count[which(Metric...System=="off-grid")])
                       )
  
##demand is projected to year 30 with some growth tied to elasticity and GDP
##let's unproject demand to understand what it is in current day demands 
electricity.multiplier <- mean(med.demand$Finance...Electricity.demand.multiplier)
##Average Demand is total nodal demand divided by total number of households
MedDemandSummary <- mutate(MedDemandSummary, Average.Demand = TOTAL.Demand/TOTAL.HHolds,
                        Average.Demand.Unprojected = Average.Demand/electricity.multiplier
                        )


####High Demand - 100kWh per year
HighDemandSummary <- ddply(high.demand, .(high.demand$Demographics...Is.rural), summarize,
                          TOTAL.Demand = sum(Demand...Projected.nodal.demand.per.year,  na.rm=T),
                          TOTAL.HHolds = sum(Demographics...Projected.household.count, na.rm=T), 
                          Grid = sum(Demographics...Projected.household.count[which(Metric...System=="grid")]), 
                          Mini.Grid = sum(Demographics...Projected.household.count[which(Metric...System=="mini-grid")]), 
                          SHS = sum(Demographics...Projected.household.count[which(Metric...System=="off-grid")])
)

##demand is projected to year 30 with some growth tied to elasticity and GDP
##let's unproject demand to understand what it is in current day demands 
electricity.multiplier <- mean(high.demand$Finance...Electricity.demand.multiplier)
##Average Demand is total nodal demand divided by total number of households
HighDemandSummary <- mutate(HighDemandSummary, Average.Demand = TOTAL.Demand/TOTAL.HHolds,
                           Average.Demand.Unprojected = Average.Demand/delectricity.multiplier
)
 
write.csv(HighDemandSummary, "100kWh_yr_demand_summaries.csv", row.names=F)
write.csv(MedDemandSummary, "75kWh_yr_demand_summaries.csv", row.names=F)
write.csv(LowDemandSummary, "50kWh_yr_demand_summaries.csv", row.names=F)


