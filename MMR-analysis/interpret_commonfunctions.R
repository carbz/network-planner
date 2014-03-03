# Common repeated functions used in evlauting raw outputs of NetworkPlanner's minimum spanning tree analysis
# Most of these functions are made for interpretting metrics.local data by aggregating by certain technology and cost types

require(gdata)
require(plyr)
require(stringr)
require(data.table)
require(ggplot2)
require(grid)
require(maptools)
require(stats)
require(PBSmapping)
require(ggmap)

summarize_metrics_local_MV4 <- function(local){
  # 
  # name_str <- names(local)
  # 
  # #identify the location of 1st column that makes sense
  #local2 <-  local[,which(str_detect(name_str, "Finance"))[1]:dim(local)[2]]
  
  local_agg <- ddply(local, .(Metric...System), summarise, 
                     sum_of_Population = sum(Demographics...Projected.population.count, na.rm=T), 
                     sum_of_Demographics...Projected.household.count = sum(Demographics...Projected.household.count, na.rm=T),
                     qty_of_settlements = length(Metric...System),
                     sum_of_Demand...Projected.nodal.demand.per.year.kWh = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
                     sum_of_Demand...Projected.nodal.discounted.demand = sum(Demand...Projected.nodal.discounted.demand, na.rm=T),
                     sum_of_Demand..peak....Projected.peak.nodal.demand.kW = sum(Demand..peak....Projected.peak.nodal.demand, na.rm=T),
                     sum_of_Demand..household....Projected.household.demand.per.year = sum(Demand..household....Projected.household.demand.per.year, na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.panel.actual.capacity = sum(System..off.grid....Photovoltaic.panel.actual.capacity, na.rm=T),
                     sum_of_System..off.grid....System.initial.cost = sum(System..off.grid....System.initial.cost,na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.panel.cost = sum(System..off.grid....Photovoltaic.panel.cost, na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.battery.cost = sum(System..off.grid....Photovoltaic.battery.cost, na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.balance.cost = sum(System..off.grid....Photovoltaic.balance.cost, na.rm=T),
                     sum_of_System..off.grid....System.recurring.cost.per.year =sum(System..off.grid....System.recurring.cost.per.year, na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.battery.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.battery.replacement.cost.per.year, na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.panel.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.panel.replacement.cost.per.year, na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.balance.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.balance.replacement.cost.per.year, na.rm=T),
                     sum_of_System..off.grid....Photovoltaic.component.operations.and.maintenance.cost.per.year = sum(System..off.grid....Photovoltaic.component.operations.and.maintenance.cost.per.year, na.rm=T),
                     sum_of_System..off.grid....System.nodal.discounted.cost = sum(System..off.grid....System.nodal.discounted.cost, na.rm=T),
                     avg_of_System..off.grid....System.nodal.levelized.cost = mean(System..off.grid....System.nodal.levelized.cost, na.rm=T),
                     sum_of_System..mini.grid....Diesel.generator.actual.system.capacity = sum(System..mini.grid....Diesel.generator.actual.system.capacity,na.rm=T),
                     sum_of_System..mini.grid....System.initial.cost = sum(System..mini.grid....System.initial.cost, na.rm=T),
                     sum_of_System..mini.grid....Diesel.generator.cost = sum(System..mini.grid....Diesel.generator.cost,na.rm=T),
                     sum_of_System..mini.grid....Diesel.generator.installation.cost = sum(System..mini.grid....Diesel.generator.installation.cost, na.rm=T),
                     sum_of_System..mini.grid....Low.voltage.line.equipment.cost = sum(System..mini.grid....Low.voltage.line.equipment.cost, na.rm=T),
                     sum_of_Distribution...Low.voltage.line.initial.cost = sum(Distribution...Low.voltage.line.initial.cost, na.rm=T),
                     sum_of_System..mini.grid....System.recurring.cost.per.year = sum(System..mini.grid....System.recurring.cost.per.year, na.rm=T),
                     sum_of_System..mini.grid....Diesel.fuel.cost.per.year = sum(System..mini.grid....Diesel.fuel.cost.per.year, na.rm=T),
                     sum_of_System..mini.grid....Diesel.generator.operations.and.maintenance.cost.per.year = sum(System..mini.grid....Diesel.generator.operations.and.maintenance.cost.per.year, na.rm=T),
                     sum_of_System..mini.grid....Diesel.generator.replacement.cost.per.year = sum(System..mini.grid....Diesel.generator.replacement.cost.per.year, na.rm=T),
                     sum_of_Distribution...Low.voltage.line.replacement.cost.per.year = sum(Distribution...Low.voltage.line.replacement.cost.per.year, na.rm=T),
                     sum_of_System..mini.grid....Low.voltage.line.equipment.operations.and.maintenance.cost.per.year = sum(System..mini.grid....Low.voltage.line.equipment.operations.and.maintenance.cost.per.year, na.rm=T),
                     sum_of_System..mini.grid....System.nodal.discounted.cost = sum(System..mini.grid....System.nodal.discounted.cost, na.rm=T),
                     avg_of_System..mini.grid....System.nodal.levelized.cost = mean(System..mini.grid....System.nodal.levelized.cost, na.rm=T),
                     sum_of_System..grid....Grid.transformer.actual.system.capacity = sum(System..grid....Grid.transformer.actual.system.capacity, na.rm=T),
                     sum_of_System..grid....Internal.system.initial.cost = sum(System..grid....Internal.system.initial.cost, na.rm=T),
                     sum_of_System..grid....Installation.cost = sum(System..grid....Installation.cost,na.rm=T),
                     sum_of_System..grid....Low.voltage.line.equipment.cost = sum(System..grid....Low.voltage.line.equipment.cost, na.rm=T),
                     sum_of_System..grid....Transformer.cost = sum(System..grid....Transformer.cost, na.rm=T),
                     sum_of_System..grid....Internal.system.recurring.cost.per.year = sum(System..grid....Internal.system.recurring.cost.per.year, na.rm=T),
                     sum_of_System..grid....Electricity.cost.per.year = sum(System..grid....Electricity.cost.per.year, na.rm=T),
                     sum_of_System..grid....Transformer.operations.and.maintenance.cost.per.year = sum(System..grid....Transformer.operations.and.maintenance.cost.per.year, na.rm=T),
                     sum_of_System..grid....Transformer.replacement.cost.per.year = sum(System..grid....Transformer.replacement.cost.per.year, na.rm=T),
                     sum_of_System..grid....Internal.system.nodal.discounted.cost = sum(System..grid....Internal.system.nodal.discounted.cost, na.rm=T),
                     avg_of_System..grid....Internal.system.nodal.levelized.cost = mean(System..grid....Internal.system.nodal.levelized.cost, na.rm=T)
  )
  
  return(local_agg)
}
#MVMax5 model has slightly different headers for minigrid, be aware of this here 
summarize_metrics_local_MV5 <- function(local){
  # 
  # name_str <- names(local)
  # 
  # #identify the location of 1st column that makes sense
  #local2 <-  local[,which(str_detect(name_str, "Finance"))[1]:dim(local)[2]]
  
  local_agg <- ddply(local, .(Metric...System), summarise, 
                         sum_of_Population = sum(Pop, na.rm=T), 
                         sum_of_Demographics...Projected.household.count = sum(Demographics...Projected.household.count, na.rm=T),
                         qty_of_settlements = length(Metric...System),
                         sum_of_Demand...Projected.nodal.demand.per.year.kWh = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
                         sum_of_Demand...Projected.nodal.discounted.demand = sum(Demand...Projected.nodal.discounted.demand, na.rm=T),
                         sum_of_Demand..peak....Projected.peak.nodal.demand.kW = sum(Demand..peak....Projected.peak.nodal.demand, na.rm=T),
                         sum_of_Demand..household....Projected.household.demand.per.year = sum(Demand..household....Projected.household.demand.per.year, na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.panel.actual.capacity = sum(System..off.grid....Photovoltaic.panel.actual.capacity, na.rm=T),
                         sum_of_System..off.grid....System.initial.cost = sum(System..off.grid....System.initial.cost,na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.panel.cost = sum(System..off.grid....Photovoltaic.panel.cost, na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.battery.cost = sum(System..off.grid....Photovoltaic.battery.cost, na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.balance.cost = sum(System..off.grid....Photovoltaic.balance.cost, na.rm=T),
                         sum_of_System..off.grid....System.recurring.cost.per.year =sum(System..off.grid....System.recurring.cost.per.year, na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.battery.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.battery.replacement.cost.per.year, na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.panel.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.panel.replacement.cost.per.year, na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.balance.replacement.cost.per.year = sum(System..off.grid....Photovoltaic.balance.replacement.cost.per.year, na.rm=T),
                         sum_of_System..off.grid....Photovoltaic.component.operations.and.maintenance.cost.per.year = sum(System..off.grid....Photovoltaic.component.operations.and.maintenance.cost.per.year, na.rm=T),
                         sum_of_System..off.grid....System.nodal.discounted.cost = sum(System..off.grid....System.nodal.discounted.cost, na.rm=T),
                         avg_of_System..off.grid....System.nodal.levelized.cost = mean(System..off.grid....System.nodal.levelized.cost, na.rm=T),
                         sum_of_System..mini.grid....Generation.System.actual.system.capacity = sum(System..mini.grid....Generation.actual.system.capacity,na.rm=T),
                         sum_of_System..mini.grid....System.initial.cost = sum(System..mini.grid....System.initial.cost, na.rm=T),
                         sum_of_System..mini.grid....Generation.cost = sum(System..mini.grid....Generation.system.cost,na.rm=T),
                         sum_of_System..mini.grid....Generation.installation.cost = sum(System..mini.grid....Generation.installation.cost, na.rm=T),
                         sum_of_System..mini.grid....Low.voltage.line.equipment.cost = sum(System..mini.grid....Low.voltage.line.equipment.cost, na.rm=T),
                         sum_of_Distribution...Low.voltage.line.initial.cost = sum(Distribution...Low.voltage.line.initial.cost, na.rm=T),
                         sum_of_System..mini.grid....System.recurring.cost.per.year = sum(System..mini.grid....System.recurring.cost.per.year, na.rm=T),
                         sum_of_System..mini.grid....Annual.Energy.Storage.Requirement.kWh = sum(System..mini.grid....Energy.storage.demand.per.year, na.rm=T),
                         sum_of_System..mini.grid....Energy.Storage.cost.per.year = sum(System..mini.grid....Energy.storage.cost.per.year, na.rm=T),
                         sum_of_System..mini.grid....Generation.operations.and.maintenance.cost.per.year = sum(System..mini.grid....Generation.operations.and.maintenance.cost.per.year, na.rm=T),
                         sum_of_System..mini.grid....Generation.replacement.cost.per.year = sum(System..mini.grid....Generation.lifetime.replacement.cost.per.year, na.rm=T),
                         sum_of_Distribution...Low.voltage.line.replacement.cost.per.year = sum(Distribution...Low.voltage.line.replacement.cost.per.year, na.rm=T),
                         sum_of_System..mini.grid....Low.voltage.line.equipment.operations.and.maintenance.cost.per.year = sum(System..mini.grid....Low.voltage.line.equipment.operations.and.maintenance.cost.per.year, na.rm=T),
                         sum_of_System..mini.grid....System.nodal.discounted.cost = sum(System..mini.grid....System.nodal.discounted.cost, na.rm=T),
                         avg_of_System..mini.grid....System.nodal.levelized.cost = mean(System..mini.grid....System.nodal.levelized.cost, na.rm=T),
                         sum_of_System..grid....Grid.transformer.desired.system.capacity     = sum(System..grid....Grid.transformer.desired.system.capacity, na.rm=T),
                         sum_of_System..grid....Grid.transformer.actual.system.capacity = sum(System..grid....Grid.transformer.actual.system.capacity, na.rm=T),
                         sum_of_System..grid....Internal.system.initial.cost = sum(System..grid....Internal.system.initial.cost, na.rm=T),
                         sum_of_System..grid....Installation.cost = sum(System..grid....Installation.cost,na.rm=T),
                         sum_of_System..grid....Low.voltage.line.equipment.cost = sum(System..grid....Low.voltage.line.equipment.cost, na.rm=T),
                         sum_of_System..grid....Transformer.cost = sum(System..grid....Transformer.cost, na.rm=T),
                         sum_of_System..grid....Internal.system.recurring.cost.per.year = sum(System..grid....Internal.system.recurring.cost.per.year, na.rm=T),
                         sum_of_System..grid....Electricity.cost.per.year = sum(System..grid....Electricity.cost.per.year, na.rm=T),
                         sum_of_System..grid....Transformer.operations.and.maintenance.cost.per.year = sum(System..grid....Transformer.operations.and.maintenance.cost.per.year, na.rm=T),
                         sum_of_System..grid....Transformer.replacement.cost.per.year = sum(System..grid....Transformer.replacement.cost.per.year, na.rm=T),
                         sum_of_System..grid....Internal.system.nodal.discounted.cost = sum(System..grid....Internal.system.nodal.discounted.cost, na.rm=T),
                         avg_of_System..grid....Internal.system.nodal.levelized.cost = mean(System..grid....Internal.system.nodal.levelized.cost, na.rm=T)
  )
  
  return(local_agg)
}


################################
####generating output tables####
################################

# Off-Grid
off.grid.summary <- function(local_agg){
  Indicator <- c("Proposed Total watts of SHS", 
                 "Proposed new Off-Grid connections", 
                 "Total Initial cost for system", 
                 "Total Recurring Costs per Year", 
                 "Total Annual demand met by Off-Grid", 
                 "Off-Grid Settlements")
  Units <- c("kW", "Households", "$", "$", "kWh/year","Qty.")
  Total <- vector(mode="double",6)
  per_HH <- vector(mode="double",6)
  data <- t(as.matrix(subset(x=local_agg,Metric...System == "off-grid")))
  
  Total[1] <- data[rownames(data) == "sum_of_System..off.grid....Photovoltaic.panel.actual.capacity"]
  Total[2] <- data[rownames(data) == "sum_of_Demographics...Projected.household.count"]
  Total[3] <- data[rownames(data) == "sum_of_System..off.grid....System.initial.cost"]   
  Total[4] <- data[rownames(data) == "sum_of_System..off.grid....System.recurring.cost.per.year"]   
  Total[5] <- data[rownames(data) == "sum_of_Demand...Projected.nodal.demand.per.year.kWh"]   
  Total[6] <- as.numeric(data[rownames(data) == "qty_of_settlements"])
  
  
  Total <- as.numeric(Total)
  per_HH <- Total/Total[2]
  per_HH[c(2)] <- NA
  per_settlement <- Total/Total[6]
  off.grid.table <- data.frame(Indicator,Units,Total,per_HH,per_settlement)
  
  return(off.grid.table)
}


# Mini-Grid
mini.grid.summary <- function(local_agg){
  
  
  Indicator <- c("Proposed Total Capacity of Mini-Grids", 
                 "Proposed new mini-grid connections", 
                 "Mini Grid Settlements", 
                 "Total Initial cost for system", 
                 "Mini-Grid Total Recurring Costs per Year",
                 "Total Annual Demand met by MiniGrids")
  
  Units <- c("kW", "Households", "Qty", "$", "$", "kWh/year")
  Total <- vector(mode="double",6)
  per_HH <- vector(mode="double",6)
  data <- t(as.matrix(subset(x=local_agg,Metric...System == "mini-grid")))
  
  Total[1] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....Diesel.generator.actual.system.capacity"])
  Total[2] <- as.numeric(data[rownames(data) == "sum_of_Demographics...Projected.household.count"])
  Total[3] <- as.numeric(data[rownames(data) == "qty_of_settlements"])
  Total[4] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....System.initial.cost"])
  Total[5] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....System.recurring.cost.per.year"])
  Total[6] <- as.numeric(data[rownames(data) == "sum_of_Demand...Projected.nodal.demand.per.year.kWh"])
  
  Total <- as.numeric(Total)
  per_HH <- Total/Total[2]
  per_settlement <- Total/Total[3]
  #per_HH[7] <- NA
  #per_settlement[7] <- NA
  mini.grid.table <- data.frame(Indicator,Units,Total,per_HH,per_settlement)
  
  return(mini.grid.table)
}


# Mini-Grid MV5
mini.grid.summary.MV5 <- function(local_agg){
  
  
  Indicator <- c("Proposed Total Capacity of Mini-Grids", 
                 "Proposed new mini-grid connections", 
                 "Mini Grid Settlements", 
                 "Total Initial Costs", 
                 "Total Recurring Costs Per Year", 
                 "Total Annual Demand met by MiniGrids", 
                 "Total Energy Storage Requirement",
                 "Annual Energy Storage Costs")
  Units <- c("kW", "Households", "Qty", "$", "$", "kWh", "kWh/yr", "$")
  Total <- vector(mode="double",8)
  per_HH <- vector(mode="double",8)
  data <- t(as.matrix(subset(x=local_agg,Metric...System == "mini-grid")))
  
  Total[1] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....Generation.System.actual.system.capacity"])
  Total[2] <- as.numeric(data[rownames(data) == "sum_of_Demographics...Projected.household.count"])
  Total[3] <- as.numeric(data[rownames(data) == "qty_of_settlements"])
  Total[4] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....System.initial.cost"])
  Total[5] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....System.recurring.cost.per.year"])
  Total[6] <- as.numeric(data[rownames(data) == "sum_of_Demand...Projected.nodal.demand.per.year.kWh"])
  Total[7] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....Annual.Energy.Storage.Requirement.kWh"])
  Total[8] <- as.numeric(data[rownames(data) == "sum_of_System..mini.grid....Energy.Storage.cost.per.year"])
  Total <- as.numeric(Total)
  per_HH <- Total/Total[2]
  per_settlement <- Total/Total[3]
  mini.grid.table <- data.frame(Indicator,Units,Total,per_HH,per_settlement)
  
  return(mini.grid.table)
}

# Grid
grid.summary <- function(local_agg, global){
  
  Indicator <- c("Existing MV line length", 
                 "Proposed MV line length", 
                 "Proposed new grid connections", 
                 "Total Initial cost for grid network (MV+LV)", 
                 "Total initial cost for MV grid network",
                 "Total initial cost for LV grid network", 
                 "System of LV Recurring costs Per Year",
                 "Total Annual Demand met by Grid", 
                 "Grid Settlements",
                 "Total Peak Demand")
  Units <- c("km", "km", "Households", "$", "$", "$", "$", "kWh/yr","Qty", "kWp")
  Total <- vector(mode="double",10)
  per_HH <- vector(mode="double",10)
  data <- t(as.matrix(subset(x=local_agg,Metric...System == "grid")))
  
  Total[1] <- global[rownames(global) == "system (grid) system total existing network length",]/1000
  Total[2] <- global[rownames(global) == "system (grid) system total proposed network length",]/1000
  Total[3] <- as.numeric(data[rownames(data) == "sum_of_Demographics...Projected.household.count",])
  Total[4] <- global[rownames(global) == "system (grid) system total external initial cost",] + global[rownames(global) == "system (grid) system total internal initial cost",]
  Total[5] <- global[rownames(global) == "system (grid) system total external initial cost",]
  Total[6] <- as.numeric(data[rownames(data) == 'sum_of_System..grid....Internal.system.initial.cost'])
  Total[7] <- as.numeric(data[rownames(data) == 'sum_of_System..grid....Internal.system.recurring.cost.per.year'])
  Total[8] <- as.numeric(data[rownames(data) == "sum_of_Demand...Projected.nodal.demand.per.year.kWh"])
  Total[9] <- as.numeric(data[rownames(data) == "qty_of_settlements"])
  Total[10] <- as.numeric(data[rownames(data) == "sum_of_Demand..peak....Projected.peak.nodal.demand.kW"])
  
  Total <- as.numeric(Total)
  per_HH <- Total/Total[3]
  per_HH[c(1,3)] <- NA
  per_settlement <- Total/Total[9]
  grid.table <- data.frame(Indicator,Units,Total,per_HH,per_settlement)
  return(grid.table)
}

# Grid with adjusted pre-existing grid figure 
grid.summary.corrected.existing <- function(local_agg, global, existing_grid, existing_conections){
  
  Indicator <- c("Existing MV line length", 
                 "Existing Utility Connections", 
                 "Proposed MV line length", 
                 "Proposed new grid connections", 
                 "Total Initial cost for grid network (MV+LV)", 
                 "Total initial cost for MV grid network",
                 "Total initial cost for LV grid network", 
                 "Total Annual Demand met by Grid", 
                 "Grid Settlements",
                 "Total Peak Demand")

  Units <- c("km", "Households", "km", "Households", "$", "$", "$", "kWh","Qty","kWp")
  Total <- vector(mode="double",10)
  per_HH <- vector(mode="double",10)
  data <- t(as.matrix(subset(x=local_agg,Metric...System == "grid")))
  
  Total[1] <- existing_grid
  Total[2] <- existing_conections 
  Total[3] <- global[rownames(global) == "system (grid) system total proposed network length",]/1000
  Total[4] <- as.numeric(data[rownames(data) == "sum_of_Demographics...Projected.household.count",])
  Total[5] <- global[rownames(global) == "system (grid) system total external initial cost",] + global[rownames(global) == "system (grid) system total internal initial cost",]
  Total[6] <- global[rownames(global) == "system (grid) system total external initial cost",]
  Total[7] <- as.numeric(data[rownames(data) == 'sum_of_System..grid....Internal.system.initial.cost'])
  Total[8] <- as.numeric(data[rownames(data) == "sum_of_Demand...Projected.nodal.demand.per.year.kWh"])
  Total[9] <- as.numeric(data[rownames(data) == "qty_of_settlements"])
  Total[10] <- as.numeric(data[rownames(data) == "sum_of_Demand..peak....Projected.peak.nodal.demand.kW"])
    
  Total <- as.numeric(Total)
  per_HH <- Total/Total[4]
  per_HH[c(4)] <- NA
  per_HH[1] <-Total[1]/existing_conections
  per_HH[2] <- NA
  per_settlement <- Total/Total[9]
  per_settlement[c(1,2)] <- NA 
  grid.table <- data.frame(Indicator,Units,Total,per_HH,per_settlement)
  return(grid.table)
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##Import Metrics Gloabl in useful way
load.global <- function(global){
  # Load global data 
  grid_type <- paste(global[,1], global[,2], sep=" ")
  global <- as.matrix(global[,3])
  row.names(global) <- grid_type
  return(global)
} 

###LOAD Desa polygons and preserve key attributed of shapefile

load.polygons <- function(polygon_directory){
  polygon <- readShapePoly(polygon_directory)
  #now let's make it more ggplottable and keep any attribute data 
  polygon@data$id <- rownames(polygon@data)
  polygon <- merge(polygon@data, fortify(polygon), by = 'id')
  
  return(polygon) 
}

load.polylines <- function(directory_name) {
  
  lines_existing <- readShapeLines(paste0(directory_name, "/networks-existing.shp"))
  lines_proposed <- readShapeLines(paste0(directory_name, "/networks-proposed.shp"))
  # change their IDs so they don't conflict
  lines_existing <- spChFIDs(lines_existing, paste0('E.', lines_existing$FID))
  lines_proposed <- spChFIDs(lines_proposed, paste0('P.', lines_proposed$FID))
  # add a 'MVLineType' attribute
  lines_existing$MVLineType <- "Existing"
  lines_proposed$MVLineType <- "Proposed"
  lines <- rbind(lines_existing, lines_proposed)
  
  #Develop ID field to map attributes to fortified shapeline 
  lines@data$id <- rownames(lines@data)
  lines <- join(fortify(lines, region="id"), lines@data, by = "id")
  
  #output the dataframe type file "lines"
  return(lines)

}

polyline.length.within <- function(local_df, directory_name) {
  #local_df <- local
  #directory_name <- '~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/62-TernateArea-MV5/'
  
  #test_lines <- proposed_all
  test_lines <- importShapefile(paste0(directory_name,"/networks-existing"))
  
  #Mainly interested in polylines only within demographic dataset
  proposed_lines_subset <- (test_lines[which((test_lines$X > min(local_df$X)) & 
                                               (test_lines$X < max(local_df$X)) & 
                                               (test_lines$Y > min(local_df$Y)) & 
                                               (test_lines$Y < max(local_df$Y)) ),])
  proposed_lines_subset <-calcLength(proposed_lines_subset)
  
  length_of_lines <- sum(proposed_lines_subset$length)
  
  return(length_of_lines)
}


#Polygons loaded have population values, let's spit it back out with Population Bins
#here we add a PopulationBin category that establishes bins that are 
#evenly distributed per number of nodes, better would be per number of polygons...
popbins <- function(polygon){
  PopulationBins <- (quantile(polygon$population, probs = c(.2, .4, .6, .8, 1), na.rm=T))
  
  polygon$PopulationBin <- 
    cut(polygon$population, c(0,PopulationBins), include.lowest = TRUE)
  levels(polygon$PopulationBin) <- c("1","2","3","4","5")
  
  return(polygon)
}

#and the plot with multi-level data identified in a 'generic' function
comprehensive_plot <- function(polygon, path, points) {
  
  ggplot() + 
    geom_polygon(data = polygon, aes(x=long,y=lat, group=group, fill=PopulationBin), alpha=0.5) +
    scale_fill_brewer(type="seq", palette="YlOrBr") +
    geom_path(data=path, 
              aes(x=long, y=lat, group=group, linetype=MVLineType), color='black') + 
    scale_size_manual(values=c(.5,1.5)) + 
    scale_linetype_manual(values=c("solid", "dotdash")) + 
    geom_point(data=points, aes(x = X, y = Y, colour = Metric...System)) +
    scale_color_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) + 
    labs(title = paste0("NetworkPlanner Outputs: ",directory_names[i]), x = "Longitude", y="Latitude", color = "Electrification Tech.", shape = "Settlement Data Source") +
    coord_equal(xlim=c(min(points$X),max(points$X)),ylim=c(min(points$Y),max(points$Y)))
  
}

google_earth_plot <- function(path, points) {
  
  ##This returns the left/bottom/right/top bounding box points 
  #of a given X, Y point set
  #names(location) <- c("left","bottom","right","top")
  loc <- c(min(points$X)-1, #left 
           min(points$Y)-1, #bottom
           max(points$X)+1, #right
           max(points$Y)+1) #top
  map <- get_map(location= loc)
  
  p<- ggmap(map, legend = "topleft") + 
    geom_path(data=path, 
              aes(x=long, y=lat, group=group, linetype=MVLineType), color='black') + 
    scale_size_manual(values=c(.5,1.5)) + 
    scale_linetype_manual(values=c("solid", "dotdash")) + 
    geom_point(data=points, aes(x = X, y = Y, colour = Metric...System)) +
    scale_color_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) + 
    labs(title = "NetworkPlanner Outputs", x = "Longitude", y="Latitude", color = "Electrification Tech.", shape = "Settlement Data Source") +
    coord_equal(xlim=c(min(points$X),max(points$X)),ylim=c(min(points$Y),max(points$Y)))
  
  return(p)
}


#Also establish a blank_theme template from Prabhas' recommendations 
blank_theme <- function() {
  theme(#axis.text=element_blank(), axis.title=element_blank(), 
    axis.ticks=element_blank(),
    panel.grid=element_blank(),
    panel.background=element_blank())
}


#Now develop ranked plot
ranked_plot <- function(polygon, farsighted.grid, path){
  ggplot() + 
     geom_polygon(data = polygon, aes(x=long,y=lat, group=group, fill=PopulationBin), alpha=0.35) +
       scale_fill_brewer(type="seq", palette="YlOrBr") +
    geom_path(data=path, 
              aes(x=long, y=lat, group=group, linetype=MVLineType), color='black') + 
      scale_size_manual(values=c(.5,1.5)) + 
      scale_linetype_manual(values=c("solid", "dotdash")) + 
     geom_point(data=farsighted.grid, aes(x = long, y = lat, colour = Phase)) +
       scale_color_brewer(palette="YlGnBu") +   
    labs(title = "Proposed Grid Rollout", x = "Longitude", y="Latitude", color = "Electrification Sequence") +
    coord_equal(xlim=c(min(farsighted.grid$long),max(farsighted.grid$long)), 
                ylim=c(min(farsighted.grid$lat),max(farsighted.grid$lat)))
}

#What does the rollout look like on Terrain map
ranked_plot_GE <- function(points){
  
  loc <- c(min(points$X)-1, #left 
           min(points$Y)-1, #bottom
           max(points$X)+1, #right
           max(points$Y)+1) #top
  map <- get_map(location= loc)
  
  p<- ggmap(map, legend = "topleft") + 
    geom_point(data=points, aes(x = X, y = Y, colour = Phase)) +
    scale_color_brewer(palette="YlGnBu") + 
    labs(title = "Proposed Grid Rollout", x = "Longitude", y="Latitude", color = "Electrification Sequence") +
    coord_equal(xlim=c(min(points$X),max(points$X)), 
                ylim=c(min(points$Y),max(points$Y)))
  return(p)
}

CombineScenarios <- function(path_name, directory_names){
  local <- read.csv(paste0(path_name,directory_names[1],"/metrics-local.csv"), skip=1, stringsAsFactors=F)
  lines_proposed <- readShapeLines(paste0(path_name,directory_names[1], "/networks-proposed.shp"))
  
  #establish alhpa-numeric codes to prefix ID and FIDS 
  prefix <- letters[1]
  #so the IDs and FIDs are not conflicting, non-unique
  lines_proposed <- spChFIDs(lines_proposed, paste0(prefix, lines_proposed$FID))
  lines_proposed$FID <- paste0(prefix,lines_proposed$FID)
  lines_proposed$MVLineType <- "Existing"
  
  #Develop ID field to map attributes to fortified shapeline 
  lines_proposed@data$id <- rownames(lines_proposed@data)

  proposed_combined <- lines_proposed 
  local_combined <- local

  
  for (i in 2:length(directory_names)){
    prefix <- letters[i]
    #import the local and shapefiles of interest
    lines_proposed <- readShapeLines(paste0(path_name,directory_names[i], "/networks-proposed.shp"))
    # change their IDs so they don't conflict
    lines_proposed <- spChFIDs(lines_proposed, paste0(prefix, lines_proposed$FID))
    lines_proposed$FID <- paste0(prefix,lines_proposed$FID)
    lines_proposed$MVLineType <- "Existing"
    #Develop ID field to map attributes to fortified shapeline 
    lines_proposed@data$id <- rownames(lines_proposed@data)    
    #combine to other proposed grids
    proposed_combined <- rbind(proposed_combined, lines_proposed)

#     #combine metrics local to the other local files
    local <- read.csv(paste0(path_name,directory_names[i],"/metrics-local.csv"), skip=1, stringsAsFactors=F)
    local_combined <- rbind.fill(local, local_combined)

  }
    
    #output the dataframe type file "lines"

  output <- vector(mode="list", 2)
  output[[1]] <- proposed_combined
  output[[2]] <- local_combined

  return(output)
    
}

