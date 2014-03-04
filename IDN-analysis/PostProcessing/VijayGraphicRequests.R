


## Load In scenario inputs to form hypothetical cost curve vs. Demand

#concatanating strings to make verbose, recongizeable variable names
inputs <- read.csv(paste0(directory_name,"/metrics-job-input.csv"),stringsAsFactors=F, head=F)
inputs$V4 <- paste(inputs$V1, "-", inputs$V2)
#subset out the variable values only
inputs2 <- inputs[3]

#Naming rows for new variable names
row.names(inputs2) = inputs$V4
names(inputs2) <- 'Variable'

#Isolating MiniGrid constants of interest
minigrid_variables <- as.data.frame(t(as.numeric(inputs2[c('distribution - low voltage line cost per meter',
                                                           'demographics - mean interhousehold distance',
                                                           'distribution - low voltage line equipment cost per connection',
                                                           'system (mini-grid) - generation cost per system kilowatt',
                                                           'system (mini-grid) - generation installation cost as fraction of generation cost'),])))
# colnames(minigrid_variables) <- c('distribution.low.voltage.line.cost.per.meter',
#                                'demographics.mean.interhousehold.distance',
#                                'distribution.low.voltage.line.equipment.cost.per.connection',
#                                'system(mini-grid).generation.cost.per.system.kilowatt',
#                                'system(mini-grid).generation.installation.cost.as.fraction.of.generation.cost')
#                                 
#Establish range of capacity values 

minigrid_capacities <- seq(0,750, by=25)

minigrid_costcurve <- (NULL)
minigrid_costcurve$PeakDemand.kW <- minigrid_capacities
minigrid_costcurve <- as.data.frame(mutate(minigrid_costcurve, 
                             'distribution.low.voltage.line.cost.per.meter'=minigrid_variables$V1,
                             'demographics.mean.interhousehold.distance'=minigrid_variables$V2,
                             'distribution.low.voltage.line.equipment.cost.per.connection'=minigrid_variables$V3,
                             'system.mini.grid..generation.cost.per.system.kilowatt'=minigrid_variables$V4,
                             'system.mini.grid..generation.installation.cost.as.fraction.of.generation.cost'=minigrid_variables$V5,
                             'Distribution...Low.voltage.line.initial.cost'= demographics.mean.interhousehold.distance*distribution.low.voltage.line.cost.per.meter,
                             'System..mini.grid....Generation.system.cost' = PeakDemand.kW *system.mini.grid..generation.installation.cost.as.fraction.of.generation.cost,
                             'System..mini.grid....System.initial.generation.system.cost' = System..mini.grid....Generation.system.cost*system.mini.grid..generation.installation.cost.as.fraction.of.generation.cost))

minigrid_costcurve$PeakDemand.kW <- as.factor(minigrid_costcurve$PeakDemand.kW)

##Get ready for line graphing
minigrid_costcurve_tall <- melt(minigrid_costcurve, id.vars=c("PeakDemand.kW"),
                                measure.vars=c("distribution.low.voltage.line.equipment.cost.per.connection", 
                                            "Distribution...Low.voltage.line.initial.cost", 
                                            "System..mini.grid....Generation.system.cost",
                                            "System..mini.grid....System.initial.generation.system.cost"))

# clean up names
names(minigrid_costcurve_tall) <- c("SystemCapacity.kWp", "Component", "Cost")

minigrid_costcurve_tall$Cost <- as.numeric(minigrid_costcurve_tall$Cost)
 
# clean up values
levels(minigrid_costcurve_tall$Component) <- str_replace_all(str_replace_all(levels(minigrid_costcurve_tall$Component), "System..grid....", ""), "[.]", " ")  

# PLOT!
plot_minigrid_costcurve <- 
  ggplot(data= minigrid_costcurve_tall, aes(x=SystemCapacity.kWp, y=Cost, fill=Component, group=Component)) + 
  #geom_bar(stat='identity') +
  geom_area(position = 'stack') +
  scale_fill_brewer(type="seq", palette="Reds") +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "MiniGrid Costs", x = "System Capacity (kW of Solar Installed)", y="Total Capital Cost per HH (USD)", 
       fill = "Component")
  
   

##Assume Grid standards for MG systems but with less annual demand
adj_minigrid_costcurve <- as.data.frame(mutate(minigrid_costcurve, 
                                           'distribution.low.voltage.line.equipment.cost.per.connection'=400))
adj_minigrid_costcurve_tall <- melt(adj_minigrid_costcurve, id.vars=c("PeakDemand.kW"),
                                measure.vars=c("distribution.low.voltage.line.equipment.cost.per.connection", 
                                               "Distribution...Low.voltage.line.initial.cost", 
                                               "System..mini.grid....Generation.system.cost",
                                               "System..mini.grid....System.initial.generation.system.cost"))

names(adj_minigrid_costcurve_tall) <- c("SystemCapacity.kWp", "Component", "Cost")

plot_adjminigrid_costcurve <- 
  ggplot(data= adj_minigrid_costcurve_tall, aes(x=SystemCapacity.kWp, y=Cost, fill=Component, group = Component)) + 
  #geom_bar(stat='identity') +
  geom_area(position = 'stack') +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "MiniGrid Costs Built to Grid LV standards", x = "System Capacity (kW of Solar Installed)", y="Total Capital Cost per HH (USD)", 
       fill = "MiniGrid Component Cost") +
  scale_fill_manual(values = c("#bdd7e7", "#fcae91", "#fb6a4a", "#cb181d"),
                    labels=c("Grid Standard Connection Costs", "LV Wire (20m/home)", "Generation System", "BOS, Racking + Installation Cost"))
  

