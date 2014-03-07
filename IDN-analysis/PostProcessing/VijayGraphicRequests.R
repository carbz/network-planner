
require(ggplot2)
require(reshape2)
require(stringr)


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
  scale_fill_brewer(type="seq", palette="Reds",
                    labels=c("MiniGrid Distribution", 
                             "MiniGrid Connection Costs",
                             "Generation System", 
                             "BOS, Racking + Installation Cost")) +
  labs(title = "MiniGrid Costs", x = "Per Household System Installed Capacity (W)", y="Total Capital Cost per HH (USD)", 
       fill = "Component") +
  ylim(0,2000) +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) #x=0

ggsave(plot=plot_minigrid_costcurve, filename="~/Desktop/MiniGrid-HHCosts-PerCapacity.png")

png(filename="~/Desktop/MiniGrid-HHCosts-PerCapacity.png"))
plot(plot_minigrid_costcurve)
dev.off()


adj_minigrid_costcurve$PeakDemand.kW <- as.integer(adj_minigrid_costcurve$PeakDemand.kW)
##Assume Grid standards for MG systems but with less annual demand
adj_minigrid_costcurve <- as.data.frame(mutate(minigrid_costcurve, 
                                           'distribution.low.voltage.line.equipment.cost.per.connection'=400))#,
                                           #'AnnualDemand.kWh' = PeakDemand.kW/(365*24)*(0.208333*0.85)))


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
  labs(title = "MiniGrid* Costs Built to Grid LV standards", 
       x = "Per Household System Installed Capacity (W)", 
       y="Total Capital Cost per HH (USD)", 
       fill = "MiniGrid Component Cost") +
  scale_fill_manual(values = c("#bdd7e7", "#fcae91", "#fb6a4a", "#cb181d"),
                    labels=c("Utility Grade Distribution", 
                             "MiniGrid Connection Costs", 
                             "Generation System", 
                             "BOS, Racking + Installation Cost")) +
  ylim(0,2000) +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) #x=0
  

##Pull the 175kWp capacity
minigrid_costcurve175 <- subset(adj_minigrid_costcurve, PeakDemand.kW == 175)

minigrid_costcurve175_tall <- melt(minigrid_costcurve175, id.vars=c("PeakDemand.kW"),
                                measure.vars=c("distribution.low.voltage.line.equipment.cost.per.connection", 
                                               "Distribution...Low.voltage.line.initial.cost", 
                                               "System..mini.grid....Generation.system.cost",
                                               "System..mini.grid....System.initial.generation.system.cost"))



names(minigrid_costcurve175_tall) <- c("SystemCapacity.kWp", "Component", "Cost")

plot_minigrid_cost175   <- 
  ggplot(data= minigrid_costcurve175_tall, aes(x=SystemCapacity.kWp, y=Cost, fill=Component)) + 
  geom_bar(stat='identity', width=.20) +
  theme(axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        text=element_text(size=15),
        legend.text = element_text(size=15),
        axis.text = element_text(size=15))+  #,
        #legend.position=c(0,1), #x=0=left, y=1=top
        #legend.justification=c(0,1)) +
  labs(title = "MiniGrid for 240 kWh/yr HH Demand", 
       x = "Capacity Installed per Household (W)", 
       y="Total Capital Cost per HH (USD)", 
       fill = "Cost Category") +
  scale_fill_manual(values = c("#eff3ff", "#fcae91", "#fb6a4a", "#cb181d"),
                    labels=c("Utility Grade Distribution", 
                             "MiniGrid Connection Costs", 
                             "Generation System", 
                             "BOS, Racking + Installation Cost"))+
  ylim(0,1500)

ggsave(plot=plot_minigrid_cost175, filename="~/Desktop/MultiDemand-MiniGrid-Costs-PerHH.pdf")
ggsave(plot=plot_minigrid_cost175, filename="~/Desktop/MultiDemand-MiniGrid-Costs-PerHH.png")


####################
###################

farsighted_all_metrics_lite <- rbind(all_settlements_ranked[c("Name",
                                                              'Phase',
                                                              'dist',
                                                              'Demand...Projected.nodal.demand.per.year',
                                                              'Demand..household....Target.household.count',
                                                              'PercentOfNewGridConnections',
                                                              'far.sighted.sequence',
                                                              'CumulativeHousesConnected.qty',
                                                              'Pln_cabang',
                                                              'Metric...System',
                                                              'Ei_subarea',
                                                              'Full_popul',
                                                              'X','Y')])

GridCosts <- ddply(all_settlements_ranked, .(Phase), summarise,
                   ## ***General Informaiton***
                   'Demand..household....Target.household.count' = sum(Demand..household....Target.household.count),
                   
                   ## ***Grid Component Costs***
                   'Total new MV Lines (kilometers)' = sum(dist)/1E3,
                   'Length of Network Installed per HH (meters)' = (sum(dist)/sum(Demand..household....Target.household.count)),
                   'Demand...Projected.nodal.demand.per.year' = sum(Demand...Projected.nodal.demand.per.year),

                   'System..grid....Transformer.cost'=sum(System..grid....Transformer.cost), 
                   'System..grid....Installation.cost'=sum(System..grid....Installation.cost),
                   'System..grid....Low.voltage.line.equipment.cost'=sum(System..grid....Low.voltage.line.equipment.cost),
                   'System..grid....Electricity.cost.per.year'=sum(System..grid....Electricity.cost.per.year),
             
                                      
                   ## ***MiniGrid Component Costs***
                   
                   'System..mini.grid....Generation.installation.cost'= sum(System..mini.grid....Generation.installation.cost), 
                   #"System..mini.grid....Generatation.installation.cost,
                   'System..mini.grid....Generation.system.cost'=sum(System..mini.grid....Generation.system.cost), 
                   'System..mini.grid....Low.voltage.line.equipment.cost'=sum(System..mini.grid....Low.voltage.line.equipment.cost), 
                   'Distribution...Low.voltage.line.initial.cost'=sum(Distribution...Low.voltage.line.initial.cost),
                   'System..mini.grid....Energy.storage.costs.per.year' = sum(System..mini.grid....Energy.storage.costs.per.year),
                   'System..mini.grid....System.recurring.cost.per.year'=sum(System..mini.grid....System.recurring.cost.per.year),
                   
                   ## ***OffGrid Component Costs***                  
                   'System..off.grid....Photovoltaic.panel.cost'=sum(System..off.grid....Photovoltaic.panel.cost),
                   'System..off.grid....Photovoltaic.battery.cost'=sum(System..off.grid....Photovoltaic.battery.cost),
                   'System..off.grid....Photovoltaic.balance.cost'=sum(System..off.grid....Photovoltaic.balance.cost),
                   'System..off.grid....Photovoltaic.balance.cost'=sum(System..off.grid....Photovoltaic.balance.cost),
                   'System..off.grid....System.recurring.cost.per.year'=sum(System..off.grid....System.recurring.cost.per.year)
                   )

GridCosts_perHH <- c(GridCosts[c(1:4)],
                     GridCosts[c(5:length(GridCosts))]/GridCosts$Demand..household....Target.household.count)

write.csv(GridCosts_perHH, "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/March-2014/Ternate-PerHHCosts-PerPhase.csv", row.names=F)
