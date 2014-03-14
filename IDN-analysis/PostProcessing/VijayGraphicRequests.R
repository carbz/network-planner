
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

minigrid_capacities <- seq(0,800, by=100)

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
minigrid_costcurve250 <- subset(adj_minigrid_costcurve, PeakDemand.kW == 250)

minigrid_costcurve250_tall <- melt(minigrid_costcurve250, id.vars=c("PeakDemand.kW"),
                                measure.vars=c("distribution.low.voltage.line.equipment.cost.per.connection", 
                                               "Distribution...Low.voltage.line.initial.cost", 
                                               "System..mini.grid....Generation.system.cost",
                                               "System..mini.grid....System.initial.generation.system.cost"))



names(minigrid_costcurve250_tall) <- c("SystemCapacity.kWp", "Component", "Cost")

plot_minigrid_cost250   <- 
  ggplot(data= minigrid_costcurve250_tall, aes(x=SystemCapacity.kWp, y=Cost, fill=Component)) + 
  geom_bar(stat='identity', width=.20) +
  theme(axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        text=element_text(size=15),
        legend.text = element_text(size=15),
        axis.text = element_text(size=15),  #,
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) +
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

ggsave(plot=plot_minigrid_cost250, filename="~/Desktop/MultiDemand-MiniGrid-Costs-PerHH.pdf")
ggsave(plot=plot_minigrid_cost250, filename="~/Desktop/MultiDemand-MiniGrid-Costs-PerHH.png")


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


####Settlement Densities by Bin 

##Developing Bin Classifications
local.binned <- all_settlements_ranked[c('Demographics...Projected.household.count',
                                         'Metric...System', 
                                         'Phase')]
#Remove 0 populations, or NAs
local.binned$Demographics...Projected.household.count[which(local.binned$Demographics...Projected.household.count==0)] <- NA

###Bins by Equal HHs 
#Sort local dataframe by HHold size
local.binned <- ddply(local.binned, "Demographics...Projected.household.count")
#add new column "HHcumsum" stores the cummulative count of HHolds
local.binned$Demographics...Projected.household.count.HHcumsum <- 
  cumsum(local.binned$Demographics...Projected.household.count)

###summarize number of settlements in bins sized by equal number of Settlements-works
#Determine HHsize/settlement breaks that split settlements into specified percentages
HHoldBinsEqualSettlementQty <- quantile(local.binned$Demographics...Projected.household.count, 
                                        probs = c(0, .2, .4, .6, .8, 1), na.rm=T)#break settlements into quantiles @ 20, 40, 60, 80 & 100%
#Determine Settlement Bins                        
local.binned$Demographics...Projected.household.count.SettlementBin <- 
  cut(local.binned$Demographics...Projected.household.count, 
      breaks = HHoldBinsEqualSettlementQty, 
      include.lowest = TRUE,
      labels = paste('<', HHoldBinsEqualSettlementQty[2:length(HHoldBinsEqualSettlementQty)]))

HHoldBins_EqualSettlements = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.SettlementBin, fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement - Equal Settlements per Bin", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")


#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-withNAs.tiff"))
plot(HHoldBins_EqualSettlements)
dev.off()
HHoldBins_EqualSettlements

#for posterity's sake, output csv with other bin categories defiend 

##Assign bins to original dataset based on fixed predefined thresholds for households/settlement - works
local.binned$Demographics...Projected.household.count.predefinedbin <- 
  cut(local.binned$Demographics...Projected.household.count, 
      c(0, 11, 21, 51, 101, 250, 501, 1000, Inf),
      labels = paste('<', c(11, 21, 51, 101, 250, 501, 1000, Inf)))

PresetBins = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=Phase)) +
  scale_fill_manual(values = c('#08519c',
                               '#3182bd',
                               '#6baed6',
                               '#bdd7e7',
                               '#eff3ff',
                               "#d7191c", "#abdda4", "#ffffbf"))+ 
  # labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement - preset Bins", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
PresetBins


PresetBins_withTransitionalMGs = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=Phase)) +
  scale_fill_manual(values = c('#08519c',
                               '#3182bd',
                               '#6baed6',
                               '#bdd7e7',
                               '#756bb1',
                               "#d7191c", "#abdda4", "#ffffbf"),
                    labels=c('1','2','3','4','MiniGrid*/ Future Grid', "MiniGrid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement - preset Bins", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
PresetBins_withTransitionalMGs


#Output Bar charts 
ggsave(plot=PresetBins, filename="~/Desktop/SettlementDensity-perPhase.pdf")
ggsave(plot=PresetBins_withTransitionalMGs, filename="~/Desktop/SettlementDensity-perPhase-TransitionalMGs.pdf")

##Get rid of pre-electrified areas
local.binned <- subset(local.binned, (Demographics...Projected.household.count != 'NA'))

PresetBins = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=Phase)) +
  scale_fill_manual(values = c('#08519c',
                               '#3182bd',
                               '#6baed6',
                               '#bdd7e7',
                               '#eff3ff',
                               "#d7191c", "#abdda4", "#ffffbf"))+ 
  # labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        #         text=element_text(size=40),
        #       legend.text = element_text(size=30),
        #       axis.text = element_text(size=20),
        legend.position=c(1,1), #x=0=left, y=1=top
        legend.justification=c(1,1)) + #x=0
  labs(title = "Households per Settlement - preset Bins", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
PresetBins


PresetBins_withTransitionalMGs = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=Phase)) +
  scale_fill_manual(values = c('#08519c',
                               '#3182bd',
                               '#6baed6',
                               '#bdd7e7',
                               '#756bb1',
                               "#d7191c", "#abdda4", "#ffffbf"),
                    labels=c('1','2','3','4','5', "MiniGrid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        text=element_text(size=20, face="bold"),
      legend.text = element_text(size=20, face="plain"),
      axis.text = element_text(size=20),
      legend.position=c(1,1), #x=0=left, y=1=top
      legend.justification=c(1,1)) + #x=0
  labs(title = "Number of Settlements per Household Density", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
PresetBins_withTransitionalMGs

#Output Bar charts 
ggsave(plot=PresetBins, filename="~/Desktop/SettlementDensity-perPhase-NAs.pdf")
ggsave(plot=PresetBins_withTransitionalMGs, filename="~/Desktop/SettlementDensity-perPhase-TransitionalMGs-NAs.pdf")


