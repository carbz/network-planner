##Explore thresholds for Myanmar cutoff for transitional grid systems
path_name <-"~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/"
setwd(path_name)
all_settlements_ranked <- read.csv('metrics-local-farsighted-20140508.csv')
# all_settlements_ranked2 <- read.csv('metrics-local-farsighted-20140516.csv')


#bringing in libraries
require(plyr)
require(zoo)
require(ggplot2)
require(stringr)

#Clean up dirty dataset
all_settlements_ranked$Metric...System <- all_settlements_ranked$Metric...System.x
all_settlements_ranked$Demand..household....Target.household.count <- all_settlements_ranked$Demand..household....Target.household.count.x
all_settlements_ranked$Name <- all_settlements_ranked$Name.x
all_settlements_ranked$Demand...Projected.nodal.demand.per.year <- all_settlements_ranked$Demand...Projected.nodal.demand.per.year.x
all_settlements_ranked$System..grid....Transformer.cost <- all_settlements_ranked$System..grid....Transformer.cost.x
all_settlements_ranked$Demographics...Projected.household.count <- all_settlements_ranked$Demographics...Projected.household.count.x
# all_settlements_ranked$XYID <- paste0(str_sub(as.character(all_settlements_ranked$X*100000),end=7L),
#                                       str_sub(as.character(all_settlements_ranked$Y*100000),end=7L))

#Add in Grid-Connections
grid_connections <- read.csv('~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/pre-electrified-villages-for-all-states-based-on-1mHV1kmMVBuffer.csv')
grid_connections$XYID <- paste0(str_sub(as.character(grid_connections$Longitude*100000),end=7L),str_sub(as.character(grid_connections$Latitude*100000),end=7L))
grid_connections$Metric...System <- 'Grid-Connected'
all_settlements_ranked <- rbind.fill(all_settlements_ranked, grid_connections)


#Removal of Major Cities
major_cities <- read.csv('~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/cities_and_towns_within_10km_Existing_HV_MVLines.csv')
major_cities$XYID <- paste0(str_sub(as.character(major_cities$Latitude*100000),end=7L),str_sub(as.character(major_cities$Longitude*100000),end=7L))
all_settlements_ranked$Demand..household....Target.household.count.Original <- all_settlements_ranked$Demand..household....Target.household.count
all_settlements_ranked[which(all_settlements_ranked$XYID %in% major_cities$XYID),'Demand..household....Target.household.count'] <- 0

all_settlements_ranked$Metric...System <- as.character(all_settlements_ranked$Metric...System)
all_settlements_ranked[which(all_settlements_ranked$XYID %in% major_cities$XYID),'Metric...System'] <- 'Major City'
all_settlements_ranked[which(all_settlements_ranked$XYID %in% major_cities$XYID),'Phase_HH'] <- 'Major City'
all_settlements_ranked$Metric...System <- as.factor(all_settlements_ranked$Metric...System)

#Add in a rolling mean
all_settlements_ranked$MV.line.per.kwh.rollingmean <- rollmean(all_settlements_ranked$dist/all_settlements_ranked$Demand..household....Target.household.count.x, 
                                                               101, 
                                                               na.pad=T)


all_settlements_ranked$OverThreshold <- ((all_settlements_ranked$CumulativeNetworkExtent.m/all_settlements_ranked$CumulativeHousesConnected.qty) > 4.25)
all_settlements_ranked$Scenario <- as.character(all_settlements_ranked$Scenario)

plot_cum <- 
  ggplot(data= all_settlements_ranked, 
         aes(x=seq_fs, 
             y=CumulativeNetworkExtent.m/CumulativeHousesConnected.qty,
             colour = OverThreshold)) +
  geom_line(size=2) +
  labs(title = "Myanmar National Grid Rollout", 
       x = "Settlements Connected to Grid", 
       y="Running Average of MV Line [m/HH]", 
       colour = "Over 4.25m Threshold?") +
  scale_colour_manual(values=custom_colors) +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(1,0), #x=0=left, y=0=bottom
        legend.justification=c(1,0)) + #x=1=left, y=1=bottom+
  geom_hline(yintercept=c(4.25), linetype="dashed") 
#   geom_text(aes(0,c(6),label = c('> 6m/HH cutoff for Transitionals'), vjust = -1))
plot_cum


all_settlements_ranked$OverThreshold <- (all_settlements_ranked$MV.line.per.kwh.rollingmean >10.0)


custom_colors <- c('#E9C31E','#E28426','#DF6026','#AC2324','#D14889',
                   '#861949','#95257C','#29265F','#0A6597','#33B4DE',
                   '#236E38','#48A548','#8EBD40','#738077','#754C29')


plot_rolling_mean <- 
  ggplot(data= (all_settlements_ranked[which(!is.na(all_settlements_ranked$MV.line.per.kwh.rollingmean)),]), 
         aes(x=seq_fs, 
             y=MV.line.per.kwh.rollingmean, 
             colour = OverThreshold)) +
  geom_line(size=.5) +
  labs(title = "Myanmar National Grid Rollout", 
       x = "Settlements Connected to Grid", 
       y="Rolling Mean (window=100) of MV Line [m/HH]", 
       colour = 'Over 10m Threshold?')+
       #colour = "Scenarios") +
  #scale_colour_manual(values=custom_colors) +
  uglify_tl() +
  geom_hline(yintercept=c(10), linetype="dashed")


#Develop 10 Phases for more granularity
all_settlements_ranked$Phase_HH_10 <- NA
total_phases <- 10
phase_increment_house <- sum(all_settlements_ranked[which(all_settlements_ranked$Metric...System=='grid'),
                                                    'Demand..household....Target.household.count'])
for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_house
  upper_cutoff <- j/total_phases*phase_increment_house
  
  all_settlements_ranked$Phase_HH_10[which((all_settlements_ranked$CumulativeHousesConnected.qty >= lower_cutoff) &
                                   (all_settlements_ranked$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
  
}

#Develop 20 Phase bins for even more granularity
all_settlements_ranked$Phase_HH_20 <- NA
total_phases <- 20
phase_increment_house <- sum(all_settlements_ranked[which(all_settlements_ranked$Metric...System=='grid'),
                                                    'Demand..household....Target.household.count'])
for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_house
  upper_cutoff <- j/total_phases*phase_increment_house
  
  all_settlements_ranked$Phase_HH_20[which((all_settlements_ranked$CumulativeHousesConnected.qty >= lower_cutoff) &
                                             (all_settlements_ranked$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
  
}
settlements_bin_summary <- ddply(all_settlements_ranked, .(Phase_HH_20), summarize,
                                 Households = sum(Demand..household....Target.household.count, na.rm=T),
                                 MV_Distance.mperHH = sum(dist, na.rm=T)/sum(Demand..household....Target.household.count, na.rm=T),
                                 Cumulative_Households_Connected = max(CumulativeHousesConnected.qty, na.rm=T))


ggplot(data= (settlements_bin_summary[which(!is.na(settlements_bin_summary$Phase_HH_20)),]), 
       aes(x=Phase_HH_20, 
           y=MV_Distance.mperHH)) +
  geom_line(size=3) +
  labs(title = "Myanmar National Grid Rollout", 
       x = "Settlement Bin Connected to Grid [5% increments]", 
       y="Average MV Distance Needed Per Bin [m/HH]", 
       colour = 'Over 10m Threshold?')+
  #colour = "Scenarios") +
  #scale_colour_manual(values=custom_colors) +
  uglify_tl() +
  geom_hline(yintercept=c(10), linetype="dashed")


#Consider Off-Grid and MiniGrid Phases too 
all_settlements_ranked$Phase <- as.character(all_settlements_ranked$Phase_HH_10)
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'mini-grid')] <- 'MiniGrid Systems'
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'off-grid')] <- 'OffGrid Systems'
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'unelectrified')] <- 'Grid-Connected'

all_settlements_ranked$Phase_HH[which(all_settlements_ranked$Metric...System == 'mini-grid')] <- 'MiniGrid Systems'
all_settlements_ranked$Phase_HH[which(all_settlements_ranked$Metric...System == 'off-grid')] <- 'OffGrid Systems'
all_settlements_ranked$Phase_HH[which(all_settlements_ranked$Metric...System == 'unelectrified')] <- 'Grid-Connected'

#
all_settlements_ranked$phase_transitionals <- as.character(all_settlements_ranked$Phase_HH)
all_settlements_ranked$CumMVLinePerConnection <- all_settlements_ranked$CumulativeNetworkExtent.m/all_settlements_ranked$CumulativeHousesConnected.qty
all_settlements_ranked$phase_transitionals[which(all_settlements_ranked$Phase_HH == 5 )] <- 'Transitional Systems'

#avg MV for cutoff candidates
sum(all_settlements_ranked$dist[which(all_settlements_ranked$CumMVLinePerConnection > 6.0)])/
  sum(all_settlements_ranked$Demand..household....Target.household.count.Adjusted[which(all_settlements_ranked$CumMVLinePerConnection > 6.0)])

PercentHouseholds <- sum(all_settlements_ranked$Demand..household....Target.household.count.Adjusted[which(all_settlements_ranked$CumMVLinePerConnection > 4.25)])/
  sum(all_settlements_ranked$Demand..household....Target.household.count.Adjusted, na.rm=T)



#Let's pull out Grid settlements' costs for grid and standalone options too 
GridCosts <- ddply(all_settlements_ranked, .(Phase), summarise,
                   ## ***General Informaiton***
                   'Demand..household....Target.household.count' = sum(Demand..household....Target.household.count),
                   
                   ## ***Grid Component Costs***
                   'Total new MV Lines (kilometers)' = sum(dist)/1E3,
                   'Length of Network Installed per HH (meters)' = (sum(dist)/sum(Demand..household....Target.household.count)),
                   'Avg.System..grid....Medium.voltage.line.cost.per.meter' = weighted.mean(System..grid....Medium.voltage.line.cost.per.meter,
                                                                                            dist,
                                                                                            na.rm = TRUE),
                   
                   'Demand...Projected.nodal.demand.per.year' = sum(Demand...Projected.nodal.demand.per.year),                                    
                   'System..grid....Transformer.cost'=sum(System..grid....Transformer.cost), 
                   'System..grid....Installation.cost'=sum(System..grid....Installation.cost),
                   'System..grid....Low.voltage.line.equipment.cost'=sum(System..grid....Low.voltage.line.equipment.cost),
                   'System..grid....Electricity.cost.per.year'=sum(System..grid....Electricity.cost.per.year),
      
                   ## ***MiniGrid Component Costs***
                   
                   'System..mini.grid....Generation.installation.cost'= sum(System..mini.grid....Diesel.generator.installation.cost), 
                   #"System..mini.grid....Generatation.installation.cost,
                   'System..mini.grid....Generation.system.cost'=sum(System..mini.grid....Diesel.generator.cost), 
                   'System..mini.grid....Low.voltage.line.equipment.cost'=sum(System..mini.grid....Low.voltage.line.equipment.cost), 
                   'Distribution...Low.voltage.line.initial.cost'=sum(Distribution...Low.voltage.line.initial.cost),
                   'System..mini.grid....Diesel.Fuel.costs.per.year' = sum(System..mini.grid....Diesel.fuel.cost.per.year),
                   'System..mini.grid....System.recurring.cost.per.year.with.fuel'=sum(System..mini.grid....System.recurring.cost.per.year),
                   
                   ## ***OffGrid Component Costs***                  
                   'System..off.grid....Photovoltaic.panel.cost'=sum(System..off.grid....Photovoltaic.panel.cost),
                   'System..off.grid....Photovoltaic.battery.cost'=sum(System..off.grid....Photovoltaic.battery.cost),
                   'System..off.grid....Photovoltaic.balance.cost'=sum(System..off.grid....Photovoltaic.balance.cost),
                   'System..off.grid....Photovoltaic.balance.cost'=sum(System..off.grid....Photovoltaic.balance.cost),
                   'System..off.grid....System.recurring.cost.per.year'=sum(System..off.grid....System.recurring.cost.per.year)
)

GridCosts_perHH <- c(GridCosts[c(1:5)],
                     GridCosts[c(6:length(GridCosts))]/GridCosts$Demand..household....Target.household.count)

#write.csv(GridCosts_perHH, "TransitionalNetworks/NationalRollout-PerHHCosts-PerPhase.csv", row.names=F)


####Settlement Densities by Bin 

##Developing Bin Classifications
local.binned<-all_settlements_ranked
# local.binned <- all_settlements_ranked[c('Demographics...Projected.household.count',
#                                          'Metric...System', 
#                                          'Phase_HH',
#                                          'phase_transitionals')]
#Remove 0 populations, or NAs
local.binned$Demographics...Projected.household.count[which(local.binned$Demographics...Projected.household.count==0)] <- NA

###Bins by Equal HHs 
#Sort local dataframe by HHold size
local.binned <- local.binned[order(local.binned$Demand..household....Target.household.count.x),]
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
  #scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Grid-Connected")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) 
  #labs(title = "Households per Settlement - Equal Settlements per Bin", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
# #Output Bar charts 
# tiff(filename="TransitionalNetworks/HouseHolds_per_settlement-withNAs.tiff")
# plot(HHoldBins_EqualSettlements)
# dev.off()
HHoldBins_EqualSettlements

#for posterity's sake, output csv with other bin categories defiend 

##Assign bins to original dataset based on fixed predefined thresholds for households/settlement - works
local.binned$Demographics...Projected.household.count.predefinedbin <- 
  cut(local.binned$Demographics...Projected.household.count, 
      c(0, 11, 21, 51, 101, 250, 501, 1000, Inf),
      labels = paste('<', c(11, 21, 51, 101, 250, 501, 1000, Inf)))

PresetBins <- ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=Phase_HH)) +
#   scale_fill_manual(values = c('#08519c',
#                                '#3182bd',
#                                '#6baed6',
#                                '#bdd7e7',
#                                '#eff3ff',
#                                "#d7191c", "#abdda4", "#ffffbf"))+ 
#   # labels=c("Grid", "Mini Grid", "Off Grid", "Grid-Connected")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) 
#   labs(title = "Households per Settlement - preset Bins", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
PresetBins
# #Output Bar charts 
# tiff(filename="TransitionalNetworks/HouseHolds_per_settlement-presetbins-withNAs.tiff")
# plot(PresetBins)
# dev.off()

local.binned <- local.binned[order(local.binned$seq_fs),]

PresetBins_withTransitionalMGs = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=phase_transitionals)) +
#   scale_fill_manual(values = c('#08519c',
#                                '#3182bd',
#                                '#6baed6',
#                                '#bdd7e7',
#                                '#eff3ff',
#                                '#bdbdbd',#grey
#                                "#d7191c",#red 
#                                "#abdda4", #green
#                                '#756bb1'),
#                     labels=c('1','2','3','4','5', "Grid-Connected", "MiniGrid", "Off Grid",
#                             'Transitionals, Avg ~ 41m/HH')) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
#   labs(title = "Households per Settlement -w/  Transitional Systems", 
#        x = "Desnity Bin Classification [HH/Settlement]", 
#        y="Number of Settlements [qty]", 
#        color = "Electrification Tech.",
#        fill = 'Electirifcation Strategy') +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1))

PresetBins_withTransitionalMGs

##Output some of this working stuff
# write.csv(local.binned, 'TransitionalNetworks/metrics-ranked-with_transitionals.csv', row.names=F)

#Output Transitional Rankings
write.csv(local.binned[c('X','Y','Demographics...Projected.household.count',
                           'Metric...System', 
                           'Phase_HH',
                           'phase_transitionals')], 
          'TransitionalNetworks/metrics-ranked-with-transitonals-20140516.csv', 
          row.names=F)