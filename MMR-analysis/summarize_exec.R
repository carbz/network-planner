#no new lines here##
require(gdata)
require(plyr)
require(stringr)
require(data.table)
require(ggplot2)
require(grid)
require(maptools)
require(stats)
require(ggmap)
require(WriteXLS)

source('~/github/network-planner/MMR-analysis/interpret_commonfunctions.R')
source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')


##Develop input data for grid lenghts
path_name <- "~/Dropbox/Myanmar_GIS/Modeling/Tests/"

directory_names <- c("625-Chin-1000HHDem-PopGr-NoDmdGr-LV15MV22-13c-SN10/",
                     "626-Kayin-1000HHDem-PopGr-NoDmdGr-LV15MV22-13c-SN10//")

i=2



for (i in 1:length(directory_names)){
  print(i)
  
  directory_name <- paste0(path_name, directory_names[i])
  scenario_prefix <- (directory_names[i])
  
  # #Import metrics local for each island areas in the analysis
  # #metrics local is the key output file of each Network Planner scenario capturing nodal level information
  local <- read.csv(paste0(directory_name,"/metrics-local.csv"), skip=1, stringsAsFactors=F)
  local$settlement_id <- rownames(local)
  
  
  proj4 <- read.csv(paste0(directory_name,"/metrics-local.csv"), nrows=1, header = FALSE)
  
  # Import proposed networks for interpeting new network, this is the NP minimum spanning tree
  #Prabhas' function imports both shapefile line  types in one go and merges them, useful...
  grid_lines <- load.polylines(directory_name)
  
  # #Import Metrics Gloabl stuff too, 
  global <- load.global(read.csv(paste0(directory_name,"/metrics-global.csv"),stringsAsFactors=F))
  
#   #Polygon data too, why not!
#   
#   MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/MMR_adm/MMR_adm3.shp")
#   #     #now let's make it more ggplottable and keep any attribute data 
#   MMR_polygon@data$id <- rownames(MMR_polygon@data)
#   MMR_polygon <- merge(MMR_polygon@data, fortify(MMR_polygon), by = 'id')
#   MMR_polygon<- malukuutara_polygon
#   
#   MMR_polygon$State <- MMR_polygon$NAME_1
#   
#   #Define more useful population catgories for project area polygons
#   polygon <- popbins(polygon)
#   
#   ### ~~~~~~~~~~~~~~DATA LOADED!~~~~~~~~~~~~~~~~~~~~~~~####
#   
#   # Plotting Maps
#   #There are some useful maps we commonly generate.  Let's try to automate and streamline that here. 
#   #all bells and whistles.  This is a comprehensive plot of information for which we can subtract/add more information in the future.
#   #Thanks @prabhasp!
#     
#  #Explicitly define the plot regions of interest based on NP outputs and BPS Polygon data
#  big_picture_plot <- comprehensive_plot(polygon, grid_lines, local) + blank_theme() 
#  big_picture_plot
  

  #Aspect Ratio: height to width
  aspect_ratio <- (max(local$Y)-min(local$Y))/(max(local$X)-min(local$X))
  width <- 1050 #desired pixel width of image outputs
  
# 
#   ##My favorite plot
#    png(filename=paste0(directory_name,"/Output-Overview-Map-20140226.png"), width = width, height=width*aspect_ratio)
#    plot(big_picture_plot)
#    dev.off()
# 
  #Develop map with Google Background for better reference
  proposed_GE_background <- google_earth_plot(grid_lines, local)
  
#   #Sample Plots
#   big_picture_plot
  proposed_GE_background
#   ##My favorite plot
#   png(filename=paste0(directory_name,"/Output-Overview-Map-GEbackground.png"), width = width, height=width*aspect_ratio)
#   plot(proposed_GE_background)
#   dev.off()
#  
  # Summarize NP Output Data
  ##Here, we interpret basic consequence of the suggested network and try to express some useful metrics.

  #Summarize outputs by technology type (ie Off-Grid, Mini-Grid and Grid systems)
  summary <- summarize_metrics_local_MV4(local)

  local_agg <- summary
  #Determine Existing Grid stastics 
  existing_length <- polyline.length.within(local, directory_name)
 
  #Grid Summary
grid <- grid.summary(local_agg, global)
  mg <- mini.grid.summary(local_agg)
  og <- off.grid.summary(local_agg)
  
  options("scipen"=100, "digits"=2)
  
  all_systems_summary <- rbind(grid, mg, og) 
  WriteXLS("all_systems_summary",paste0(directory_name,"/MetricsLocal-SingleSheetSummary.xls"))
  
  # Rollout 
  #If all that stuff works, let's suggest a sequence in which to roll out the construction of grid-nodes.  This has been pre-developed and we're reapplying here 
  #Importing proposed grid by itself, no existing lines as well
  proposed <- readShapeLines(paste0(directory_name,"/networks-proposed.shp")) #RUNTIME ~ 00:08 mins
  
  #Establish unique IDs for metrics local file
  local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point
  
  ## ensure FID is unqiue
  proposed$FID <- row.names(proposed)
  
  #Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
  #takes a shapefile (network) and csv (nodal descriptions and weights) 
  #and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
  #***RUNTIME ~08:00***********
  greedy_grid <- prioritized.grid.greedy(local,proposed,proj4)
  ##***************************
  
  #Explicitly define greedy grid output as a dataframe
  #Sometimes I need to explicitly call the fataframe for greedy.grid - arghhhh
  if (length(greedy_grid)==2){
    print("Houston, we have a problem with our dataframe")
    greedy_grid  <- as.data.frame(greedy_grid[1])
  }
  
  #Function to determine downstream summations for greedy grid
  greedy_grid_cumulatives <- downstream.sum.calculator(greedy_grid)
  
  #Far Sighted function to improve near-sighted greedy grid
  #* **********************
  farsighted_grid <- far_sighted_rollout(greedy_grid_cumulatives)
  #******************************
  
  ##Phasing, Rollout and Costs
  #Order the suggested grid path by optimal sequence
  farsighted_grid$seq_fs <- farsighted_grid$far.sighted.sequence#shapefile chops longer names
  farsighted_grid <- farsighted_grid[order(farsighted_grid$far.sighted.sequence),]
  
  #Develop cummulative sum of network length metric
  farsighted_grid <- mutate(farsighted_grid, 
                            CumulativeNetworkExtent.m = cumsum(dist),
                            CumulativeHousesConnected.qty = cumsum(Demand..household....Target.household.count))
  
  #Scalar Values of region before expansion efforts began
  total_houses <- sum(local$Full_population/local$Ho_size)
  
  new_grid_connections <- max(farsighted_grid$CumulativeHousesConnected.qty)
  
  #Establish some Castalia-specific Metrics 
  farsighted_grid <- mutate(farsighted_grid, 
                            MVLinePerConnection = dist/Demand..household....Target.household.count,
                            TransformerCostPerConnection = System..grid....Transformer.cost/Demand..household....Target.household.count,
                            PercentOfNewGridConnections = CumulativeHousesConnected.qty/new_grid_connections)
  
  #That lets us develop Phase bins
  farsighted_grid$Phase <- NA
  total_phases <- 5
  phase_increment_grid <- sum(farsighted_grid$dist)
  
  for (j in 1:total_phases){
    
    lower_cutoff <- (j-1)/total_phases*phase_increment_grid
    upper_cutoff <- j/total_phases*phase_increment_grid
    
    farsighted_grid$Phase[which((farsighted_grid$CumulativeNetworkExtent.m >= lower_cutoff) &
                                  (farsighted_grid$CumulativeNetworkExtent.m <= upper_cutoff))] <- j
    
  }
  
  #Ensure Phase is considered a factor
  farsighted_grid$Phase <- as.factor(farsighted_grid$Phase)
#   
#    #Now develop ranked plot
#   options("scipen"=100, "digits"=10) #ensuring lat/longs dont get chopped
#   rollout_plot <- ranked_plot(polygon, farsighted_grid, grid_lines) + blank_theme()
#   rollout_plot 
#   
#   #Output Map png
#   tiff(filename=paste0(directory_name,"/Rollout-Overview-Map.tiff"), width = width, height=width*aspect_ratio)
#   plot(rollout_plot)
#   dev.off()
    
  # Binning Categories
  #It can be useful to sort the NP results of settlement data by bins.  Ex, phases for construction, technology by settlement size, cost buckets, etc. 
  


###################
#~~~~~~~~~~~~~~~~
####################



# #Output 'lite' version of metrics local
shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted_grid))]
farsighted_all <- merge(local, farsighted_grid, by = shared_column_names, all.x=F, all.y=T)
farsighted_all_metrics <- merge(local, farsighted_grid, by = shared_column_names, all.x=T, all.y=T)

grid_settlements_ranked <- merge(local, farsighted_grid, by = shared_column_names, all.x=F, all.y=T)

all_settlements_ranked <- merge(local, farsighted_grid, by = shared_column_names, all.x=T, all.y=T)

all_settlements_ranked$Phase <- as.character(all_settlements_ranked$Phase)
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'mini-grid')] <- 'MiniGrid Systems'
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'off-grid')] <- 'OffGrid Systems'
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'unelectrified')] <- 'Pre-Electrified'

all_settlements_ranked$Phase <- as.factor(all_settlements_ranked$Phase)

##Output csv of it all with ranking and origin metrics local stuff
write.csv(farsighted_grid, paste0(directory_name,
                                  "/",
                                  scenario_prefix,
                                  "metrics-local-grid-only-rollout_sequence.csv"), row.names=F) 
##Ouput more comprehensive spreadsheet
standalone_systems <- subset(local, ((Metric...System == "mini-grid") |
                                       Metric...System == "off-grid"))
grid_settlements_ranked <- farsighted_all 

WriteXLS(c("grid_settlements_ranked", "standalone_systems"), 
         paste0(directory_name,"/",scenario_prefix,"ForCastalia-metrics-local-rollout-sequenced.xls"))


##Developing Bin Classifications
local.binned <- all_settlements_ranked[c('Demand..household....Target.household.count',
                        'Metric...System',
                        'Phase')]
#Remove 0 populations, or NAs
local.binned$Demand..household....Target.household.count[which(local.binned$Demand..household....Target.household.count==0)] <- NA

###Bins by Equal HHs 
#Sort local dataframe by HHold size
local.binned <- ddply(local.binned, "Demand..household....Target.household.count")
#add new column "HHcumsum" stores the cummulative count of HHolds
local.binned$Demographics...Projected.household.count.HHcumsum <- 
  cumsum(local.binned$Demand..household....Target.household.count)

###summarize number of settlements in bins sized by equal number of Settlements-works
#Determine HHsize/settlement breaks that split settlements into specified percentages
HHoldBinsEqualSettlementQty <- quantile(local.binned$Demand..household....Target.household.count, 
                                        probs = c(0, .2, .4, .6, .8, 1), na.rm=T)#break settlements into quantiles @ 20, 40, 60, 80 & 100%
#Determine Settlement Bins                        
local.binned$Demographics...Projected.household.count.SettlementBin <- 
  cut(local.binned$Demand..household....Target.household.count, 
      breaks = HHoldBinsEqualSettlementQty, 
      include.lowest = TRUE,
      labels = paste('<', HHoldBinsEqualSettlementQty[2:length(HHoldBinsEqualSettlementQty)]))

HHoldBins_EqualSettlements = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.SettlementBin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix, "- Households per Settlement - Equal Settlements per Bin"), 
       x = "Desnity Bin of Households/Settlement", 
       y="Number of Settlements", 
       color = "Electrification Tech.")

#for posterity's sake, output csv with other bin categories defiend 

##Assign bins to original dataset based on fixed predefined thresholds for households/settlement - works
local.binned$Demographics...Projected.household.count.predefinedbin <- 
  cut(local.binned$Demand..household....Target.household.count, 
      c(0, 11, 21, 51, 101, 250, 501, 1000, Inf),
      labels = paste('<', c(11, 21, 51, 101, 250, 501, 1000, Inf)))

PresetBins = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix,"- Households per Settlement - preset Bins"), 
       x = "Desnity Bin of Households/Settlement", 
       y="Number of Settlements", 
       color = "Electrification Tech.")
PresetBins

PresetBinsbyPhase = ggplot() + 
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
  labs(title = paste(scenario_prefix,"- Households per Settlement - preset Bins"), 
       x = "Desnity Bin of Households/Settlement", 
       y="Number of Settlements", 
       color = "Electrification Tech.")
PresetBinsbyPhase

#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-presetBins-withNAs.tiff"))
plot(PresetBins)
dev.off()

#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-presetBins-withNAs.tiff"))
plot(PresetBinsbyPhase)
dev.off()

###Bins by Equal HHs Successful Attempt - Works!
#add new column "HHcumsum" stores the cummulative count of HHolds
local.binned$Demographics...Projected.household.count.HHcumsum <- 
  cumsum(local.binned$Demand..household....Target.household.count)
#add new column "HHBin" and assign bins of 10 parts
bin.count <- 10  #defining number of bins desired
local.binned$Demographics...Projected.household.count.HHbin <- 
  cut(local.binned$Demographics...Projected.household.count.HHcumsum, 
      breaks = seq(0, sum(local.binned$Demand..household....Target.household.count, na.rm=T), 
                   by=sum(local.binned$Demand..household....Target.household.count, na.rm=T)/10))

EqualHHCounts = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.HHbin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix,"- Households per Settlement - Equal Households per Bin"), 
       x = "Desnity Bin of Households/Settlement", 
       y="Number of Settlements", 
       color = "Electrification Tech.")
#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-BinsWithEqualHH-withNAs.tiff"))
plot(EqualHHCounts)
dev.off()

##summarize number of HHolds (sum) and number of settlements (AKA observations/nobs) per bin as was originally done in Excel for scenario 230
##still needs work if want cross-comparison... 
##Specify the Bin categories of ClustHouseholds(Edwin did in original 230 analysis) 
local.binned$Clusthhold.bin.OriginalMethod <- 
  cut(local.binned$Demand..household....Target.household.count, c(1, 10, 20, 50, 100, 250, 500, 1000, Inf))

write.csv(local.binned, paste0(directory_name,
                               "/",
                               scenario_prefix,
                               "metrics-local-HHold-bins.csv"), row.names=F) 


#Plot these guys and remove NA values AKA pre-electrified areas 
local.binned <- subset(local.binned, (Demand..household....Target.household.count != 'NA'))
##Regeneratre Bar Charts now
EqualHHCounts = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.HHbin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix,"- Households per Settlement - Equal Households per Bin"),
       x = "Desnity Bin of Households/Settlement",
       y="Number of Settlements",
       color = "Electrification Tech.")
#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-BinsWithEqualHH.tiff"))
plot(EqualHHCounts)
dev.off()

PresetBins = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.predefinedbin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix,"- Households per Settlement - Preset Bins"), 
       x = "Desnity Bin of Households/Settlement",
       y="Number of Settlements",
       color = "Electrification Tech.")
#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-presetBins.tiff"))
plot(PresetBins)
dev.off()

HHoldBins_EqualSettlements = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demographics...Projected.household.count.SettlementBin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix,"- Households per Settlement - Equal Settlements per Bin"), 
       x = "Desnity Bin of Households/Settlement", 
       y="Number of Settlements", 
       color = "Electrification Tech.")

#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement.tiff"))
plot(HHoldBins_EqualSettlements)
dev.off()

###################
#~~~~~~~~~~~~~~~~
####################

#Now pass through everything, grid and non grid alike into single csv
shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted_all))]
farsighted_all_combined <- merge(local, farsighted_all, by = shared_column_names, all.x=T, all.y=T)
write.csv(farsighted_all_combined, paste0(directory_name,
                                          "/",
                                          scenario_prefix,
                                          "metrics-local-rollout_sequence.csv"), row.names=F)

#Phase Bins already developed in the rollout sections, so let's aggregate by that
#Now we can summarize key metrics by phase
phase_summary <- ddply(farsighted_all, .(Phase), summarise, 
                       sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
                       sum_of_Demand...Projected.nodal.discounted.demand = sum(Demand...Projected.nodal.discounted.demand, na.rm=T),
                       sum_of_Demand..peak....Projected.peak.nodal.demand = sum(Demand..peak....Projected.peak.nodal.demand, na.rm=T),
                       sum_of_Demand..household....Projected.household.demand.per.year = sum(Demand..household....Projected.household.demand.per.year, na.rm=T),
                       sum_of_System..grid....Internal.system.initial.cost = sum(System..grid....Internal.system.initial.cost, na.rm=T),
                       sum_of_System..grid....Installation.cost = sum(System..grid....Installation.cost,na.rm=T),
                       sum_of_System..grid....Low.voltage.line.equipment.cost = sum(System..grid....Low.voltage.line.equipment.cost, na.rm=T),
                       sum_of_System..grid....Transformer.cost = sum(System..grid....Transformer.cost, na.rm=T),
                       sum_of_System..grid....Internal.system.recurring.cost.per.year = sum(System..grid....Internal.system.recurring.cost.per.year, na.rm=T),
                       sum_of_System..grid....Electricity.cost.per.year = sum(System..grid....Electricity.cost.per.year, na.rm=T),
                       sum_of_System..grid....Transformer.operations.and.maintenance.cost.per.year = sum(System..grid....Transformer.operations.and.maintenance.cost.per.year, na.rm=T),
                       sum_of_System..grid....Transformer.replacement.cost.per.year = sum(System..grid....Transformer.replacement.cost.per.year, na.rm=T),
                       sum_of_System..grid....Internal.system.nodal.discounted.cost = sum(System..grid....Internal.system.nodal.discounted.cost, na.rm=T),
                       avg_of_System..grid....Internal.system.nodal.levelized.cost = mean(System..grid....Internal.system.nodal.levelized.cost, na.rm=T),
                       sum_of_Population = sum(Demographics...Projected.population.count, na.rm=T), 
                       sum_of_Target_Households = sum(Demand..household....Target.household.count, na.rm=T), 
                       sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
                       sum_of_installed_MV_network.m = sum(dist),
                       new_grid_electrification_achieved = max(PercentOfNewGridConnections)
)
write.csv(phase_summary, paste0(directory_name,
                                "/",
                                scenario_prefix,
                                "PhaseSummaryofAnticipatedGridRollout.csv"), row.names=F)

#Develop Phase summaries on a per household level to come up with "average" metrics
phase_summary_perHH <- ddply(farsighted_all, .(Phase), summarise,
                             'Number of Households Connected (qty)' = sum(Demand..household....Target.household.count),
                             'Total Cost of Phase (USD Millions)' = (sum(System..grid....Internal.system.initial.cost) + sum(dist)*30)/1E6,
                             'Per HH Cost of Phase (USD)' = ((sum(System..grid....Internal.system.initial.cost) + sum(dist)*30) / 
                                                               sum(Demand..household....Target.household.count)),
                             'Total new MV Lines (kilometers)' = sum(dist)/1E3,
                             'Length of Network Installed per HH (meters)' = (sum(dist)/sum(Demand..household....Target.household.count)),
                             'Percent of Grid Connected Households Electrified' = max(PercentOfNewGridConnections)
)
write.csv(phase_summary_perHH, paste0(directory_name,
                                      "/",
                                      scenario_prefix,
                                      "PhaseSummaryofAnticipatedGridRollout-perHH-022614.csv"), row.names=F)

##Prabhas' tips on bar graphs
require(reshape2); require(stringr)


farsighted_some <- subset(farsighted_all, select=c("Phase", 
                                                   "System..grid....Transformer.cost", 
                                                   "System..grid....Installation.cost", 
                                                   "System..grid....Low.voltage.line.equipment.cost", 
                                                   "System..grid....Electricity.cost.per.year",
                                                   "dist",
                                                   "Demand..household....Target.household.count"))
farsighted_some$MV.Wire.Costs <- farsighted_some$dist * 30

farsighted_some_tall <- melt(farsighted_some, id.vars=c("Phase"),
                             measure.vars=c("System..grid....Low.voltage.line.equipment.cost",
                                            "System..grid....Installation.cost", 
                                             #"System..grid....Electricity.cost.per.year",
                                            "System..grid....Transformer.cost",
                                            "MV.Wire.Costs"                                          
                                                                                    
                             ))
 
# clean up names
names(farsighted_some_tall) <- c("Phase", "Type", "Cost") 

# clean up values
levels(farsighted_some_tall$Type) <- str_replace_all(str_replace_all(levels(farsighted_some_tall$Type), "System..grid....", ""), "[.]", " ")  

#Add per HH unit cost
farsighted_some_tall$CostsPerHH <- farsighted_some_tall$Cost/farsighted_some$Demand..household....Target.household.count

# PLOT!
phase_costs <- ggplot(farsighted_some_tall, aes(x=Phase, y=Cost/1E6, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix,"- Capital Costs Per Phase"), 
       x = "Phase", y="Total Phase Cost (USD Millions)", 
       fill = "Cost Category") +
  scale_fill_brewer(type="seq", palette="Blues")


#Output Bar charts 
tiff(filename=paste0(directory_name,"/TotalCost-Summary-Phased.tiff"))
plot(phase_costs)
dev.off()

houses_per_phase <- ddply(farsighted_all, .(Phase), summarise, houses = sum(Demand..household....Target.household.count, na.rm=T), grid = sum(dist, na.rm=T))[2]
#average_HH_per_Phase <- sum(farsighted_all$Demand..household....Target.household.count)/length(unique(farsighted_some$Phase))
farsighted_some_tall$CostsPerHH = farsighted_some_tall$Cost/farsighted_some_tall$Houses

phase_costs_perHH <- ggplot(farsighted_some_tall, aes(x=Phase, y=CostsPerHH, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = paste(scenario_prefix,"- Capital Costs Per Phase Per Household"), x = "Phase", y="Average Cost per Household (USD)", 
       fill = "Cost Category")+
  scale_fill_brewer(type="seq", palette="Blues")



#Output Bar charts 
tiff(filename=paste0(directory_name,"/TotalCost-Summary-Phased-PerHH.tiff"))
plot(phase_costs_perHH)
dev.off()


#What does the rollout look like on Terrain map 
rollout_plot_GE <- ranked_plot_GE(farsighted_all_combined) + blank_theme()
##My favorite plot
tiff(filename=paste0(directory_name,
                     "/",
                     scenario_prefix,                
                     "Output-Overview-Map-GE.tiff"), 
     width = width, height=width*aspect_ratio)
plot(rollout_plot_GE)
dev.off()

# #Output a new shapefile with all the attirbute data of interest
#remove multiple nodes on line segments
# metrics_local_with_sequence <- (farsighted_grid[which(!(duplicated(farsighted_grid$id))),])
# proposed_with_rollout <- merge(proposed, metrics_local_with_sequence, by.x = "FID", by.y = "id")
# writeLinesShape(proposed_with_rollout, paste0(directory_name,
#                                               "/",
#                                               scenario_prefix,                
#                                               "networks-proposed-with-rollout.shp"))


}

# # 
# # #Evaluating 375m buffer process to subset settlements already grid connected
# # #in Maluku Utara
# # #how much does this agree with Desa level stastics?
# # 
# # buffered_points_EA <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/83-Ternate-EA_pop_Buffered/demographics.csv")
# # BPS_pooints <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/84-Ternate-JC_orig_pops/demographics.csv")
# # 
# # buffered_points_EA <- buffered_pooints_EA[c("Name","pop")]
# # colnames(buffered_points_EA)[2] <- "pop_outside_375m_only"
# # 
# # merged_datasets <- merge(BPS_pooints, buffered_points_EA, by="Name", all.x=T)
# # 
# # desa_comparison <- ddply(merged_datasets, summarise)
