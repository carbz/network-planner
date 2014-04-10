
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

source('~/github/network-planner/IDN-analysis/PostProcessing/interpret_commonfunctions.R')
source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')


# ##original scenario set for IDN
low_demand_path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/SensitivityAnalysis-480kWhDemand/"
low_demand_directory_names <- c("116-Ternate",
                                #2-5: Malukus
                                "123-Tual_TualBarat",
                                "122-Tual_Tual",
                                "121-Tual_Saumlaki",
                                "115-Ambon",
                                "114-Tual_PulauDoboi",
                                #7-10: NTT
                                "120-Sumba",
                                "119-FloresTimur",
                                "118-FloresBarat",
                                "117-Kupang")


# orig_path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/"
# orig_directory_names <- c("65-FloresBarat-MV5",
#                           "64-FloresTimor-MV5",
#                           "63-SumbaArea-MV5",
#                           "59-KupanArea-MV5",
#                           "62-TernateArea-MV5",
#                           "61-TualArea-MV5",
#                           "60-AmbonArea-MV5")
# 
##Develop input data for grid lenghts
path_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/"

directory_names <- c("98-Ternate-1km",
                     #Maluku 2
                     "97-Ambon-1kmBuffer",
                     "96-Taul-PulauDoboi-1km",
                     "103-TualSaumlaki-1km",
                     "106-TualBarat-1km",
                     "105-Tual-1km",
                     #NTT 7
                     "99-Kupang-1kmBuffer",
                     "100-FloresBarat-1km",
                     "101-FloresTimur-1km",
                     "102-Sumba-1km")
# path_name <- low_demand_path_name
# directory_names <- low_demand_directory_names

farsighted_all_metrics <- as.data.frame(NULL)
farsighted_all_metrics_lite <- as.data.frame(NULL)

i=1
farsighted_all_metrics <- as.data.frame(NULL)
for (i in 1:length(directory_names)){
  print(i)
  
  directory_name <- paste0(path_name, directory_names[i])
  scenario_prefix <- str_sub(directory_names[i], end = str_locate(directory_names[i], "-")[1])
  
  #directory_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/564-MalukuUtara-900HHD/"
  #directory_name <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/HybridBuffer/108-Tual-TualBarat-Hybrid/" 
  
  # #Import metrics local for each island areas in the analysis
  # #metrics local is the key output file of each Network Planner scenario capturing nodal level information
  local <- read.csv(paste0(directory_name,"/metrics-local.csv"), skip=1, stringsAsFactors=F)
  local$settlement_id <- rownames(local)
  
  #Naichen's process of converting csv to shapefile and back again abbreviates variable names
  #here I redefine the field of full_populaton in case it was lost 
  if ("Full_popul" %in% names(local)){
    local$Full_population <- local$Full_popul 
  }
  #check to see if older typo variable version is considered 
  if ("System..mini.grid....Generatation.installation.cost" %in% names(local)){
    local$System..mini.grid....Generation.installation.cost <- local$System..mini.grid....Generatation.installation.cost
  }
  if(!("System..mini.grid....Energy.storage.demand.per.year" %in% names(local))){
   local <- mutate(local,
                   System..mini.grid....Energy.storage.demand.per.year = System..mini.grid....Energy.storage.costs.per.year/
                     System..mini.grid....Energy.storage.cost.per.kwh)
  }
  #check to see if older spelling of Energy Storage is in play
  if ("System..mini.grid....Energy.storage.costs.per.year" %in% names(local)){
    local$System..mini.grid....Energy.storage.cost.per.year <- local$System..mini.grid....Energy.storage.costs.per.year
  }
    
  
proj4 <- read.csv(paste0(directory_name,"/metrics-local.csv"), nrows=1, header = FALSE)[1]
#   
#   # Import proposed networks for interpeting new network, this is the NP minimum spanning tree
#   #Prabhas' function imports both shapefile line  types in one go and merges them, useful...
  grid_lines <- load.polylines(directory_name)
#   
  # #Import Metrics Gloabl stuff too, 
  global <- load.global(read.csv(paste0(directory_name,"/metrics-global.csv"),stringsAsFactors=F))
  

  # #DESA POLYGONS
polygon <- LoadIDNPolygons(i)
  #Define more useful population catgories for project area polygons
  polygon <- popbins(polygon)
  
  ### ~~~~~~~~~~~~~~DATA LOADED!~~~~~~~~~~~~~~~~~~~~~~~####
  
  ## ~ Plotting Maps
  #There are some useful maps we commonly generate.  Let's try to automate and streamline that here. 
  #all bells and whistles.  This is a comprehensive plot of information for which we can subtract/add more information in the future.
    
  #Explicitly define the plot regions of interest based on NP outputs and BPS Polygon data
  big_picture_plot <- comprehensive_plot(polygon, grid_lines, local) + blank_theme() 
  #Develop map with Google Background for better reference
  proposed_GE_background <- google_earth_plot(grid_lines, local)
  #Sample Plots
  big_picture_plot
  proposed_GE_background
  
  ## ~ Output Maps to directory 
  #Aspect Ratio: height to width
  aspect_ratio <- (max(local$Y)-min(local$Y))/(max(local$X)-min(local$X))
  width <- 1050 #desired pixel width of image outputs

  #My favorite plot
  png(filename=paste0(directory_name,"/Output-Overview-Map-20140226.png"), width = width, height=width*aspect_ratio)
  plot(big_picture_plot)
  dev.off()
  
  ##My favorite plot
  png(filename=paste0(directory_name,"/Output-Overview-Map-GEbackground.png"), width = width, height=width*aspect_ratio)
  plot(proposed_GE_background)
  dev.off()
 
  # Summarize NP Output Data
  ##Here, we interpret basic consequence of the suggested network and try to express some useful metrics.

  #Summarize outputs by technology type (ie Off-Grid, Mini-Grid and Grid systems)
  summary <- summarize_metrics_local_MV5(local)

  local_agg <- summary
  #Determine Existing Grid stastics 
  existing_length <- polyline.length.within(local, directory_name)
  existing_pop <- sum(local$Full_population)
  existing_houses <- sum((local$Full_population-local$Old_pop)/local$Ho_size)
  
  #Grid Summary
grid<-grid.summary.corrected.existing(summary, global, existing_length, existing_houses)
mg <- mini.grid.summary.MV5(local_agg)
  og <- off.grid.summary(local_agg)
  
  options("scipen"=100, "digits"=2)
  
  all_systems_summary <- rbind(grid, mg, og) 
#  WriteXLS("all_systems_summary","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/SensitivityAnalysis-480kWhDemand/Tual-Combined/Tual-MetricsLocal-MVMax5-SingleSheetSummary0221.xls")
# 
#   WriteXLS("all_systems_summary", str_c(directory_name,
#                                         "/",
#                                         scenario_prefix,
#                                         "MetricsLocal-MVMax5-SingleSheetSummary-VMrequest-0221.xls"))

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



###Bins by Equal HHs Successful Attempt - Works!
#add new column "HHcumsum" stores the cummulative count of HHolds
local.binned$Demand..household....Target.household.count.HHcumsum <- 
  cumsum(local.binned$Demand..household....Target.household.count)
#add new column "HHBin" and assign bins of 10 parts
bin.count <- 10  #defining number of bins desired
local.binned$Demand..household....Target.household.count.HHbin <- 
  cut(local.binned$Demand..household....Target.household.count.HHcumsum, 
      breaks = seq(0, sum(local.binned$Demand..household....Target.household.count, na.rm=T), 
                   by=sum(local.binned$Demand..household....Target.household.count, na.rm=T)/10))

EqualHHCounts = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demand..household....Target.household.count.HHbin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement - Equal Households per Bin", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
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
  geom_bar(data = local.binned, aes(x=Demand..household....Target.household.count.HHbin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement - Equal Households per Bin", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-BinsWithEqualHH.tiff"))
plot(EqualHHCounts)
dev.off()

PresetBins = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demand..household....Target.household.count.predefinedbin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement - Preset Bins", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement-presetBins.tiff"))
plot(PresetBins)
dev.off()

HHoldBins_EqualSettlements = ggplot() + 
  geom_bar(data = local.binned, aes(x=Demand..household....Target.household.count.SettlementBin, 
                                    fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement - Equal Settlements per Bin", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")

#Output Bar charts 
tiff(filename=paste0(directory_name,"/HouseHolds_per_settlement.tiff"))
plot(HHoldBins_EqualSettlements)
dev.off()


  

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
  percent_houses_connected_at_start <- 1-sum(local$Old_pop/local$Ho_size)/(sum(local$Full_population/local$Ho_size))
  houses_connected_at_start <- (sum(local$Full_population/local$Ho_size) - sum(local$Old_pop/local$Ho_size))
  total_houses <- sum(local$Full_population/local$Ho_size)
  
  new_grid_connections <- max(farsighted_grid$CumulativeHousesConnected.qty)
  
  #Track cummulative electrification ratio 
  farsighted_grid <- mutate(farsighted_grid, 
                            PercentElectrification = (houses_connected_at_start + CumulativeHousesConnected.qty)/total_houses,
                            PercentHousesElectrified.2010Census = percent_houses_connected_at_start,
                            HousesConnected.2010Census = houses_connected_at_start,
                            TotalHouses.2010Census = total_houses)
  
  #Establish some Castalia-specific Metrics 
  farsighted_grid <- mutate(farsighted_grid, 
                            MVLinePerConnection = dist/Demand..household....Target.household.count,
                            TransformerCostPerConnection = System..grid....Transformer.cost/Demand..household....Target.household.count,
                            PercentOfNewGridConnections = CumulativeHousesConnected.qty/new_grid_connections)
  
  #That lets us develop Phase bins
  farsighted_grid$Phase <- NA
  total_phases <- 5
  phase_increment_house <- sum(farsighted_grid$Demand..household....Target.household.count)
  
  for (j in 1:total_phases){
    
    lower_cutoff <- (j-1)/total_phases*phase_increment_house
    upper_cutoff <- j/total_phases*phase_increment_house
    
    farsighted_grid$Phase[which((farsighted_grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                  (farsighted_grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
    
  }
  
  #Ensure Phase is considered a factor
  farsighted_grid$Phase <- as.factor(farsighted_grid$Phase)
  
  #Reverse factor order of phase so color corresponds better 
  farsighted_grid$PhasePlot = factor(farsighted_grid$Phase)
  #farsighted_grid$PhasePlot = factor(farsighted_grid$PhasePlot, levels = rev(farsighted_grid$PhasePlot))
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
#     
#   # Binning Categories
#   #It can be useful to sort the NP results of settlement data by bins.  Ex, phases for construction, technology by settlement size, cost buckets, etc. 
#   
#It can be useful to sort the NP results of settlement data by bins.  Ex, phases for construction, technology by settlement size, cost buckets, etc. 

# #Output 'lite' version of metrics local
shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(farsighted_grid))]
grid_settlements_ranked <- merge(local, farsighted_grid, by = shared_column_names, all.x=F, all.y=T)

all_settlements_ranked <- merge(local, farsighted_grid, by = shared_column_names, all.x=T, all.y=T)

all_settlements_ranked$Phase <- as.character(all_settlements_ranked$Phase)
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'mini-grid')] <- 'MiniGrid Systems'
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'off-grid')] <- 'OffGrid Systems'
all_settlements_ranked$Phase[which(all_settlements_ranked$Metric...System == 'unelectrified')] <- 'Pre-Electrified'

all_settlements_ranked$Phase <- as.factor(all_settlements_ranked$Phase)

# #Output a new shapefile with all the attirbute data of interest
#remove multiple nodes on line segments
metrics_local_with_sequence <- (farsighted_grid[which(!(duplicated(farsighted_grid$id))),])
proposed_with_rollout <- merge(proposed, metrics_local_with_sequence, by.x = "FID", by.y = "id")
writeLinesShape(proposed_with_rollout, paste0("~/Desktop/ProposedGrid-FullDemand/",
                                              scenario_prefix,                
                                              "networks-proposed-with-3PhaseRollout-LowDemand.shp"))
farsighted_all_metrics <- rbind(farsighted_all_metrics_lite, all_settlements_ranked)
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
                                       
write.csv(farsighted_all_metrics_lite, paste0(directory_name,
          "/Demand-AllNodes-LiteOutputs.csv"),
          row.names=F)
}


##Output csv of it all with ranking and origin metrics local stuff
write.csv(farsighted_grid, paste0(directory_name,
                                  "/",
                                  scenario_prefix,
                                  "metrics-local-grid-only-rollout_sequence.csv"), row.names=F) 
##Ouput more comprehensive spreadsheet
standalone_systems <- subset(local, ((Metric...System == "mini-grid") |
                                       Metric...System == "off-grid"))

WriteXLS(c("grid_settlements_ranked", "standalone_systems"), 
         paste0(directory_name,"/",scenario_prefix,"ForCastalia-metrics-local-rollout-sequenced.xls"))

#Now pass through everything, grid and non grid alike into single csv
shared_column_names <- colnames(local)[which(colnames(local) %in% colnames(grid_settlements_ranked))]
farsighted_all_combined <- merge(local, grid_settlements_ranked, by = shared_column_names, all.x=T, all.y=T)
write.csv(farsighted_all_combined, paste0(directory_name,
                                          "/",
                                          scenario_prefix,
                                          "metrics-local-rollout_sequence.csv"), row.names=F)


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



#Phase Bins already developed in the rollout sections, so let's aggregate by that
#Now we can summarize key metrics by phase
phase_summary <- ddply(grid_settlements_ranked, .(Phase), summarise, 
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
                       sum_of_Population = sum(Pop, na.rm=T), 
                       sum_of_Target_Households = sum(Demand..household....Target.household.count, na.rm=T), 
                       sum_of_Demand...Projected.nodal.demand.per.year = sum(Demand...Projected.nodal.demand.per.year, na.rm=T),
                       sum_of_installed_MV_network.m = sum(dist),
                       electrification_achieved = max(PercentElectrification)
)
write.csv(phase_summary, paste0(directory_name,
                                "/",
                                scenario_prefix,
                                "PhaseSummaryofAnticipatedGridRollout.csv"), row.names=F)

#Develop Phase summaries on a per household level to come up with "average" metrics
phase_summary_perHH <- ddply(grid_settlements_ranked, .(Phase), summarise,
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

## Comparing component costs of grid phases
farsighted_some <- subset(grid_settlements_ranked, select=c("Phase", 
                                                   "System..grid....Transformer.cost", 
                                                   "System..grid....Installation.cost", 
                                                   "System..grid....Low.voltage.line.equipment.cost", 
                                                   "System..grid....Electricity.cost.per.year",
                                                   "dist",
                                                   "Demand..household....Target.household.count"))
farsighted_some$MV.Wire.Costs <- farsighted_some$dist * 30

farsighted_some_tall <- melt(farsighted_some, id.vars=c("Phase"),
                             measure.vars=c("System..grid....Installation.cost", 
                                            "System..grid....Low.voltage.line.equipment.cost", 
                                            #"System..grid....Electricity.cost.per.year",
                                            "System..grid....Transformer.cost",
                                            "MV.Wire.Costs"
                             ))

# clean up names
names(farsighted_some_tall) <- c("Phase", "Type", "Cost") 
# clean up values
levels(farsighted_some_tall$Type) <- str_replace_all(str_replace_all(levels(farsighted_some_tall$Type), "System..grid....", ""), "[.]", " ")  

# PLOT!
phase_costs <- ggplot(farsighted_some_tall, aes(x=Phase, y=Cost/1E6, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Capital Costs Per Phase", x = "Phase", y="Total Phase Cost (USD Millions)", 
       fill = "Cost Category") +
  scale_fill_brewer(type="seq", palette="Blues")


#Output Bar charts 
tiff(filename=paste0(directory_name,"/TotalCost-Summary-Phased.tiff"))
plot(phase_costs)
dev.off()


average_HH_per_Phase <- sum(grid_settlements_ranked$Demand..household....Target.household.count)/length(unique(farsighted_some$Phase))

phase_costs_perHH <- ggplot(farsighted_some_tall, aes(x=Phase, y=Cost/average_HH_per_Phase, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Capital Costs Per Phase Per Household", x = "Phase", y="Average Cost per Household (USD)", 
       fill = "Cost Category")+
  scale_fill_brewer(type="seq", palette="Blues")



#Output Bar charts 
tiff(filename=paste0(directory_name,"/TotalCost-Summary-Phased-PerHH.tiff"))
plot(phase_costs_perHH)
dev.off()




## Comparistng component costs of grid phases and standalone options 
standalone_settlements <- subset(all_settlements_ranked, ((Phase == 'MiniGrid Systems')))

minigrid_components <- subset(standalone_settlements, select=c("Phase", 
                                                   "System..mini.grid....Generation.installation.cost", 
                                                   #"System..mini.grid....Generatation.installation.cost",
                                                   "System..mini.grid....Generation.system.cost", 
                                                   "System..mini.grid....Low.voltage.line.equipment.cost", 
                                                   "Distribution...Low.voltage.line.initial.cost",
                                                   "Demand..household....Target.household.count"))

names(minigrid_components)[2] <- 'System..mini.grid....Generatation.installation.cost'

minigrid_components_tall <- melt(minigrid_components, id.vars=c("Phase"),
                             measure.vars=c("Distribution...Low.voltage.line.initial.cost",
                                            'System..mini.grid....Low.voltage.line.equipment.cost',
                                            "System..mini.grid....Generation.system.cost",                                         
                               "System..mini.grid....Generatation.installation.cost"
                                            #"System..mini.grid....Generation.installation.cost", 
                                            
                             ))

# clean up names
names(minigrid_components_tall) <- c("Phase", "Type", "Cost") 
# clean up values
levels(minigrid_components_tall$Type) <- str_replace_all(str_replace_all(levels(minigrid_components_tall$Type), "System..grid....", ""), "[.]", " ")  

mg_phase_costs <- ggplot(minigrid_components_tall, aes(x=Phase, y=Cost, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Capital Costs Per Phase", x = "Phase", y="Total Phase Cost (USD)", 
       fill = "Cost Category") +
  scale_fill_brewer(type="seq", palette="Reds")

mg_houses = sum(standalone_settlements$Demand..household....Target.household.count)
minigrid_components_tall$CostPerHH <- minigrid_components_tall$Cost/mg_houses

mg_phase_costs_perHH <- ggplot(test2, aes(x=Phase, y=CostPerHH, fill=Type)) + 
  geom_bar(stat='identity') +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "MiniGrid Costs", y="Total Capital Cost per HH (USD)", 
       fill = "Cost Category") +
  scale_fill_brewer(type="seq", palette="Reds")

#Output MiniGrid Cost breakdwon charts 
png(filename=paste0(directory_name,"/MultiDemand-MiniGrid-Costs-PerHH.png"))
plot(mg_phase_costs_perHH)
dev.off()



metrics_local_with_sequence <- (farsighted_grid[which(!(duplicated(farsighted_grid$id))),])
proposed_with_rollout <- merge(proposed, metrics_local_with_sequence, by.x = "FID", by.y = "id")
writeLinesShape(proposed_with_rollout, paste0(directory_name,
                                              "/",
                                              scenario_prefix,                
                                              "networks-proposed-with-rollout-5Phase.shp"))




}

# 
# #Evaluating 375m buffer process to subset settlements already grid connected
# #in Maluku Utara
# #how much does this agree with Desa level stastics?
# 
# buffered_points_EA <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/83-Ternate-EA_pop_Buffered/demographics.csv")
# BPS_pooints <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/84-Ternate-JC_orig_pops/demographics.csv")
# 
# buffered_points_EA <- buffered_pooints_EA[c("Name","pop")]
# colnames(buffered_points_EA)[2] <- "pop_outside_375m_only"
# 
# merged_datasets <- merge(BPS_pooints, buffered_points_EA, by="Name", all.x=T)
# 
# desa_comparison <- ddply(merged_datasets, summarise)
