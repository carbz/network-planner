setwd('~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/')

# ### LIBERIA ##
# LIB_local <- read.csv('~/Downloads/273/metrics-local.csv', skip=1, stringsAsFactors=F)
# LIB_proposed <- readShapeLines('~/Downloads/273/networks-proposed.shp')
# proj4 <- read.csv('~/Downloads/273/metrics-local.csv',  nrows=1, header = FALSE)
# #*******************************************************#
# farsighted_grid_LIB <-standard_rollout(LIB_local,LIB_proposed,proj4)
# farsighted_grid_LIB_noMonrovia <- subset(farsighted_grid_LIB, Name != 'Monrovia (Settlement)')
# 
# ggplot(data=farsighted_grid_LIB_noMonrovia, aes(x=PercentOfNewGridConnections, y=CumulativeNetworkExtent.m/CumulativeHousesConnected.qty)) +
#   geom_line()
#   
# ## Indonesia 
# IDN_local <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/metrics-local-all.csv', 
#                       stringsAsFactors=F)
# 
# proj4 <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2014/HybridBuffer/107-Tual-Saumlaki-Hybrid/metrics-local.csv',
#                   nrows=1, header = FALSE)
# IDN_proposed <- readShapeLines('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/March-2014/CombineProposedNetworks/networks-proposed-with-rollout.shp')
# IDN_proposed_modified <- readShapeLines('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/March-2014/Shapefiles/FilesForJonathan_via_EA/WaterJumpsRemoved_EA/IDN-PropGrWRollout-WaterJumpsRemoved-EA-Final.shp')
# 
# IDN_farsighted_grid <- standard_rollout(IDN_local, IDN_proposed, proj4)
# IDN_farsighted_grid_modified <- standard_rollout(IDN_local, IDN_proposed_modified, proj4)
# 
# ##Bring in MMR
# path_name <-"~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/"
# MMR_farsighted <- read.csv(paste0(path_name,'metrics-local-farsighted-20140507.csv'), stringsAsFactors=F)
# 
# #Combine them all
# IDN_farsighted_grid$country <- 'IDN'
# IDN_farsighted_grid_modified$country <- 'IDN-modified'
# farsighted_grid_LIB$country <- 'LIB'
# farsighted_grid_LIB_noMonrovia$country <- 'LIB-noMonrovia'
# MMR_farsighted$country <- 'MMR'
# MMR_farsighted['Settlement.id'] <- NULL
# MMR_farsighted['Name'] <- NULL
# MMR_farsighted['piece'] <- NULL
# MMR_farsighted['group'] <- NULL
# 
# 
# IDN_farsighted_grid_modified <- common_values_post_sequence(IDN_farsighted_grid_modified)
# IDN_farsighted_grid <- common_values_post_sequence(IDN_farsighted_grid)
# farsighted_grid_LIB <- common_values_post_sequence(farsighted_grid_LIB)
# farsighted_grid_LIB_noMonrovia <- common_values_post_sequence(farsighted_grid_LIB_noMonrovia)
# MMR_farsighted <- common_values_post_sequence(MMR_farsighted)


ALL_farsighted <- rbind.fill(IDN_farsighted_grid, 
                             IDN_farsighted_grid_modified, 
                             farsighted_grid_LIB,
                             farsighted_grid_LIB_noMonrovia,
                             MMR_farsighted)

# write.csv(ALL_farsighted,
#           '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/InternationalComparisonOfRolloutsV2.csv', 
#           row.names=F)
ALL_farsighted <- read.csv('InternationalComparisonOfRolloutsV2.csv')


ALL_farsighted_binned <- ddply(ALL_farsighted, .(FivePercentHHBins, country), summarize,
                               MVperHH = sum(dist, na.rm=T)/sum(Demand..household....Target.household.count, na.rm=T),
                               CumulativeNetworkExtent.m = sum(dist, na.rm=T),
                               CumulativeHousesConnected.qty = max(Demand..household....Target.household.count, na.rm=T))
  
ALL_farsighted_binned_Halves <- ddply(ALL_farsighted, .(HalfPercentHHBins, country), summarize,
                               MVperHH = sum(dist, na.rm=T)/sum(Demand..household....Target.household.count, na.rm=T),
                               CumulativeNetworkExtent.m = sum(dist, na.rm=T),
                               CumulativeHousesConnected.qty = max(Demand..household....Target.household.count, na.rm=T))



HalfPercent <- ggplot(data=ALL_farsighted_binned_Halves, aes(x=HalfPercentHHBins, y=MVperHH, colour = country)) + 
  geom_line(size=1) +
  labs(title = "Cumulative Average of MV Line", 
       x = "Household Bin [0.5% equal increments]", 
       y="MV Line per Household [m/HH]", 
       colour = "Scenarios") +
  uglify_tl()
FivePercent <- ggplot(data=ALL_farsighted_binned, aes(x=FivePercentHHBins, y=MVperHH, colour = country)) + 
  geom_line(size=1) +
  labs(title = "Cumulative Average of MV Line", 
       x = "Household Bin [5% equal increments]", 
       y="MV Line per Household [m/HH]", 
       colour = "Scenarios") +
  uglify_tl()
multiplot(HalfPercent, FivePercent, cols=2)

moving.average.plot <- function(ALL_farsighted, rollingmean=MV.line.per.kwh.rollingmean, title = 'Moving Average of MV Line'){
window25 <-  ggplot(data=ALL_farsighted, 
         aes(x=PercentOfNewGridConnections, y=MV.line.per.kwh.rollingmean25, color = country)) + 
  geom_line(size=0.5) +
  labs(title = 'Moving Average window = 25', 
       x = "Percent Houses Connected", 
       y="MV Line [m/HH]", 
       colour = "Scenarios")+ 
  #scale_colour_manual(values=custom_colors2) +
  uglify_tl()
}

window25 <- moving.average.plot(ALL_farsighted, rollingmean=MV.line.per.kwh.rollingmean25, '25')
window50 <- moving.average.plot(ALL_farsighted, MV.line.per.kwh.rollingmean50, '50')
window100 <- moving.average.plot(ALL_farsighted, MV.line.per.kwh.rollingmean, '100')
window150 <- moving.average.plot(ALL_farsighted, MV.line.per.kwh.rollingmean150, '150')
window200 <- moving.average.plot(ALL_farsighted, MV.line.per.kwh.rollingmean200, '200')
window1000 <- moving.average.plot(ALL_farsighted, rollingmean=MV.line.per.kwh.rollingmean1000, '1000')


multiplot(window25, window50,
          window100, window150, 
          window200, window1000, cols=2)


no_LIB_df<-subset(ALL_farsighted_1binned,country!='LIB')
no_LIB_df<-subset(no_LIB_df,country!='IDN')

ggplot(data=no_LIB_df, 
       aes(x=OnePercentHHBins, y=MVperHH, color = country)) + 
  geom_line(size=0.5) +
  labs(title = "1% Bins of MV Line", 
       x = "Percent Houses Connected", 
       y="MV Line per Household [m/HH]", 
       colour = "Scenarios") +
  #scale_colour_manual(values=custom_colors2) +
  uglify_tl()

read.csv('/Volumes/Archived Projects/Liberia_Geospatial_Analysis/Liberia_Modeling/2013-03/NPOutputs/273-JC working')


write.csv(ALL_farsighted, 'Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/InternationalComparisonOfRollouts.csv', row.names=F)


common_values_post_sequence <- function(farsighted_grid, total_phases=20){
  
  #####*************************************#########
  ######Define some typical Useful Values ###########
  #####*************************************#########
  
  farsighted_grid <- mutate(farsighted_grid,
                CumulativeNetworkExtent.m = cumsum(dist),
                CumulativeHousesConnected.qty = cumsum(Demand..household....Target.household.count))
    
  ##Phasing, Rollout and Costs
  #Order the suggested grid path by optimal sequence
  farsighted_grid$seq_fs <- farsighted_grid$far.sighted.sequence#shapefile chops longer names
  farsighted_grid <- farsighted_grid[order(farsighted_grid$far.sighted.sequence),]
  
  #Develop cummulative sum of network length metric
  farsighted_grid <- mutate(farsighted_grid, 
                            CumulativeNetworkExtent.m = cumsum(dist),
                            CumulativeHousesConnected.qty = cumsum(Demand..household....Target.household.count))
  
  #Scalar Values of region before expansion efforts began
  percent_houses_connected_at_start <- 0
  houses_connected_at_start <- 0
  total_houses <- sum(farsighted_grid$Demand..household....Target.household.count, na.rm=T)
  new_grid_connections <- max(farsighted_grid$CumulativeHousesConnected.qty)
  
  #Establish some Castalia-specific Metrics 
  farsighted_grid <- mutate(farsighted_grid, 
                            MVLinePerConnection = dist/Demand..household....Target.household.count,
                            TransformerCostPerConnection = System..grid....Transformer.cost/Demand..household....Target.household.count,
                            PercentOfNewGridConnections = CumulativeHousesConnected.qty/new_grid_connections)
  
  #That lets us develop Phase bins
  farsighted_grid$Phase_HH <- NA
  phase_increment_house <- sum(farsighted_grid$Demand..household....Target.household.count)
  
  for (j in 1:total_phases){
    
    lower_cutoff <- (j-1)/total_phases*phase_increment_house
    upper_cutoff <- j/total_phases*phase_increment_house
    
    farsighted_grid$Phase_HH[which((farsighted_grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                     (farsighted_grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
    
  }
  
  farsighted_grid$Phase_MV <- NA
  phase_increment_grid <- sum(farsighted_grid$dist)
  
  for (j in 1:total_phases){
    
    lower_cutoff <- (j-1)/total_phases*phase_increment_grid
    upper_cutoff <- j/total_phases*phase_increment_grid
    
    farsighted_grid$Phase_MV[which((farsighted_grid$CumulativeNetworkExtent.m >= lower_cutoff) &
                                     (farsighted_grid$CumulativeNetworkExtent.m <= upper_cutoff))] <- j
  }
  
  farsighted_grid$OnePercentHHBins <- NA
  for (j in 1:100){
    
    lower_cutoff <- (j-1)/100*phase_increment_house
    upper_cutoff <- j/100*phase_increment_house
    
    farsighted_grid$OnePercentHHBins[which((farsighted_grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                     (farsighted_grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
    
  }
  farsighted_grid$HalfPercentHHBins <- NA
  for (j in 1:200){
    
    lower_cutoff <- (j-1)/200*phase_increment_house
    upper_cutoff <- j/200*phase_increment_house
    
    farsighted_grid$HalfPercentHHBins[which((farsighted_grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                             (farsighted_grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
    
  }
  farsighted_grid$FivePercentHHBins <- NA
  for (j in 1:20){
    
    lower_cutoff <- (j-1)/20*phase_increment_house
    upper_cutoff <- j/20*phase_increment_house
    
    farsighted_grid$FivePercentHHBins[which((farsighted_grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                              (farsighted_grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
    
  }
  
  
  
    #####*************************************#########
  farsighted_grid$MV.line.per.kwh.rollingmean150 <- (rollmean(farsighted_grid$dist, 101, na.pad=T)/
                                                    rollmean(farsighted_grid$Demographics...Projected.household.count, 101, na.pad=T))
  #Jason asks 'Carlos' suggested that to validate a moving ave window we simply test different sizes (maybe 25, 50, 100, 150, 200? -'
  farsighted_grid$MV.line.per.kwh.rollingmean25 <- (rollmean(farsighted_grid$dist, 25, na.pad=T)/
                                                    rollmean(farsighted_grid$Demographics...Projected.household.count, 25, na.pad=T))
  farsighted_grid$MV.line.per.kwh.rollingmean50 <- (rollmean(farsighted_grid$dist, 51, na.pad=T)/
                                                       rollmean(farsighted_grid$Demographics...Projected.household.count, 51, na.pad=T))
  farsighted_grid$MV.line.per.kwh.rollingmean150 <- (rollmean(farsighted_grid$dist, 151, na.pad=T)/
                                                       rollmean(farsighted_grid$Demographics...Projected.household.count, 151, na.pad=T))
  farsighted_grid$MV.line.per.kwh.rollingmean200 <- (rollmean(farsighted_grid$dist, 201, na.pad=T)/
                                                       rollmean(farsighted_grid$Demographics...Projected.household.count, 201, na.pad=T))
  farsighted_grid$MV.line.per.kwh.rollingmean1000 <- (rollmean(farsighted_grid$dist, 1000, na.pad=T)/
                                                        rollmean(farsighted_grid$Demographics...Projected.household.count, 1000, na.pad=T))
  
  
  
  #####*************************************#########
  return(farsighted_grid)
  
}       

standard_rollout <- function(local,proposed,proj4){
  local$Settlement.id <- row.names(local)
  ####Do the rollout thing on it
  greedy_grid <- prioritized.grid.greedy(local,proposed,proj4)
  ##***************************
  #Explicitly define greedy grid output as a dataframe
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
  #*******************************************************#
  return(farsighted_grid)
}

uglify_tl <- function() {
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0.1,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) 
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
        