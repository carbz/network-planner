#April 7, 2014
#Vijay wants to wrap up core messages from IDN work in a Geospatial Overlay

require(WriteXLS)
require(ggplot2)
require(plyr)

#metrics_local <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-metrics-local-lite.csv')
metrics_local <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-metrics-local-lite-V2.csv')
rollout <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-networks-proposed-with-rollout.csv')

#combining sequenced grid settlements and non-grid points all together
all_settlements <- merge(metrics_local, rollout, by = 'Name',all=T)

#settlements outside 1km buffer
all_settlements_1kmbuffer <- subset(all_settlements, Metric...System == 'grid')



#Order by Cabang then sequence
all_settlements <- all_settlements_1kmbuffer[with(all_settlements_1kmbuffer, order(Pln_cabang, seq_fs)),]

#Assign Scenario tag each settlement
all_settlements$scenario <- as.character(all_settlements$Pln_cabang)
replacements <- as.character(all_settlements$Ei_subarea[which(all_settlements$Pln_cabang == "Cabang Tual")])
all_settlements$scenario[which(all_settlements$Pln_cabang == "Cabang Tual")] <- replacements


#Recalculate Cumulative Averages
all_settlements$CumulativeHH <- NA
all_settlements$CumulativeMV <- NA
Cabangs <- unique(all_settlements$Pln_cabang)
EI_areas <- unique(all_settlements$Ei_subarea)
scenarios <- unique(all_settlements$scenario)

all_settlements$dist.N.19.11[which(is.na(all_settlements$dist))] <- 0 

for (j in 1:length(scenarios)){
  #define cumulative household
  all_settlements$CumulativeHH[which(all_settlements$scenario == scenarios[j])] <-
    cumsum(all_settlements$Demand..household....Target.household.count[which(all_settlements$scenario == scenarios[j])])
  #define cumulative MV installed
  all_settlements$CumulativeMV[which(all_settlements$scenario == scenarios[j])] <- 
    cumsum(all_settlements$dist.N.19.11[which(all_settlements$scenario == scenarios[j])])
  
}
all_settlements <- mutate(all_settlements, averageMVperHH =CumulativeMV/CumulativeHH,
                          MVperHH = dist.N.19.11/Demand..household....Target.household.count,
                          averageMVperHH_old = Cumulative/Cumulati_1)
#Improve Labeling
all_settlements$scenario <- as.character(all_settlements$scenario)
all_settlements$scenario[which(all_settlements$scenario == 'AMBON')] <- 'Ambon'
all_settlements$scenario[which(all_settlements$scenario == 'Area Kupan')] <- 'Kupang'
all_settlements$scenario[which(all_settlements$scenario == 'AreaSumba')] <- 'Sumba'
all_settlements$scenario[which(all_settlements$scenario == 'FloresBara')] <- 'Flores Barat'
all_settlements$scenario[which(all_settlements$scenario == 'FloresTimu')] <- 'Flores Timur'

#Write for XLS
WriteXLS("all_settlements","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-for-ElectrificationV20140408.xls")
## 

h = 12#c(12,15,18) # Y intercept Cutoff

plot_pln_cabangs <- 
  ggplot(data= all_settlements, 
         aes(x=PercentOfN.N.19.15, 
             y=averageMVperHH, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Per Household", 
       x = "Percent of New Households Connected [%]", 
       y="MV Line [m/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) + 
  geom_hline(yintercept=c(h), colour="#636363", linetype="dashed") #+
  #geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs

plot_pln_cabangs_cost <- 
  ggplot(data= all_settlements, 
         aes(x=PercentOfN.N.19.15, 
             y=averageMVperHH*30, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Cost Per Household", 
       x = "Percent of New Households Connected [%]", 
       y="MV Line Cost per Household [$USD/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) + 
  geom_hline(yintercept=c(h*30), colour="#636363", linetype="dashed") #+
#geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs_cost

plot_pln_cabangs_HH <- 
  ggplot(data= all_settlements, 
         aes(x=CumulativeHH, 
             y=averageMVperHH, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Per Household", 
       x = "Number of Households Connected [qty]", 
       y="MV Line [m/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(1,1), #x=0=left, y=1=top
        legend.justification=c(1,1)) + 
  geom_hline(yintercept=c(h), colour="#636363", linetype="dashed") #+
#geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs_HH

plot_pln_cabangs_HHcost <- 
  ggplot(data= all_settlements, 
         aes(x=CumulativeHH, 
             y=averageMVperHH*30, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Cost Per Household", 
       x = "Percent of New Households Connected [%]", 
       y="MV Line Cost per Household [$USD/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(1,1), #x=0=left, y=1=top
        legend.justification=c(1,1)) + 
  geom_hline(yintercept=c(h*30), colour="#636363", linetype="dashed") 
#geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs_HHcost



#Determine the geospatial overlay categorization

for (j in 1:length(scenarios)){
  #Seqence spots more than 
  
  all_settlements[which(all_settlements$scenario == scenarios[j]),] <- 
    mutate(all_settlements[which(all_settlements$scenario == scenarios[j]),],
           
           
           
           )
  
  
  for (j in 1:length(scenarios)){
    #define cumulative household
    all_settlements$CumulativeHH[
      which(all_settlements$scenario == scenarios[j])[
        which]
      ]
  }
  
  
  all_settlements$seq_fs[which(all_settlements$averageMVperHH_old >12 == scenarios[j])] <-
    cumsum(all_settlements$Demand..household....Target.household.count[which(all_settlements$scenario == scenarios[j])])
  #define cumulative MV installed
  all_settlements$CumulativeMV[which(all_settlements$scenario == scenarios[j])] <- 
    cumsum(all_settlements$dist.N.19.11[which(all_settlements$scenario == scenarios[j])])
  
}

#Plot total MV installed per region
plot_pln_cabangs_MVline <- 
  ggplot(data= all_settlements, 
         aes(x=CumulativeHH, 
             y=CumulativeMV/1000, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Sum of MV Line and Households per Region", 
       x = "Cumulative Households Connected [qty]", 
       y="MV Line [km]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(1,1)) + 
  geom_abline(intercept = 0, slope = 12/1000, colour="#636363", linetype="dashed") + 
  #geom_hline(yintercept=c(h), colour="#636363", linetype="dashed") #+
  geom_text(aes(y=400,x=30000,label = '> 12 m/HH', angle = 20, vjust = -1))
plot_pln_cabangs_MVline


#Load categorized 
grid_ranked <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-for-ElectrificationV20140409.csv')

#combining sequenced grid settlements and non-grid points all together
nongrid_settlements <- merge(metrics_local, rollout, by = 'Name',all=T)
nongrid_settlements <- subset(nongrid_settlements, Metric...System != 'grid')

#Combine nongrid and on-grid all together
all_settlements <- rbind.fill(grid_ranked, nongrid_settlements)

NonGridFields <- c("Geospatial.Overlay...15m.Threshold",
                   "Geospatial.Overlay...10m.Threshold",         
                   "Geospatial.Overlay...12m.Threshold",
                   "Geospatial.Overlay...18m.Threshold")   

#Define phases as a factor to better mesh in the NAs when applicable 
all_settlements$Phase <- as.factor(all_settlements$Phase)

new_factors <- c(levels(all_settlements$Geospatial.Overlay...15m.Threshold),levels(all_settlements$Phase)) 

levels(all_settlements$Geospatial.Overlay...15m.Threshold) <- new_factors
levels(all_settlements$Geospatial.Overlay...10m.Threshold) <- new_factors
levels(all_settlements$Geospatial.Overlay...12m.Threshold) <- new_factors
levels(all_settlements$Geospatial.Overlay...18m.Threshold) <- new_factors

#Replace NAs with original phase categorization 
replacements<-  all_settlements[which(is.na(all_settlements$Geospatial.Overlay...15m.Threshold)),'Phase']        
all_settlements[which(is.na(all_settlements$Geospatial.Overlay...15m.Threshold)),NonGridFields] <- replacements

#Output Singular Desa Field
all_settlements$Desa <- substr(all_settlements$Name,1,10)

all_settlements <- all_settlements[with(all_settlements,order(seq_fs,Phase.C.5)),]
allsettlements.desas <- ddply(all_settlements, c("Desa"), subset, Name==(Name)[1])

WriteXLS("allsettlements.desas","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Desas-CategorizedForStrategicGuidance-V20140409V2.xls")
write.csv(allsettlements.desas,"~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Desas-CategorizedForStrategicGuidance-V20140409V2.csv")

WriteXLS("all_settlements","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-CategorizedForStrategicGuidance-V20140409.xls")
write.csv(all_settlements,"~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-CategorizedForStrategicGuidance-V20140409.csv")

###PART 2 
### Characterize Project Area with cross-water jumps removed

source('~/github/network-planner/MMR-analysis/interpret_commonfunctions.R')
source('~/github/network-planner/Prioritized/NetworkPlanner_SystemRollout_Greedy.R')
source('~/github/network-planner/Prioritized/Custom_Rollout_Functions.R')


# Rollout 
#If all that stuff works, let's suggest a sequence in which to roll out the construction of grid-nodes.  This has been pre-developed and we're reapplying here 
#Importing proposed grid by itself, no existing lines as well

proposed <- readShapeLines("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/Modified Scenario/metrics-proposed.shp")
proposed$FID <- row.names(proposed) # ensure FID is unqiue

#Establish unique IDs for metrics local file
local <- read.csv("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/Modified Scenario/ALL-metrics-local.csv", skip=1) 
local$Settlement.id <- rownames(local) #use generic row names for unique ID of each unique settlement point

proj4 <- read.csv("~/Dropbox/Myanmar_GIS/Modeling/Tests/680/metrics-local.csv", nrows=1, header = FALSE)[1]

#Use output of priortized.grid function as input to far-sighted optimized rollout algorithim 
#takes a shapefile (network) and csv (nodal descriptions and weights) 
#and suggests a sequential, phased roll-out of the system based on a greedy, one step ahead view
#***RUNTIME ~05:30***********
greedy_grid <- prioritized.grid.greedy(local,proposed, proj4)
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
percent_houses_connected_at_start <- 0
houses_connected_at_start <- 0
total_houses <- sum(local$Demand..household....Target.household.count, na.rm=T)
new_grid_connections <- max(farsighted_grid$CumulativeHousesConnected.qty)

#Establish some Castalia-specific Metrics 
farsighted_grid <- mutate(farsighted_grid, 
                          MVLinePerConnection = dist/Demand..household....Target.household.count,
                          TransformerCostPerConnection = System..grid....Transformer.cost/Demand..household....Target.household.count,
                          PercentOfNewGridConnections = CumulativeHousesConnected.qty/new_grid_connections)

#That lets us develop Phase bins
farsighted_grid$Phase_HH <- NA
total_phases <- 5
phase_increment_house <- sum(farsighted_grid$Demand..household....Target.household.count)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_house
  upper_cutoff <- j/total_phases*phase_increment_house
  
  farsighted_grid$Phase_HH[which((farsighted_grid$CumulativeHousesConnected.qty >= lower_cutoff) &
                                   (farsighted_grid$CumulativeHousesConnected.qty <= upper_cutoff))] <- j
  
}

farsighted_grid$Phase_MV <- NA
total_phases <- 5
phase_increment_grid <- sum(farsighted_grid$dist)

for (j in 1:total_phases){
  
  lower_cutoff <- (j-1)/total_phases*phase_increment_grid
  upper_cutoff <- j/total_phases*phase_increment_grid
  
  farsighted_grid$Phase_MV[which((farsighted_grid$CumulativeNetworkExtent.m >= lower_cutoff) &
                                   (farsighted_grid$CumulativeNetworkExtent.m <= upper_cutoff))] <- j
}

##Output The Good stuff
path_name <-"~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/Modified Scenario/"
setwd(path_name)

metrics_local_with_sequence <- (farsighted_grid[which(!(duplicated(farsighted_grid$id))),])

proposed_with_rollout <- merge(proposed, metrics_local_with_sequence, by.x = "FID", by.y = "id")
writeLinesShape(proposed_with_rollout, "networks-proposed-with-rollout.shp")

  
  

