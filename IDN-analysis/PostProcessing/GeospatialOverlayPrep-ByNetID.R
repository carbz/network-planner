###PART 2 
### Characterize Project Area with cross-water jumps removed
### Edwin really pushed on doing something like this

require(plyr)

proposed_with_rollout <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/Modified Scenario/networks-proposed-with-rollout.csv')
#Get join fields to agree
proposed_with_rollout$X <- proposed_with_rollout$long_x
proposed_with_rollout$Y <- proposed_with_rollout$lat_x

modified_proposed_rollout <- proposed_with_rollout[c('FID',
                                                     'Name_x',
                                                     'X', 'Y',
                                                     'root_y','branch_y')]  


#Use NetID dataframe as index for grouping roots
netIDs <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/Shapefiling/OriginalRoots-NetID_Attirbutes.csv')
netIDs_lite <- netIDs[c('Name','X','Y','NetID','root')]

proposed_rollout_netIDs <- merge(netIDs_lite, modified_proposed_rollout, by = c('X','Y'), all.y = TRUE)

#Of roots that have NetIDs, propogate throughout all proposed-rollout path
roots_netIDs <- subset(proposed_rollout_netIDs, !(is.na(NetID)))

for (i in 1:length(roots_netIDs$root_y)){
  #Match known netIDs to all roots that it applies to
  proposed_rollout_netIDs$NetID[which(proposed_rollout_netIDs$root_y == roots_netIDs$root_y[i])] <-
    roots_netIDs$NetID[i]
}

#all unassigned roots, are unique subnetworks, likely isolated island so default to root as NetID

roots_only <- proposed_rollout_netIDs[which(as.character(proposed_rollout_netIDs$branch_y) == as.character(proposed_rollout_netIDs$root_y)),]

roots_absent_netIDs <- subset(roots_only, (is.na(NetID)))
proposed_rollout_netIDs$NetID <- as.character(proposed_rollout_netIDs$NetID)

for (j in 1:length(roots_absent_netIDs$root_y)){
  #Match known netIDs to all roots that it applies to
  proposed_rollout_netIDs$NetID[which(proposed_rollout_netIDs$root_y == roots_absent_netIDs$root_y[j])] <-
    as.character(roots_absent_netIDs$root_y[j])
}

#Calculate the number of HHs associated with each Network
proposed_with_rollout <- merge(proposed_with_rollout, proposed_rollout_netIDs, by =intersect(names(proposed_rollout_netIDs),names(proposed_with_rollout)))

NetworkSize <- ddply(proposed_with_rollout, .(NetID), summarize,
                     netHHS = sum(Demographi_1),
                     netSettlements = length(Name_x),
                     MVdistance.m = sum(dist_y),
                     PhaseComplete = max(Phase_HH),
                     PhaseStart = min(Phase_HH))

proposed_with_rollout$netHHs <- NA

for (k in 1:length(NetworkSize$NetID)){
  #Match known Network Cumulative HHs to all settlements that have the same netID
  proposed_with_rollout$netHHs[which(proposed_with_rollout$NetID == NetworkSize$NetID[k])] <-
    as.integer(NetworkSize$netHHS[k])
}

#Calculate the number of HHs associated with each branch
proposed_with_rollout$branchHHS <- NA
BranchSize <- ddply(proposed_with_rollout, .(root_y), summarize,
                     branchHHS = sum(Demographi_1))

for (l in 1:length(BranchSize$root_y)){
  #Match known Network Cumulative HHs to all settlements that have the same netID
  proposed_with_rollout$branchHHS[which(proposed_with_rollout$root_y == BranchSize$root_y[l])] <-
    as.integer(BranchSize$branchHHS[l])
}

#Output Results
roots_only <- proposed_with_rollout[which(as.character(proposed_with_rollout$branch_y) == as.character(proposed_with_rollout$root_y)),]

# write.csv(roots_only, 
#           '~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/Modified Scenario/ModifiedRootsOnly-wNetIDs.csv',
#           row.names = F)

proposed_with_rollout$Name <- proposed_with_rollout$Name_x
# write.csv(proposed_with_rollout, 
#           '~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/Modified Scenario/ModifiedAllSettlementsRanked-wNetIDs.csv',
#           row.names = F)

#Pull in Original MV per HH rating
original_classification <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Desas-CategorizedForStrategicGuidance-V20140409V2.csv')

#Merge with new NetID rankings 
all_settlements <- merge(original_classification,
                         proposed_with_rollout,
                         by = 'Name', all.x = T)




all_grid_settlements <- subset(all_settlements, Metric...System == 'grid')

all_grid_roots <- all_grid_settlements[which(as.character(all_grid_settlements$branch_y) == as.character(all_grid_settlements$root_y)),]


## Explore the data a bit
require(ggplot2)

library(RColorBrewer)


ggplot(data= all_grid_roots, aes(x=Pln_cabang, y=branchHHS/1000, fill=netHHs*.274/1000, group=netHHs)) + 
  geom_bar(stat='identity') +
  scale_fill_gradient2(#low="red", high="blue",
                      low = "#f4a582",high = "#4393c3", mid = '#762a83', 
                      limits=c(0,20),
                      midpoint = 10,
                      #guide = 'legend',
                      na.value = "#053061") + 
  labs(title = "Proposed Networks by Region", 
       x = "Geographic Region", 
       y="1,000 Households Served by Network", 
       fill = "Network Peak Load [MW]") +
   # ylim(0,50000) +
  
  ##FORMATTING FOR GRAPH
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(1,1), #x=1=RIGHT, y=1=top
        legend.justification=c(1,1)) #x=0

## Branch dataset too confusing, make a network dataset



NetworkSize <- ddply(all_settlements, .(NetID), summarize,
                     netHHS = sum(Demographi_1),
                     netSettlements = length(Name_x),
                     MVdistance.m = sum(dist_y),
                     PhaseComplete = max(Phase_HH),
                     PhaseStart = min(Phase_HH),
                     Pln_cabang = Pln_cabang[1])
NetworkSize <- NetworkSize[order(NetworkSize$netHHS, decreasing =T),]

ggplot(data=all_grid_roots, aes(x=branchHHS, fill=branchHHS<100)) + geom_histogram() + facet_wrap(~Pln_cabang)

ggplot(data=NetworkSize, aes(x=netHHS*.274/1000, fill = netHHS<2779)) + 
  geom_histogram() + 
  facet_wrap(~Pln_cabang, ncol = 4) +
  labs(title = "Proposed Networks by Region (143 total estimated)", 
       y="Number of Networks", 
       x="Peak Load of Network [MW]",
       fill = "Networks Smaller than 750 kWp") +
  theme(text=element_text(size=30),
      legend.text = element_text(size=20),
      axis.text = element_text(size=20),
      #axis.ticks=element_blank(), 
      #panel.grid=element_blank(),
      #panel.background=element_blank(),
      legend.position=c(1,1), #x=1=RIGHT, y=1=top
      legend.justification=c(1,1)) #x=0


##Mapping Network Sizes by region
ggplot(data= NetworkSize, aes(x=Pln_cabang, y=netHHS/1000, fill=netHHS*.274/1000, group=netHHS)) + 
  geom_bar(stat='identity') +
  scale_fill_gradient2(#low="red", high="blue",
    low = "#f4a582",high = "#4393c3", mid = '#762a83', 
    limits=c(0,20),
    midpoint = 7.5,
    #guide = 'legend',
    na.value = "#053061") + 
  labs(title = "Proposed Networks by Region", 
       x = "Geographic Region", 
       y="1,000 Households Served by Network", 
       fill = "Network Peak Load [MW]") +
  # ylim(0,50000) +
  
  ##FORMATTING FOR GRAPH
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(1,1), #x=1=RIGHT, y=1=top
        legend.justification=c(1,1)) #x=0

