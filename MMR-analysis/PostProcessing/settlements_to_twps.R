
full_demand_local <- read.csv('~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/TransitionalNetworks/metrics-ranked-with-transitonals-20140513.csv')


#Coerce points to spatial dataframe
coordinates(full_demand_local) = ~X+Y

## Village Tracts are incomplete
# MMR_polygon_tracts <- readShapePoly(
#   "~/Dropbox/Myanmar_GIS/Village_Tracts/all_village_tracts.shp")

# #States are too coarse
# MMR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")


MMR_polygon_twps <- readShapePoly(
  "~/Dropbox/Myanmar_GIS/Admin_Boundaries/5_adm3_townships1_250k_mimu/adm3_townships1_250k_mimu.shp")


#Subset States on interest to match Shaky's joined subset
# States <- c("Chin", "Magway")
# local_all <- subset(local_all, State %in% States) #Unreliable because ST attribute is not consistent


InMMR <- over(full_demand_local,MMR_polygon_twps) 
InMMR <- cbind(full_demand_local, InMMR)

##Messy way to deaggregate households by technology 
InMMR$GridHHs <- NA
InMMR$GridHHs[which(InMMR$Metric...System=='grid')] <- InMMR$Demographics...Projected.household.count[which(InMMR$Metric...System=='grid')]
InMMR$MiniGridHHs <- NA
InMMR$MiniGridHHs[which(InMMR$Metric...System=='mini-grid')] <- InMMR$Demographics...Projected.household.count[which(InMMR$Metric...System=='mini-grid')]
InMMR$OffGridHHs <- NA
InMMR$OffGridHHs[which(InMMR$Metric...System=='off-grid')] <- InMMR$Demographics...Projected.household.count[which(InMMR$Metric...System=='off-grid')]
#How many HHs in each phase
InMMR$Phase1HH <- NA
InMMR$Phase1HH[which(InMMR$phase_transitionals=='1')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='1')]
InMMR$Phase2HH <- NA
InMMR$Phase2HH[which(InMMR$phase_transitionals=='2')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='2')]
InMMR$Phase3HH <- NA
InMMR$Phase3HH[which(InMMR$phase_transitionals=='3')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='3')]
InMMR$Phase4HH <- NA
InMMR$Phase4HH[which(InMMR$phase_transitionals=='4')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='4')]
InMMR$Phase5HH <- NA
InMMR$Phase5HH[which(InMMR$phase_transitionals=='5')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='5')]
InMMR$PhaseMGHH <- NA
InMMR$PhaseMGHH[which(InMMR$phase_transitionals=='MiniGrid Systems')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='MiniGrid Systems')]
InMMR$PhaseOGHH <- NA
InMMR$PhaseOGHH[which(InMMR$phase_transitionals=='OffGrid Systems')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='OffGrid Systems')]
InMMR$PhaseTransHH <- NA
InMMR$PhaseTransHH[which(InMMR$phase_transitionals=='Transitional Systems')] <- InMMR$Demographics...Projected.household.count[which(InMMR$phase_transitionals=='Transitional Systems')]
### yuck 

#Aggregate by township or TS_PCODE
INMMR_twp <- ddply(InMMR, .(TS_PCODE), summarize,
                   gridHHs = sum(GridHHs, na.rm=T),
                   minigridHHs = sum(MiniGridHHs, na.rm=T),
                   offgridHHs = sum(OffGridHHs, na.rm=T),
                   totalHHs = sum(Demographics...Projected.household.count, na.rm=T),
                   
                   Phase1HH = sum(Phase1HH, na.rm=T),
                   Phase2HH = sum(Phase2HH, na.rm=T),
                   Phase3HH = sum(Phase3HH, na.rm=T),
                   Phase4HH = sum(Phase4HH, na.rm=T),
                   Phase5HH = sum(Phase5HH, na.rm=t),
                   PhaseMGHH = sum(PhaseMGHH, na.rm=t),
                   PhaseOGHH = sum(PhaseOGHH, na.rm=T),
                   PhaseTransHH =sum(PhaseTransHH, na.rm=T)
                   )
INMMR_twp <- mutate(INMMR_twp, PercentTransitional=PhaseTransHH/totalHHs)
                   
#Append stuff to our township polygons
twps_included <- unique(InMMR$TS_PCODE)
MMR_polygon_twps$has_points <- MMR_polygon_twps$TS_PCODE %in% twps_included

MMR_polygon_twps <- merge(MMR_polygon_twps, INMMR_twp, by='TS_PCODE')

#Output it 
writePolyShape(MMR_polygon_twps, "~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/master_merged/TransitionalNetworks/townships1_250k_mimu-Classified.shp")

ggplot() + 
  geom_polygon(data = MMR_polygon_twps, aes(x=long,y=lat, group=group)) + 
#                                             fill=get(MMR_polygon_twps$has_points))) +
#   geom_polygon(data = MMR_polygon_twps, aes_string(x='long',y='lat', group='group', 
#                                             fill='has_points')))
    geom_point(data=full_demand_local, aes(x = X, y = Y, colour = Metric...System)) + #, shape = phase_transitionals))
  coord_equal()