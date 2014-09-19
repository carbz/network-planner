#Jonathan's Directory 

#comprehensive composite dataset lives here
#path_name <-"~/Dropbox/Myanmar/Demographics/Rural and Urban Population/Final_Pop/All_States_Village_Urban_Pop_Points_Merged_NodalOveridedValue_with_MIMU_and_GAD_Combined.csv"
path_name <-"~/Dropbox/MMR-Training-docs/data/NEP-Data/NationalAggerate/National-Metric-local-09.17-full.csv"
path_name <-"~/Dropbox/MMR-Training-docs/data/NEP-Data/NationalAggerate/NatlMlLite.csv"


#read that csv in
MMR_pop <- read.csv(path_name)

#break it up by State

#No Good because there are 26 states with outliers
#states <- unique(MMR_pop$State)

#Inside 'Ayeyawady' State?
coordinates(MMR_pop) = ~Longitude+Latitude
coordinates(MMR_pop) = ~X+Y


MR_polygon <- readShapePoly("~/Dropbox/Myanmar_GIS/Admin_Boundaries/3_adm1_states_regions2_250k_mimu/adm1_states_regions2_250k_mimu.shp")

State_GIS <- over(MMR_pop,MMR_polygon)

MMR_pop <- read.csv(path_name)#revert back to dataframe
MMR_pop <- cbind(MMR_pop, 'State_GIS'=State_GIS[,2]) 


#Write csv's now
#directory_name <- "~/Dropbox/MMR-Training-docs/data/Demographics/All Population/"
directory_name <- "~/Dropbox/MMR-Training-docs/data/NEP Raw Data (Day 3)/"

write.csv(MMR_pop, paste0(directory_name,
                          'ALL-National-Metric-local-09182014.csv'),
          row.names=F)
states <- unique(MMR_pop$State_GIS)
  
for (i in 1:length(states)){
  output <- subset(MMR_pop, State_GIS == states[i])
#  write.csv(output, paste0(directory_name,
WriteXLS("output", paste0(directory_name,
                         #'State-level Outputs-full/',
                         '/State-level Outputs-lite/',
                           str_sub(states[i],1L,3L),
                           '-ml-lite.xlsx'))
#,
#  row.names=F)
}


##Repeat with data within the Buffer




WriteXLS
