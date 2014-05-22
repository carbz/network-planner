library(tools)
library(rgdal)
library(geosphere) # must be v1.3-8 or greater
library(stringr)


#Jonathan's Directory 
path_name <-"~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/500kWh_Scenarios/"

directory_names <-c(#'158-Mon+Tanintharyi+Kayin/',
                  #'160-Chin/',
                  '161-Nyapitaw/',
                  #'168-Ayewardy_All/',
                  #'169-Sagaing/',
                  #'170-Bago_All/',
                  #'171-Kachin/',
                  #'172-Kayah/',
                  #'707-Shan_All/',
                  '708-Mandalay/')
                  #'709-Rakhine/',
                  #'710-Yangon/',
                  #'711-Magway/')


#Pulling most 'common' variable from datasets for easier handling##
short_names <- c('Name',"X","Y", "Metric...System","State","Demographics...Projected.household.count",
                 "Demand..household....Target.household.count","Demand...Projected.nodal.demand.per.year",
                 "System..grid....Transformer.cost","Demographics...Population.count",
                 "Demographics...Projected.population.count","Scenario","Scenario_name")

### MERGE SCENARIOS TOGETHER ####
for (i in 1:length(directory_names)){
  print(i)   
  local <- read.csv(paste0(path_name,directory_names[i],'/metrics-local.csv'), skip=1) #RUNTIME ~ 00:28 mins
  scenario <- substr(directory_names[i],0,3)
  ##local$Scenario <- scenario
  #local_lite <- local[,c(short_names)]
  local_lite <- local
  
  #Merge all proposed shapefiles together
  proposed_i <- readShapeLines(paste0(path_name,directory_names[i],'/networks-proposed.shp'))
  
  # change their IDs so they don't conflict
  proposed_i <- spChFIDs(proposed_i, as.character(paste0(scenario,'.', proposed_i$FID)))
  proposed_i$FID <- row.names(proposed_i)
  
  # bind to previous dataset 'MVLineType' attribute
  if (i>1){
    all_lines <- rbind(proposed_i, all_lines) 
    
    #Merge
    shared_col_names <- intersect(names(local_lite),names(local_all_orig)) #c('Village_co', 'X','Y','Metric...System')
    local_all_orig <- merge(local_all_orig, local_lite, by=shared_col_names, all=T)
    ##New guys get latest scenario designation
    local_all_orig[which(is.na(local_all_orig$Scenario)),'Scenario'] <- scenario
  
    }else {
    all_lines <- proposed_i
  } 
} 

##Remove duplicates
local_all_orig$XYID <- paste0(str_sub(as.character(local_all_orig$X*100000),end=7L),str_sub(as.character(local_all_orig$Y*100000),end=7L))

local_all_orig <- local_all_orig[order(local_all_orig$Metric...System),]#get grids to the top
duplicates <- local_all_orig[which(duplicated(local_all_orig$XYID, 
                                              fromLast=FALSE)),]#minimize grid nodes being removed
uniques <- local_all_orig[which(!(duplicated(local_all_orig$XYID, 
                                             fromLast=FALSE))),]#minimize grid nodes being removed


##Output the Right data 
write.csv(uniques, 
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/metrics-local-2States-500kWhDemand.csv', row.names=F)
write.csv(uniques[names(uniques)%in%short_names], 
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/metrics-local-2States-500kWhDemand-lite.csv', row.names=F)
writeLinesShape(all_lines, 
                '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/networks-proposed-2States-500kWh.shp')

