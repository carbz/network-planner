library(tools)
library(rgdal)
library(geosphere) # must be v1.3-8 or greater
library(stringr)
library(maptools)

#Jonathan's Directory 
path_name <-"~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/500kWh_Scenarios/"

directory_names <-c('158-Mon+Tanintharyi+Kayin/',
                  '160-Chin/',
                  '161-Nyapitaw/',
                  '168-Ayewardy_All/',
                  '169-Sagaing/',
                  '170-Bago_All/',
                  '171-Kachin/',
                  '172-Kayah/',
                  '707-Shan_All/',
                  '708-Mandalay/',
                  '709-Rakhine/',
                  '710-Yangon/',
                  '711-Magway/')

#Pulling most 'common' variable from datasets for easier handling##
short_names <- c('Name',"X","Y", "Metric...System","State","Demographics...Projected.household.count",
                 "Demand..household....Target.household.count","Demand...Projected.nodal.demand.per.year",
                 "System..grid....Transformer.cost","Demographics...Population.count",
                 "Demographics...Projected.population.count","Scenario","Scenario_name")
admin_codes <- c('District','District_c','Gr_ml_cm','Ho_size_r','Ho_size_u',
                 'Mg_fl_cl','Pop_2001','Pop_2011','Pop_2013','Pop_g_r','Pop_g_u',
                 'Population','Source','State','State_code','Township','Township_c',
                 'Village','Village_co','Village_fa','Village_hh','Village_po','Villagetra',
                 'Vt_code','Vt_hh','Vt_pop')
EA_codes <- c('order','piece','group','id','root','branch','dist','depth',
              'Total.Downstream.Demand.kWh','Total.Downstream.Network.Extent.m',
              'far.sighted.sequence','CumulHH','PhaseByHHQuintile','PhaseByHHQuintRnd',
              'CumulDist','PhaseByMVQuintile','PhaseByMVQuintRnd','BinsBySett.Size') 


### 0. MERGE SCENARIOS TOGETHER ####
local_all_orig <- as.data.frame(NULL)
for (i in 1:length(directory_names)){
  print(i) 
  
  ##****************************************************##
  ##*********Step 1: Combine metrics-local.csv**********##
  ##****************************************************##
  local <- read.csv(paste0(path_name,directory_names[i],'/metrics-local.csv'), 
                    skip=1, stringsAsFactors = FALSE) #RUNTIME ~ 00:28 mins
  scenario <- substr(directory_names[i],0,3)
  #local_lite <- local[,c(short_names)]
  #local_lite <- local
  
  #Merge 1,2 & 3
  shared_col_names <- intersect(names(local),names(local_all_orig))
  
  #Preserve original scenario field
  if(i==1) {
    local_all_orig <- local
    local_all_orig$Scenario <- scenario
    } else {
    ##New guys get latest scenario designation
    local_all_orig <- merge(local_all_orig, local, by=shared_col_names, all=T)
    local_all_orig[which(is.na(local_all_orig$Scenario)),'Scenario'] <- scenario
    } 
  ##********************************************************##
  ##*********Step 2: Combine networks-proposed.shp**********##
  ##********************************************************##
  
  proposed_i <- readShapeLines(paste0(path_name,directory_names[i],'/networks-proposed.shp'))
  # change their IDs so they don't conflict
  proposed_i <- spChFIDs(proposed_i, as.character(paste0(scenario,'.', proposed_i$FID)))
  proposed_i$FID <- row.names(proposed_i)
  
  if(i==1) {
    #Designate merged file
    all_lines <- proposed_i
    }else {
      # bind to previous dataset 'MVLineType' attribute
      all_lines <- rbind(proposed_i, all_lines) 
    }
} 

##************************************************************##
##*********Step 3: Eliminate duplicate settlement ************##
##**************** vertices in metrics-local composite********##
##************************************************************##


local_all_orig$XYID <- paste0(str_sub(as.character(local_all_orig$X*100000),end=7L),str_sub(as.character(local_all_orig$Y*100000),end=7L))

##Remove conflicting electrification types(Metric...System) for same settlements
local_all_orig <- local_all_orig[order(local_all_orig$Metric...System),]#get grids to the top
duplicates <- local_all_orig[which(duplicated(local_all_orig$XYID, 
                                              fromLast=FALSE)),]#minimize grid nodes being removed
uniques <- local_all_orig[which(!(duplicated(local_all_orig$XYID, 
                                             fromLast=FALSE))),]#minimize grid nodes being removed
local_all_orig <- uniques

##********************************************************##
##*********Step 4: OUTPUT the composite Datasets**********##
##********************************************************##
write.csv(local_all_orig, 
          '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networkplannerR_data/metrics-local-All-500kWh.csv', row.names=F)
writeLinesShape(all_lines, 
                '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networks-proposed-ALL-500kWh.shp')
