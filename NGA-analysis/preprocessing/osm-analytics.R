#jcarbz
#March 30, 2015
#Input: Take in a single shapefile of the existing grid
#Output: (1) calculate a 'cum_dist' attribute column of 'dist' attribute
#         (2) k subsets of the shapefile each of equal 'dist' length


#load libraries
library(maptools)
library(chron)

#Read in data to split
path_name <-"~/Dropbox/Nigeria-NEAP-GIS/animation_for_blog/"

osm_lines <- readShapeLines(paste0(path_name,
                                   'KEDCO-MV_lines-20150327/KEDCO-MV_lines-20150327.shp'))

#liten up
osm_lines <- osm_lines[c('osm_id','osm_user','osm_user','osm_versio','osm_timest',
                         'dist', 'commit_seq')]

#Pull time range of data of interest
osm_lines$start_time <- as.chron(as.character(osm_lines$osm_timest))
#date1 <- as.POSIXct("2014-12-03 0:00:00")
date1 <- "12/11/2014"
osm_lines = osm_lines[order(osm_lines$start_time),]
osm_lines = osm_lines[which(osm_lines$start_time > date1),]

#day counter
start_date <- min(osm_lines$start_time)
osm_lines$days_mapping <- as.integer(osm_lines$start_time- start_date)

#Format shapefile to make a little more useful
#Add cummulative MV count
osm_lines$commit_seq <- row.names(osm_lines)
osm_lines$cum_dist <- cumsum(osm_lines$dist)


#Subset & output osm_lines.shp 
total_mv <- max(osm_lines$cum_dist)
  <- max(osm_lines$start_time) - min(osm_lines$start_time)

k=10
osm_lines_i <- subset(osm_lines, commit_seq<10)

for (j in 1:k){
  #lower_cutoff <- (j-1)/100*total_days
  lower_cutoff <- 0 #cummulative
  upper_cutoff <- j/k*total_days
  
  
  
  osm_lines_i <- osm_lines[which(osm_lines$days_mapping <= upper_cutoff),]
  total_mv_i <- max(osm_lines_i$cum_dist)
  
  #cleaning up for output
  osm_lines_i$start_time <- NULL
  osm_lines_i$days_mapping <- as.integer(osm_lines_i$days_mapping)
  osm_lines_i$osm_timest <- as.character(osm_lines_i$osm_timest)
    
  writeLinesShape(osm_lines_i, 
                  paste0(path_name,
                         'KEDCO-MV_lines-20150327/split/',
                         j,
                         '-KEDCO-MV_lines-20150327-',
                         round(upper_cutoff),'_days-',
                         round(total_mv_i/1000),'km.shp'))

}


#Output 
writeLinesShape(all_lines, 
                '~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/merged_tests/networks-proposed_allMMR1000kWh.shp')




proposed_i <- readShapeLines(paste0(path_name,directory_names[1],'/networks-proposed.shp'))
