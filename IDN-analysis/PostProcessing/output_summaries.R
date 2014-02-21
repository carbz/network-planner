#^. TO USE THIS FILE, PLEASE UN-ZIP THE INPUT FILE AND CHANGE THE WORKING DIRECTORY
#^. TO CHANGE THE WORKING DIRECTORY, 
#^. CHANGE THE ADDRESS of the folder in line 12 with setwd("FULL PATH OF YOUR FOLDER") e.g "~/Documents/Modi Labs/"
rm(list=ls())

require(gdata)
require(plyr)
require(stringr)
require(data.table)
require(ggplot2)
require(grid)
require(maptools)

source('~/github/local/network-planner//IDN-analysis//PostProcessing/interpret_commonfunctions.R')


#Setting working directory to C:/Users/zmyao/Dropbox/Network Planning/230"
#setwd("C:/Users/zmyao/Dropbox/Network Planning/230")

# ##Jonathan's directory

setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/")
#Import metrics local summary of each settlements
maluku <- read.csv("552-Maluku-900HHD/metrics-local.csv", skip=1,stringsAsFactors=F)
flores <- read.csv("543-Flores-900HHD/metrics-local.csv", skip=1,stringsAsFactors=F)##Need to update! 
malukuutara <- read.csv("551-MalukuUtara-900HHD/metrics-local.csv", skip=1,stringsAsFactors=F)
sumba <- read.csv("542-Sumba-900HHD/metrics-local.csv", skip=1,stringsAsFactors=F)
timor <- read.csv("554-KupangArea-900HHD/metrics-local.csv", skip=1,stringsAsFactors=F)

#Import proposed networks for interpeting pipeline network
maluku_proposed <- readShapeLines("552-Maluku-900HHD/networks-proposed.shp")
flores_proposed <- readShapeLines("543-Flores-900HHD/networks-proposed.shp")
malukuutara_proposed <- readShapeLines("551-MalukuUtara-900HHD/networks-proposed.shp")
sumba_proposed <- readShapeLines("542-Sumba-900HHD/networks-proposed.shp")
timor_proposed <- readShapeLines("554-KupangArea-900HHD/networks-proposed.shp")

#Import Metrics Gloabl stuff too 
maluku_global <- load.global(read.csv("552-Maluku-900HHD/metrics-global.csv",stringsAsFactors=F))
flores_global <- load.global(read.csv("543-Flores-900HHD//metrics-global.csv",stringsAsFactors=F))
malukuutara_global <- load.global(read.csv("551-MalukuUtara-900HHD/metrics-global.csv",stringsAsFactors=F))
sumba_global <- load.global(read.csv("542-Sumba-900HHD/metrics-global.csv",stringsAsFactors=F))
timor_global <- load.global(read.csv("554-KupangArea-900HHD/metrics-global.csv",stringsAsFactors=F))


#Develop some basic plots of data 
p1 <- ggplot(malukuutara, aes(x = X, y = Y)) + 
  geom_point(aes(colour = Metric...System)) + 
  labs(title = "NetworkPlanner Outputs - Maluku Utara")+
  coord_equal()

p2 <- ggplot(maluku, aes(x = X, y = Y)) + 
  geom_point(aes(colour = Metric...System)) + 
  labs(title = "NetworkPlanner Outputs - Maluku") + 
  coord_equal()

p3 <- ggplot(flores, aes(x = X, y = Y)) + 
  geom_point(aes(colour = Metric...System)) + 
  labs(title = "NetworkPlanner Outputs - Flores") +
  coord_equal()

p4 <- ggplot(sumba, aes(x = X, y = Y)) + 
  geom_point(aes(colour = Metric...System)) + 
  labs(title = "NetworkPlanner Outputs - Sumba") +
  coord_equal()

p5 <- ggplot(timor, aes(x = X, y = Y)) + 
  geom_point(aes(colour = Metric...System)) + 
  labs(title = "NetworkPlanner Outputs - Kupang Area")+
  coord_equal()


multiplot(p1, p2, p3, p4, p5, cols=2)

#Take a visual look at our info

#Summarize outputs by technology type (ie Off-Grid, Mini-Grid and Grid systems)
malukuutara_summary <- summarize_metrics_local(malukuutara)
maluku_summary <- summarize_metrics_local(maluku)
flores_summary <- summarize_metrics_local(flores)
sumba_summary <- summarize_metrics_local(sumba)
timor_summary <- summarize_metrics_local(timor)

##Summarize Key Files

all_data_sets <- list(malukuutara_summary,
                      maluku_summary,
                      flores_summary,
                      sumba_summary,
                      timor_summary)

global_data_sets <- list(malukuutara_global,
                         maluku_global,
                         flores_global,
                         sumba_global,
                         timor_global)

names <- c("malukuutara_summary",
           "maluku_summary",
           "flores_summary",
           "sumba_summary",
           "timor_summary")


for (i in 1:length(all_data_sets)){
  local_agg <- as.data.frame(all_data_sets[1])
  name <- names[1]
  global <- as.data.frame(global_data_sets[1])
  #Grid Summary
  grid<- grid.summary(local_agg, global)
  mg <- mini.grid.summary(local_agg)
  og <- off.grid.summary(local_agg)
}
  

# output final result by grid.type
write.csv(mini.grid.table, "mini_grid_result.csv", row.names=F)
write.csv(off.grid.table, "off_grid_result.csv", row.names=F)
write.csv(grid.table, "grid.table.csv", row.names=F)



#Append total & Column percentage
pct <- as.data.frame(prop.table(as.matrix(local_agg[,2:ncol(local_agg)]),2))
colnames(pct) <- paste("percentage of", colnames(pct), sep=": ")

name_vec <- names(local_agg)[1]
for (i in 1:ncol(pct))
{
    name_vec <- c(name_vec, names(local_agg)[2:ncol(local_agg)][i], names(pct)[i])    
}
local_agg_output <- cbind(local_agg,pct)
local_agg_output <- local_agg_output[,name_vec]
local_agg_output["Total", 2:ncol(local_agg_output)] <- colSums(local_agg_output[, 2:ncol(local_agg_output)])
local_agg_output[,"Metric...System"] <- as.character(local_agg_output[,"Metric...System"])
local_agg_output[5, "Metric...System"] <- "Total"
rownames(local_agg_output) <- NULL