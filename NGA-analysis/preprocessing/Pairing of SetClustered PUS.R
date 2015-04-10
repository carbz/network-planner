# Apr 10, 2015
# NGA NEAP project to characterize polling unit data with everything we know
# Pairing Clustered Outputs previously performed in SetCovering.py
# with their appropriate parent polling unit as well as respective voter records

##1. INPUT: read in the population dataset
pu_path <-'~/Dropbox/Nigeria-NEAP-GIS/preprocessing/polling_units/polling_units_per_transformers/polling_units_per_transformers-v2.csv'
pts <- read.csv(pu_path)

##2. INPUT: reading previous version of clusterd PUs
clustered100_pts_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/All_Points-SetCovering-search_radius_100m.csv'
clustered100 <- read.csv(clustered100_pts_path)

clustered500_pts_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/All_Points-SetCovering-search_radius_500m.csv'
clustered500 <- read.csv(clustered500_pts_path)

#3. INPUT: original polling unit file 
pus_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/all-polling-units-20150402.csv'
pus <- read.csv(pus_path)

#4. Prepare polling units for pairing 
pus$central_node = rownames(pus) #rownames are central_node
pus$central_node <- as.integer(pus$central_node)-1 #0 indexed
pus <- pus[c('central_node','NAME')] #all we need
colnames(pus) <- c('central_node','parent_PUID') #name it better

#5. Merge clustered outputs with original parent PU
clustered100v2 <- merge(clustered100, pus, by = "central_node", all.x=T)
clustered500v2 <- merge(clustered500, pus, by = "central_node", all.x=T)

#6. While we're merging, pass through registered voters too 
voters <- pts[c('PUID','Sum_regist')]
clustered100v2 <- merge(clustered100v2, voters, by.x = "node_id", by.y = "PUID", all.x=T)
clustered500v2 <- merge(clustered500v2, voters, by.x = "node_id", by.y = "PUID", all.x=T)

#7. OUTPUT: pass csv's to directory
#Write csv's now
directory_name <- "~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/clustering-v2/"
write.csv(clustered100v2, paste0(directory_name,
                     'All_Points-SetCovering-search_radius_100m.csv'),
          row.names=F)

write.csv(clustered500v2, paste0(directory_name,
                                 'All_Points-SetCovering-search_radius_500m.csv'),
          row.names=F)