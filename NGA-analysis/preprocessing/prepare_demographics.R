# Apr 9, 2015
# NGA NEAP project to characterize polling unit data with everything we know

#  *******************************************************************  #
#  *******************************************************************  #
# Develop demographic files based on polling units include
# (i) Voronoi service territory, (ii) clustered group, (iii) vicinity to xfrmrs,
# (iv) voters, (v) source of voter information, (vi) parent LGA population,
# (vii) LGA percent voters, (viii) poverty-LGA, (ix) poverty-5km-pixel, 
# (x) admin_data, (xi) is_urban? (xii) modeling_subset_region 
#  *******************************************************************  #
#  *******************************************************************  #

require(WriteXLS)
require(ggplot2)
require(plyr)
library(sp)
library(rgdal)
library(maptools)

##1. INPUT: read in the population dataset
pu_path <-'~/Dropbox/Nigeria-NEAP-GIS/preprocessing/polling_units/polling_units_per_transformers/polling_units_per_transformers-v2.csv'
pts <- read.csv(pu_path)

##1. Account for clustering too
clustered100_pts_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/All_Points-SetCovering-search_radius_100m.csv'
clustered500_pts_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/All_Points-SetCovering-search_radius_500m.csv'

clustered100 <- read.csv(clustered100_pts_path)
clustered100$cluster_group_100m <- clustered100$cluster_id

clustered500 <- read.csv(clustered500_pts_path)
clustered500$cluster_group_500m <- clustered500$cluster_id

clustered <- merge(clustered100, clustered500, by = "node_id", all=T)

clustered <- clustered[c("node_id","cluster_group_500m","cluster_group_100m")]
merged <- merge(pts, clustered, by.x = "PUID", by.y = "node_id", all=T)

##2. Convert csv pts to spatial object
# coordinates(pop_pts) = ~x+y
merged <- merged[-which(is.na(merged$X)),]
coordinates(merged) = ~X+Y
pts <- merged

##3. INPUT Load in Polling Unit shapefiles
#pus_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/shapefiling/KanoPollingUnits-rural_only/KanoPollingUnits-rural_only-ALL-VoronoiBounds.shp'
lga_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/preprocessing/workspace/admin2-LGA-w_subsets/KEDCO_3_States_Kano_Katsina_Jigawa.shp'
lgas <- readShapePoly(lga_path) ##akin to Delhi sub-division


##4. Which polygon are the pts inside?
# State_GIS <- over(pop_pts,MMR_polygon)
lga_parent <- over(pts, lgas)

##5. Assign Polygon attribute data to Points
lga_fields <- c("LGA","STATE","POP_DENS","AREA_KM2",
                "POP_2006","Subset_Reg")

pts <- cbind(pts, lga_parent[lga_fields])


#6 Assign demographic relationships...
df <- data.frame(pts)
df$voter_est <- NA
df$pop <- NA

for (i in 1:dim(df)[1]){
  if (is.na(df$Sum_regist[i])){
    df$voter_est[i] = 590 #polling unit voter density
    #http://www.inecnigeria.org/?inecnews=chairs-press-conference-on-pus
    df$voter_source[i] = "INEC_average"
  } else {
    df$voter_est[i] = df$Sum_regist[i]
    df$voter_source[i] = "StakeholderDemocracy"
  }
}

#How many voters per LGA?
df$LGA_voter_sums <- NA
LGA_voter_sums <- ddply(df, .(LGA), summarise,
                        LGA_voters = sum(Sum_regist, na.rm=T))

for (j in 1:dim(LGA_voter_sums)[1]) {
    df$LGA_voter_sums[which(df$LGA == LGA_voter_sums$LGA[j])] <- 
      LGA_voter_sums$LGA_voters[j]
}

df$voter_percent_of_LGA <- df$voter_est/df$LGA_voter_sums

#estimate population per polling units
df$pop <- integer(df$voter_percent_of_LGA * df$POP_2006)


##Best Guess of connected KEDCO connections
penetration_rate <- 0.7
hhold_size = 6.1
existing_connections = 350000

df <- df[order(df$Distance_to_xfrmr),]
df$cum_HH_est = cumsum(df$pop/hhold_size*penetration_rate)
  
df$KEDCO_connection <- 'Unlikely'
df$KEDCO_connection[which(df$cum_HH_est<existing_connections)] <- 'Probable'

df$tgt_ho_prt <- 1.0
df$tgt_ho_prt[which(df$cum_HH_est<existing_connections)] <- 0.3

#Cluster Close Polling Units

df_clustered <- ddply(df, .(cluster_group_100m), summarise,
                      PUID = PUID[1],
                      X = mean(X), Y = mean(Y),
                      Sum_regist = sum(Sum_regist, na.rm=T),
                      INEC_Records_via_StakeholderDemocracy = sum(INEC_Records_via_StakeholderDemocracy, na.rm=T),
                      Distance_to_xfrmr = mean(Distance_to_xfrmr),
                      cluster_group_500m = cluster_group_500m[1],
                      cluster_group_100m_count = length(cluster_group_100m),
                      LGA = LGA[1], STATE = STATE[1], POP_DENS =POP_DENS[1],
                      AREA_KM2 = AREA_KM2[1],LGA_POP_2006=POP_2006[1],
                      Subset_Reg = Subset_Reg[1], 
                      voter_est = sum(voter_est),
                      tgt_ho_prt = sum(tgt_ho_prt*pop)/sum(pop),
                      household_penetration_min = min(tgt_ho_prt),
                      pop = sum(pop),
                      voter_source = voter_source[1],
                      LGA_voter_sums = LGA_voter_sums[1],
                      voter_percent_of_LGA = sum(voter_percent_of_LGA))

#Write csv's now
directory_name <- "~/Dropbox/Nigeria-NEAP-GIS/preprocessing/demographics/NP_modeling/"
write.csv(df, paste0(directory_name,
                          'ALL_KEDCO_demographics.csv'),
          row.names=F)

write.csv(df_clustered, paste0(directory_name,
                     'ALL_KEDCO_demographics-100m_clustering.csv'),
          row.names=F)

regions <- unique(df$Subset_Reg)

for (i in 1:length(regions)){
  output <- subset(df, Subset_Reg == regions[i])
  write.csv(output, paste0(directory_name,
                           'subsets/',
                           regions[i],
                           '_KEDCO_demographics.csv'),
            row.names=F)
  
  output_clustered <- subset(df_clustered, Subset_Reg == regions[i])
  write.csv(output, paste0(directory_name,
                           'subsets_clustered/',
                           regions[i],
                           '_KEDCO_demographics.csv'),
            row.names=F)
}


##Repeat with data within the Buffer

