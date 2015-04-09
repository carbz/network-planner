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

clustered100_pts_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/All_Points-SetCovering-search_radius_100m.csv'
clustered500_pts_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/All_Points-SetCovering-search_radius_500m.csv'

##2. Convert csv pts to spatial object
# coordinates(pop_pts) = ~x+y
coordinates(pts) = ~X+Y

##3. INPUT Load in Polling Unit shapefiles
#pus_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/shapefiling/KanoPollingUnits-rural_only/KanoPollingUnits-rural_only-ALL-VoronoiBounds.shp'
lga_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/preprocessing/workspace/admin2-LGA-w_subsets/KEDCO_3_States_Kano_Katsina_Jigawa.shp'
lgas <- readShapePoly(lga_path) ##akin to Delhi sub-division

#clean out jenky attributes
pus$radius.km. <- NULL
pus$radius.m. <- NULL
pus$radius_deg <- NULL


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
    df$voter_est[i] = 590
    df$voter_source[i] = "INEC_average"
  } else {
    df$voter_est[i] = df$Sum_regist[i]
    df$voter_source[i] = "StakeholderDemocracy"
  }
}

#How many voters per LGA?
df$voter_percent_of_LGA <- NA
df$LGA_voter_sums <- NA
LGA_voter_sums <- ddply(df, .(LGA), summarise,
                        LGA_voters = sum(Sum_regist, na.rm=T))

for (j in 1:dim(LGA_voter_sums)[1]) {
  if (df$LGA == LGA_voter_sums$LGA[j]){
    df$LGA_voter_sums[which(df$LGA == LGA_voter_sums$LGA[j])] <- 
      LGA_voter_sums$LGA_voters[j]
  }
}



#Write csv's now
directory_name <- "~/Dropbox/Nigeria-NEAP-GIS/preprocessing/demographics/NP_modeling/"
write.csv(df, paste0(directory_name,
                          'ALL_KEDCO_demographics.csv'),
          row.names=F)

regions <- unique(df$Subset_Reg)

for (i in 1:length(regions)){
  output <- subset(df, Subset_Reg == regions[i])
  write.csv(output, paste0(directory_name,
                           'subsets/',
                           regions[i],
                           '_KEDCO_demographics.csv'),
            row.names=F)}


##Repeat with data within the Buffer

