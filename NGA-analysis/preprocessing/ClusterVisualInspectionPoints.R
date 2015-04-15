# Apr 10, 2015
# NGA NEAP project to characterize polling unit data 
# Objective is to run a NP scenario of visual inspected clusters (500m)
# assign pop such that it is 2x voters
# voters proportioned out in accordance to polling unit  
# end result, MV/HH vs. density 

require(WriteXLS)
require(ggplot2)
require(plyr)
library(sp)
library(rgdal)
library(maptools)

##1. INPUT: read in the population dataset
pts_path <-'~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/comparitive_np_scenarios/All_Compounds-SetCovering_radius_500m.csv'
pts <- read.csv(pts_path)

##1A. INPUt: read all set covered scenaros as sensitivity
pts2000 <- assemble_compounds(2000)
pts1000 <- assemble_compounds(1000)
pts500 <- assemble_compounds(500)
pts100 <- assemble_compounds(100)

##1. INPUT: read in the population datase

assemble_compounds <- function(dist)
{
  main_folder <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/'
  pts1_path <- paste0(main_folder,
                      'Buf1/clustering-set_cover/aggregated-All_Points-SetCovering-search_radius_',
                      dist,
                      'm.csv')
  pts2_path <- paste0(main_folder,
                      'Buf2/clustering-set_cover/aggregated-All_Points-SetCovering-search_radius_',
                      dist,
                      'm.csv')
  pts3_path <- paste0(main_folder,
                      'Buf3/clustering-set_cover/aggregated-All_Points-SetCovering-search_radius_',
                      dist,
                      'm.csv')
  
  pts1 <- read.csv(pts1_path)
  pts2 <- read.csv(pts2_path)
  pts3 <- read.csv(pts3_path)
  
  #combine together
  pts <- rbind.fill(pts1, pts2)
  pts <- rbind.fill(pts, pts3)
  
  #assign x & y 
  pts$x <- pts$x_centroid
  pts$y <- pts$y_centroid
  
  #give it a comment
  comment(pts) <- as.character(dist) 
  
  #output
  return(pts)
}

cross_ref_pts <- function(pts)
{
  
  meta <- comment(pts)
  ##2. Convert csv pts to spatial object
  # coordinates(pop_pts) = ~x+y
  coordinates(pts) = ~x+y
  
  ##3. INPUT Load in Polling Unit shapefiles
  pus_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/shapefiling/KanoPollingUnits-rural_only/KanoPollingUnits-rural_only-ALL-VoronoiBounds.shp'
  #lga_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/preprocessing/workspace/admin2-LGA-w_subsets/KEDCO_3_States_Kano_Katsina_Jigawa.shp'
  pus <- readShapePoly(pus_path) ##akin to Delhi sub-division
  
  
  ##4. Which polygon are the pts inside?
  # State_GIS <- over(pop_pts,MMR_polygon)
  pus_parent <- over(pts, pus)
  
  ##Assign Polygon attribute data to Points
  pus_fields <- c("PollingUni")
  
  pts <- cbind(pts, pus_parent[pus_fields])
  
  ##4.1 Which clustered polling unit too? 
  clustered500_pts_path <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/clustered_polling_units/All_Points-SetCovering-search_radius_500m.csv'
  clustered500 <- read.csv(clustered500_pts_path)
  clustered500$cluster_group_500m <- clustered500$cluster_id
  
  clustered500 <- clustered[c("node_id","cluster_group_500m")]
  merged <- merge(pts, clustered500, by.x = "PollingUni", by.y = "node_id", all.x=T, all.y=F)
  pts <- merged 
  
  
  
  ##5. Assign Polygon attribute data to Points
  pus_fields <- c("Sum.of.reg","Area_m","PollingUni")
  pts <- cbind(pts, pus_parent[pus_fields])
  
  #6 Assign demographic relationships...
  df <- data.frame(pts)
  df$voter_est <- NA
  df$pop <- NA
  
  
  #dummy value of 573 for voterless PUs
  df$Sum.of.reg[which(is.na(df$Sum.of.reg))] <- 573
  
  # for (i in 1:dim(df)[1]){
  #   if (is.na(df$Sum_regist[i])){
  #     df$voter_est[i] = 590 #polling unit voter density
  #     #http://www.inecnigeria.org/?inecnews=chairs-press-conference-on-pus
  #     df$voter_source[i] = "INEC_average"
  #   } else {
  #     df$voter_est[i] = df$Sum_regist[i]
  #     df$voter_source[i] = "StakeholderDemocracy"
  #   }
  # }

  #How many voters per LGA?
  df$PU_compounds <- NA
  PU_compound_sums <- ddply(df, .(PollingUni), summarise,
                            PU_compounds = sum(compounds_qty, na.rm=T),
                            PU_Area_m = min(Area_m),
                            cluster_group_500m = cluster_group_500m[1],
                            Sum.of.reg = Sum.of.reg[1]
                            )
  
  for (j in 1:dim(PU_compound_sums)[1]) {
    df$PU_compounds[which(df$PollingUni == PU_compound_sums$PollingUni[j])] <- 
      PU_compound_sums$PU_compounds[j]
  }
  
  df$voter_percent_of_PU <- df$compounds_qty/df$PU_compounds
  
  #estimate population per polling units
  df$voter_est <- as.integer(df$voter_percent_of_PU * df$Sum.of.reg)
  df$pop <- df$voter_est*2.0
  
  df$demo_mn_inter_hh_dist <- df$mst_length_m/(df$pop/8)
  
  #Voter density?
  PU_500m_density_voter.per.sqm <- ddply(PU_compound_sums, .(PollingUni), summarise,
                            PU_500m_density = (Sum.of.reg/PU_Area_m)*1000^2)
  
  df$PU_500m_density_voter.per.sqm <- NA
  for (k in 1:dim(PU_500m_density_voter.per.sqm)[1]) {
    df$PU_500m_density_voter.per.sqm[which(df$PollingUni == PU_compound_sums$PollingUni[k])] <- 
      PU_500m_density_voter.per.sqm$PU_500m_density[k]
  }
  
  
  #preserve commenting
  comment(df) <- as.character(meta)
  
  return(df)
}

df2000 <- cross_ref_pts(pts2000)
df1000 <- cross_ref_pts(pts1000)
df500 <- cross_ref_pts(pts500)
df100 <- cross_ref_pts(pts100)





#Write csv's now
output_csv <- function(df)
{
  directory_name <- "~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/comparitive_np_scenarios/"
  write.csv(df, paste0(directory_name,
                       'All_Compounds-SetCovering_radius_',
                       comment(df),
                       'm-NP_ready.csv'),
            row.names=F)
  
}

df100[is.na(df100)] <- 0 
df500[is.na(df500)] <- 0 
df1000[is.na(df1000)] <- 0 

output_csv(df2000)
output_csv(df1000)
output_csv(df500)
output_csv(df100)



