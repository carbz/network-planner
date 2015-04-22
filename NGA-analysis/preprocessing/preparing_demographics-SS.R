
shaky_pop <- '~/Dropbox/Nigeria-NEAP-GIS/preprocessing/demographics/All_3_States_Polling_Units_with_Voter_Pop_with_Pre_electrified_removed.csv'
demo_ss <- read.csv(shaky_pop)

#Cluster Close Polling Units

df_clustered <- ddply(df, .(cluster_group_500m), summarise,
                      PUID = PUID[1],
                      X = mean(X), Y = mean(Y),
                      Sum_regist = sum(Sum_regist, na.rm=T),
                      Distance_to_xfrmr = mean(Distance_to_xfrmr),
                      cluster_group_500m = cluster_group_500m[1],
                      LGA = LGA[1], STATE = STATE[1], POP_DENS =POP_DENS[1],
                      AREA_KM2 = AREA_KM2[1],LGA_POP_2006=POP_2006[1],
                      Subset_Reg = Subset_Reg[1], 
                      voter_est_jc = sum(voter_est),
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
  write.csv(output_clustered, paste0(directory_name,
                                     'subsets_clustered/',
                                     regions[i],
                                     '_KEDCO_demographics.csv'),
            row.names=F)
}


demand_nodes_file <- "/Users/carbz/Dropbox/Nigeria-NEAP-GIS/preprocessing/demographics/AllKEDCO_500mSetCover_Clustered_PollingUnits_with_Pre_electrified_Pop_Reduced.csv"

