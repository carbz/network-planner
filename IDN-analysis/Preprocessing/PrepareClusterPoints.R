source('~/github/local/network-planner/IDN-analysis/Preprocessing/DataMungingFunctions.R')

#original processed BIG data had some gaps but was usable
cluster_pointsV1 <- read.csv("cluster_points_NTT+MMU.csv", header = T)
row.names(cluster_points) <- cluster_points$Clust_Code

#As of Jan 2, Shaky corrected for landuse areas that spanned multiple Desas
cluster_pointsV2 <- read.csv("ShakyUpdate-20140102/All_Settlement_Points_with_Population.csv", header = T)

#on Jan 8, we incorporated the Sula area
cluster_pointsV3 <- read.csv("ShakyUpdate-20140108/Sula_All_Settlement_Points_with_Population.csv", header = T)

#combine Shaky's two most up to data cluster identification files
cluster_points <- rbind.fill(cluster_pointsV2, cluster_pointsV3)

#Cluster points are without a unique cluster code
cluster_points <- unique_clust_code(cluster_points)

#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########
#Import centroid file which has geographic region depicted
setwd('~/github/local/network-planner/IDN-analysis/Preprocessing/')

NTT_Centroids <- readShapePoints("Shapefiles/NTT_centroids.shp")
NTT_Centroids <- as.data.frame(NTT_Centroids) 
#Renames 'EI_Subarea' variable name in NTT to agree with other Provinces
NTT_Centroids <- rename(NTT_Centroids, replace=c("EI_subarea"="EI_SubArea"))

Maluku_Centroids <- readShapePoints("Shapefiles/Maluku-Desa_centroids.shp")
MalukuUtara_Centroids <- readShapePoints("Shapefiles/Maluku_Utara-Desa_centroids.shp")

#Combine all shapefiles of centroids into single dataframe 
all_centroids <- rbind.fill(as.data.frame(NTT_Centroids),
                            as.data.frame(Maluku_Centroids),
                            as.data.frame(MalukuUtara_Centroids))

#rename lat, long columns to better agree with NP nomenclature
all_centroids <- rename(all_centroids, replace=c("coords.x1"="Longitude",
                                                 "coords.x2"="Latitude"))

#establish a unique cluster code for Desa centroid locations for faster indexing
all_centroids$Clust_Code <- str_c(all_centroids$IDSP2010,"99")
all_centroids$XY_Source <- "BPS_Centroids"

###DATA DIRTY
#integrity issue with Centroids file is resolved here
#certain desas are composed of multi-polygons and therefore have multiple observations reported
#specifically, the following desas have repeat, non-unqiue, centroid observations.  
Repeat_Desas = c(530901001299,
                 810602002199, 
                 820306200199, 
                 820402200299, 
                 820506000199)

multiple_centroid_desas <- all_centroids[which(all_centroids$Clust_Code %in% Repeat_Desas),]

#use loop to select first occurence of each repeat Desa centroid in 'consolidate_multiples' df
consolidate_multiples <- as.data.frame(NULL)
for (i in 1:length(Repeat_Desas)){
  first_of_repeats <- all_centroids[which(all_centroids$Clust_Code == Repeat_Desas[i]),][1,]
  consolidate_multiples <- rbind.fill(consolidate_multiples,
                                      first_of_repeats)
}
##omit the the repeats for now because they're causing complications and I dont know which single one to select to include
all_centroids <- all_centroids[which(!(all_centroids$Clust_Code %in% multiple_centroid_desas$Clust_Code)),]
#now add back in only the first of each repeated Desa Centroid 
all_centroids <- rbind.fill(all_centroids,
                            consolidate_multiples)
###DATA Cleaned
#####~~~now we have centroid file to reference~~~~~~~~~~~~~~##########

row.names(all_centroids) <- all_centroids$Clust_Code



#Output one consolidated list of settlement location w/ pop & household data broken down by PLN geographic region

#Ensure imported clusters have regional classifications assigned
cluster_points$EI_SubArea <- NA
cluster_points$PLN_Cabang <-NA

##This loop takes a long time to run - so I only want to do it once...
for (i in 1:nrow(cluster_points)) 
{
  #assign the corresponding EI SubArea value
  cluster_code <- str_c(cluster_points[i, "IDSP2010"],99)
  cluster_points[i, "EI_SubArea"] <- as.character(all_centroids[cluster_code, "EI_SubArea"])
  cluster_points[i, "PLN_Cabang"] <- as.character(all_centroids[cluster_code, "PLN_Cabang"])
}

#My output is a more consolidated list of clusters so this one time analysis does not need to be re-run in future
write.csv(cluster_points, "~//Dropbox//Indonesia Geospatial Analysis//Data Modeling and Analysis//NPinputs//Jan2014-Preprocessing/cluster_points_NTT+MMU-updated20140109+Clust_Code.csv")
write.csv(cluster_points, "~//github//local//network-planner//IDN-analysis//Preprocessing//cluster_points_NTT+MMU-updated20140109+Clust_Code.csv")

