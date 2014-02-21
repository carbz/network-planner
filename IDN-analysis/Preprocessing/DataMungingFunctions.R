#libraries that all may or may not be used in script
library(plyr)
library(stringr)


unique_clust_code <- function(cluster_points){
  
  ##Develop Clust_Code field for cluster_points to give each obs a unique ID with IDSP no. as prefix
  #Order cluster points by Desa or IDSP2010 number
  cluster_points <- cluster_points[order(cluster_points$IDSP2010),]
  cluster_no <- 2
  cluster_points$Clust_Code <- NA
  cluster_points$Clust_Code[1] <- str_c(cluster_points$IDSP2010[1],
                                        str_pad(1, 2, pad = "0"))
  
  for (i in 1:(nrow(cluster_points)-1)){
    if (cluster_points$IDSP2010[i] != cluster_points$IDSP2010[i+1]){
      cluster_no <- 1
    }
    cluster_points$Clust_Code [i+1] <- str_c(cluster_points$IDSP2010[i+1],
                                             str_pad(cluster_no, 2, pad="0"))
    cluster_no <- cluster_no+1
    }
  return(cluster_points)
}
  
#Areas in NTT do not have PLN_Cabang office areas defined.  Instead only an EI_SubArea is defined
#In those cases, use the EI_SubArea values for the PLN_Cabang variable value
fill_PLN_Cabang <- function(cluster_points){
  
  cluster_points[which(is.na(cluster_points$PLN_Cabang)),'PLN_Cabang'] <- 
    cluster_points[which(is.na(cluster_points$PLN_Cabang)),'EI_SubArea'] 
  
  return(cluster_points)
}
