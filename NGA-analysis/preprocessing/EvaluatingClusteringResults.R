

setwd('~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/')
setcover_100 <- read.csv('Buf2/clustering-set_cover/aggregated-All_Points-SetCovering-search_radius_100m.csv')
# heavy2 <- read.csv('Buf2/All_points-500m_cluster-aggregations-20150109.csv')
# heavy3 <- read.csv('Buf3/All_points-500m_cluster-aggregations-20150109.csv')

names(setcover_100)
attach(setcover_100)
hist(mst_length_per_compound_m,col=1,breaks=22, 
     main='Region2: SetCover Clustering @ 100m',
     freq=F)

pairs(setcover_100[c(2,3,6)],
      main='Region2: SetCover Clustering @ 100m',
      lower.panel=NULL, 
      cex.labels = 1, #font size
      labels = c('LV Network [m]','Structure Count [qty]','LV per Structure [m/HH]'))
