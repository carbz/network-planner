#Feb 12, 2015
#NGA NEAP project to chracterize polling unit data
#develop matrix of apparent communities perceived within sampled polling units
#track key features of communities s.a. (i) compound qty, (ii) lv network and (iii) 
#track key features of polling units s.a. (i) density, (ii) area (iii) number of voters



require(WriteXLS)
require(ggplot2)
require(plyr)


##1. INPUT: read in the population dataset
pts1_path <-'~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/clustering-set_cover/aggregated-All_Points-SetCovering-search_radius_100m.csv'
pts2_path <-'~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf2/clustering-set_cover/aggregated-All_Points-SetCovering-search_radius_100m.csv'
pts3_path <-'~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf3/clustering-set_cover/aggregated-All_Points-SetCovering-search_radius_100m.csv'

pts1 <- read.csv(pts1_path)
pts2 <- read.csv(pts2_path)
pts3 <- read.csv(pts3_path)

#combine together
pts <- rbind.fill(pts1, pts2)
pts <- rbind.fill(pts, pts3)

##2. Convert csv pts to spatial object
# coordinates(pop_pts) = ~x+y
coordinates(pts) = ~x_centroid+y_centroid

##3. INPUT Load in Polling Unit shapefiles
states_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/shapefiling/KanoPollingUnits-rural_only/KanoPollingUnits-rural_only-ALL-VoronoiBounds.shp'
pus <- readShapePoly(states_path) ##akin to Delhi sub-division
#clean out jenky attributes
pus$radius.km. <- NULL
pus$radius.m. <- NULL
pus$radius_deg <- NULL


##4. Which polygon are the pts inside?
# State_GIS <- over(pop_pts,MMR_polygon)
pu_parent <- over(pts, pus)

##5. Assign Polygon attribute data to Points
pts <- cbind(pts, pu_parent)


#6 Explore relationships...
df <- data.frame(pts)
attach(df)

vars <- c('mst_length_m','compounds_qty',
          'mst_length_per_compound_m','Sum.of.reg',
          'Area_m')

tags = c('LV Network [m]','Structure Count [qty]','LV per Structure [m/HH]',
           'Registered Voters [qty]', 'Polling Unit Area [m^2]')

library(GGally)
pairs(df[vars],
      main='Three Regions: Characterizing Communites within 100m',
      lower.panel=NULL, 
      cex.labels = 1.5, #font size
      labels = tags)

plot <- GGally::ggpairs(df[,vars], 
        title='Three Regions: Characterizing Communites within 100m',
        columnLabels = tags,
        colour=factor('Sum.of.reg'),
        alpha = 1)

save_to <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/'
png(filename = paste0(save_to,'ScattersOfClusters-correlations.png'),
    width = width, height=height)
plot(plot)
dev.off()
    
width = 1300
height = 1000


#summarize by polling unit
attach(df)
polls <- ddply(df, .(df$PollingUni), summarize,
               Total_Voters.qty = max(Sum.of.reg),
               Compounds_100m_or_more.qty = sum(compounds_qty[which(compounds_qty==1)]),
               Total_Compouds.qty = sum(compounds_qty),
               Polling_Unit_Area.m = max(Area_m),
               Total_LV_est.m = sum(mst_length_m, na.rm=T),
               Average_LV_per_HH.m = mean(mst_length_per_compound_m)
               )

GGally::ggpairs(polls,
                2:7,
                title='Three Regions: Characterizing Polling Units',
#                 columnLabels = tags,
                alpha = 1)

