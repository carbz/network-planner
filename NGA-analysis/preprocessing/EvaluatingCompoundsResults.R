##1. INPUT: read in the population dataset
pts1_path <-'~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/clustering-set_cover/All_Points-SetCovering-search_radius_100m.csv'
pts2_path <-'~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf2/clustering-set_cover/All_Points-SetCovering-search_radius_50m.csv'
pts3_path <-'~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf3/clustering-set_cover/All_Points-SetCovering-search_radius_100m.csv'

pts1 <- read.csv(pts1_path)
pts2 <- read.csv(pts2_path)
pts3 <- read.csv(pts3_path)

#combine together
library(plyr)
pts_all <- rbind.fill(pts1, pts2)
pts_all <- rbind.fill(pts_all, pts3)

x_y <- (pts_all[,c(3,4)])
x <- (pts_all[,3])
y <- (pts_all[,4])

#Convert to UTM
library(sp)
library(rgdal)

# pts_all <- x_y
# pts_all <- pts2

coordinates(pts_all) <- c("x", "y")
proj4string(pts_all) <- CRS("+proj=longlat +datum=WGS84")  ## for example

res <- spTransform(xy, CRS("+proj=utm +zone=51 ellps=WGS84"))
res <- spTransform(pts_all, CRS("+proj=utm +zone=32 ellps=WGS84"))
utm <- as.data.frame(as(res, "SpatialPoints"))


#Adjacency Matrix
adj_mat <- (as.matrix(dist(utm, upper=TRUE)))
for (i in 1:nrow(adj_mat))
  {
  points_dis[i] <- min(adj_mat[,i][adj_mat[,i]>0])
}

##Append adjacency value to all compounds
pts_all$points_dis <- points_dis

# save_as <- '~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/All_Points_Buf123_nearnes_index.csv'
# write.csv(pts_all, save_as, row.names=F)

##2. Convert csv pts to spatial object
# coordinates(pop_pts) = ~x+y
coordinates(data.frame(pts_all)) = ~x+y

##3. INPUT Load in Polling Unit shapefiles
#pus_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/shapefiling/KanoPollingUnits-rural_only/KanoPollingUnits-rural_only-ALL-VoronoiBounds.shp'
pus_path <- '/Users/carbz/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf3/comparitive_np_scenarios/PU_mv-proposed-via_clustering+pu.shp'
pus <- readShapePoly(pus_path) ##akin to Delhi sub-division
#clean out jenky attributes
pus$radius.km. <- NULL
pus$radius.m. <- NULL
pus$radius_deg <- NULL


##4. Which polygon are the pts inside?
# State_GIS <- over(pop_pts,MMR_polygon)
proj4string(pts_all) <- CRS("+proj=longlat +datum=WGS84")  ## for example
proj4string(pus) <- CRS("+proj=longlat +datum=WGS84")  ## for example

pu_parent <- over(pts_all, pus)

##5. Assign Polygon attribute data to Points
pts <- cbind(pts_all, pu_parent)


#6 Explore relationships...
df <- data.frame(pts)
attach(df)

vars <- c('points_dis','Sum.of.reg','Area_m')
# 'PollingUni',

tags = c('LV Network [m]','Structure Count [qty]','LV per Structure [m/HH]',
         'Registered Voters [qty]', 'Polling Unit Area [m^2]')

library(GGally)
pairs(df[vars],
      main='Three Regions: Characterizing Compounds',
      lower.panel=NULL, 
      cex.labels = 1.5) #font size
      labels = tags)

ggpairs(df[vars],
        title='Three Regions: Characterizing Compounds')

#summarize by polling unit
attach(df)

#"cluster_id"        "central_node"      "x"                 "y"                 "node_id"           "weight_households" "points_dis"       
#"PollingUni"        "Sum.of.reg"        "Lat"               "Long"              "Inhabited"         "Area_m"            "VoterDensi"       

polls_no_cluster <- ddply(df, .(df$PollingUni), summarize,
               Total_Voters.qty = max(Sum.of.reg),
               Total_Compounds.qty = sum(weight_households),
               Isolated_Compounds_100m.qty = sum(weight_households[which(points_dis>100)]),
               Isolated_Compounds_75m.qty = sum(weight_households[which(points_dis>75)]),
               Isolated_Compounds_50m.qty = sum(weight_households[which(points_dis>50)]),
               Polling_Unit_Area.m = max(Area_m)
)

GGally::ggpairs(polls_no_cluster,
                2:7,
                title='Three Regions: Characterizing Polling Units by Compounds Adjacency',
                #                 columnLabels = tags,
                alpha = 1)



