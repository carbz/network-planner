library(ggplot2)
source('~/github/local/network-planner//IDN-analysis//PostProcessing//interpret-metricslocal.R')



# This example uses the ChickWeight dataset, which comes with ggplot2
# First plot
p1 <- 
  ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
  geom_line() +
  ggtitle("Growth curve for individual chicks")

# Second plot
p2 <- 
  ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
  geom_point(alpha=.3) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("Fitted growth curve per diet")

# Third plot
p3 <- 
  ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
  geom_density() +
  ggtitle("Final weight, by diet")

# Fourth plot
p4 <- 
  ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
  geom_histogram(colour="black", binwidth=50) +
  facet_grid(Diet ~ .) +
  ggtitle("Final weight, by diet") +
  theme(legend.position="none")        # No legend (redundant in this graph)    


multiplot(p1, p2, p3, p4, cols=2)

#~~~~~~~Can of whoopass~~~~~~~~~~~~~

require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")

setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/")
maluku <- read.csv("537-Maluku-900HHD/metrics-local.csv", skip=1,stringsAsFactors=F)
maluku_proposed <- readShapeLines("537-Maluku-900HHD/networks-proposed.shp")
maluku_existing <- readShapeLines("537-Maluku-900HHD/networks-existing.shp")

#Proposed Extensions by Network Planner 
maluku_proposed@data$id <- rownames(maluku_proposed@data)
maluku_proposed_lines <- fortify(maluku_proposed, region="id")
maluku_proposed_lines <- join(maluku_proposed_lines, maluku_proposed@data, by = "id")

#Get the Existing Grid Lines into good shape 
maluku_existing@data$id <- rownames(maluku_existing@data)
maluku_existing_lines <- fortify(maluku_existing, region="id")
maluku_existing_df <- join(maluku_existing_lines, maluku_existing@data, by = "id")

#Now, let's incorporate some polygon shapefile polygons for background and references 
maluku_polygon <- readShapePoly("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPinputs/Dec2013-Preprocessing/Shapefiles/Maluku_with_Census_Data_+_PLN_Areas.shp")
#now let's make it more ggplottable and keep any attribute data 
maluku_polygon@data$id <- rownames(maluku_polygon@data)
maluku_polygon <- merge(maluku_polygon@data, fortify(maluku_polygon), by = 'id')

##This is the most comprehensive plot##
all_data_plot <- 
  ggplot() +
  geom_polygon(data = maluku_polygon, aes(x=long,y=lat,group=group, fill=PLN_Cabang)) +
  geom_path(data=maluku_proposed_lines, aes(x=long, y=lat, group = group, colour = 'Proposed Grid')) +
  geom_path(data=maluku_existing_df, aes(x = long, y = lat, group = group, colour='Existing Grid')) +
  geom_point(data = maluku, aes(x = X, y = Y, colour = Metric...System)) + 
  scale_fill_hue(c=40, l=40) + 
  coord_equal(ylim=c(-9,-2.5)) +
  labs(colour = "Electrification")



maluku_lines <- 
  #ggplot() + 
  geom_path(data=maluku_proposed_df, 
            aes(x=long, y=lat, group = group, colour = 'blue')) +
  scale_fill_hue(c=100, l=80) +
  coord_equal() #+
  #scale_fill_hue(c=100, l=80)

maluku_existing <- 
  ggplot() +
  geom_path(data=maluku_existing_df, aes(x = long, y = lat, group = group, colour='red')) +
  scale_fill_hue(c=40, l=40) + 
  coord_equal()
  
maluku_points <- ggplot() + 
  geom_point(data = maluku, aes(x = X, y = Y, colour = Metric...System)) + 
  labs(title = "NetworkPlanner Outputs - Maluku")


multiplot(maluku_points, maluku_lines, cols = 1)

## Make one combined plot of output data
maluku_combined <- 
  ggplot() + 
  
  geom_point(data = maluku, aes(x = X, y = Y, colour = Metric...System)) +
  geom_path(data = maluku_proposed_df, aes(long, lat, group = group)) +
  coord_equal()

maluku_combined2 <- ggplot() +
  geom_polygon(data = pp, aes(x=long,y=lat,group=group, fill=PLN_Cabang)) +
  geom_point(data = maluku, aes(x = X, y = Y, colour = Metric...System)) +
  geom_path(data = maluku_proposed_df, aes(long, lat, group = group)) +
  coord_equal()+
  scale_fill_hue(c=45, l=70)

#preserve pre-existing info with a merge
##maluku_polygon_df <- join(maluku_polygon, maluku)

ggplot() +
  geom_polygon(data = pp, aes(x=long,y=lat,group=group)) 


  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer("PLN Cabang")

ggplot(maluku_polygon) +
  aes(long,lat,group=group) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer("Hey Hey")


maluku_polygon$PLN_Cabang


#### From SummarizeSummaries rmd


p1 <- ggplot() + 
  geom_polygon(data = malukuutara_polygon, aes(x=long,y=lat,group=group, fill=population)) +
  geom_point(data = malukuutara, aes(x = X, y = Y, colour = Metric...System)) + 
  labs(title = "NetworkPlanner Outputs - Maluku Utara")+
  coord_equal()

# p2 <- ggplot() +
#   geom_polygon(data = maluku_polygon, aes(x=long,y=lat,group=group, fill=population)) +
#   geom_point(data = maluku, aes(x = X, y = Y, colour = Metric...System)) + 
#   labs(title = "NetworkPlanner Outputs - Maluku") + 
#   coord_equal()


# sumba_today <- ggplot() + 
#   geom_polygon(data = ntt_polygon, aes(x=long,y=lat,group=group, fill=population)) +
#   geom_path(data=sumba_existing, aes(x=long, y=lat, group = group, colour = 'Existing Grid')) +
#   #geom_point(data= timor, aes(x = X, y = Y, colour = Metric...System)) + 
#   geom_point(data=sumba, aes(x = X, y = Y)) + 
#   #geom_point(data=flores, aes(x = X, y = Y, colour = Metric...System)) + 
#   labs(title = "Sumba Today: 19.4% electrification") +
#   coord_equal(ylim=c(-10.5,-9), xlim=c(118.5,121))
# 
# 
# ##Timor
# timor_plot <- ggplot() + 
#   geom_polygon(data = ntt_polygon, aes(x=long,y=lat,group=group, fill=population)) +
#   geom_path(data=timor_existing, aes(x=long, y=lat, group = group)) +
#   geom_path(data=timor_proposed, aes(x=long, y=lat, group = group, colour = 'Proposed Grid')) +
#   #geom_point(data= timor, aes(x = X, y = Y, colour = Metric...System)) + 
#   geom_point(data=timor, aes(x = X, y = Y, colour = Metric...System)) + 
#   #geom_point(data=flores, aes(x = X, y = Y, colour = Metric...System)) + 
#   labs(title = "NetworkPlanner Outputs - Timor") +
#   coord_equal(ylim=c(-11,-8.9), xlim=c(121.5,125.5))
# 
# timor_today <- ggplot() + 
#   geom_polygon(data = ntt_polygon, aes(x=long,y=lat,group=group, fill=population)) +
#   geom_path(data=timor_existing, aes(x=long, y=lat, group = group, colour = 'Existing Grid')) +
#   geom_point(data=timor, aes(x = X, y = Y)) + 
#   labs(title = "Timor Today: 37% electrification") +
#   coord_equal(ylim=c(-11,-8.9), xlim=c(121.5,125.5))



# #~~~~~~~~Older iterations of a plot
# p4 <- ggplot() + 
#   geom_point(data=sumba, aes(x = X, y = Y, colour = Metric...System)) + 
#   labs(title = "NetworkPlanner Outputs - Sumba") +
#   coord_equal()
# 
# p5 <- ggplot() + 
#   geom_point(data= timor, aes(x = X, y = Y, colour = Metric...System)) + 
#   labs(title = "NetworkPlanner Outputs - Kupang Area")+
#   coord_equal()
# 
# multiplot(p1, p2, p3, p4, p5, cols=2)
# #Take a visual look at our info


##This is the most comprehensive plot##
all_data_plot <- 
  ggplot() +
  geom_polygon(data = ntt_polygon, aes(x=long,y=lat,group=group, fill=PLN_Cabang)) +
  geom_path(data=maluku_proposed_lines, aes(x=long, y=lat, group = group, colour = 'Proposed Grid')) +
  geom_path(data=maluku_existing_df, aes(x = long, y = lat, group = group, colour='Existing Grid')) +
  geom_point(data = maluku, aes(x = X, y = Y, colour = Metric...System)) + 
  scale_fill_hue(c=40, l=40) + 
  coord_equal(ylim=c(-9,-2.5)) +
  labs(colour = "Electrification")