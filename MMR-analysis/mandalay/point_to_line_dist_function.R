# Determine the nearest point on a line for a set of points
#
# input:  
#  output_dir:  where result files will be placed
#  point_csv_file:  csv of points (with Longitude, Latitude columns) 
#  line_shape_file:  shapefile of lines
# 
# output (will all be placed in output_dir): 
#  points_on_line.csv:  for each point in point_csv_file, the closest point to
#                       a line in line_shape_file
#  points_distances.csv:  point_csv_file with additional "distance" field
#                         representing the distance to the nearest line in
#                         line_shape_file
#  shortest_lines.shp:  shapefile representing shortest lines from input point
#                       to closest point on a line in line_shape_file
# 
# Run via command line:
# Rscript --vanilla point_to_segment_dists.R output_dir point_csv_file line_shape_file

require(tools); require(rgdal); require(geosphere) # must be v1.3-8 or greater

args <- commandArgs(trailingOnly = TRUE)

output_dir_name <- '~/Dropbox/Myanmar-ADB/10-draft_report/data/power_assets/'
input_point_csv_name <- '~/Dropbox/Myanmar-ADB/10-draft_report/data/village_summaries.csv'
input_line_shapefile_name <- '~/Dropbox/existing-grid//All_MMR_MVLine.shp'

input_line_shapefile_directory <- '~/Dropbox/Myanmar-ADB/10-draft_report/'
input_line_shapefile_names <- c('major_roads/major_roads.shp',
                                'secondary_roads/secondary_roads.shp',
                                'tertiary_roads/tertiary_roads.shp')


# 
# input_point_csv_name <- "tmp/demographicsLL.csv"
# input_line_shapefile_name <- "tmp/LeonaNetworks.shp"

# make points a SpatialPointsDataFrame
lonlat <- CRS("+proj=longlat +datum=WGS84")
point_df <- read.csv(input_point_csv_name)
#dataset needs lat,longs defined
# coordinates(point_df) <- ~Longitude+Latitude
coordinates(point_df) <- ~x_centroid+y_centroid



sp::proj4string(point_df) <- lonlat

# make network a SpatialLinesDataFrame
for (i in 1:length(input_line_shapefile_names)){
  input_line_shapefile_name <- stringr::str_c(input_line_shapefile_directory,
                                              input_line_shapefile_names[i])
  print(input_line_shapefile_name)
}

shape_dir <- dirname(input_line_shapefile_name)
shape_base <- file_path_sans_ext(basename(input_line_shapefile_name))

# use readOGR b/c it captures the projection
net_df <- readOGR(dsn=shape_dir, layer=shape_base)
net_df <- spTransform(net_df, lonlat)

# calculate the dists to lines
dists <- dist2Line(point_df, net_df)
dists_df <- as.data.frame(dists)
# add the distance to the original point dataset
point_df$distance_to_grid <- dists_df$distance

# create lines representing shortest distance to network
from_mat <- as.matrix(coordinates(point_df))
to_mat <- as.matrix(dists_df[,c("lon", "lat")])
sp_lines_list <- lapply(1:nrow(from_mat), function(i) {
  Lines(Line(rbind(from_mat[i,], to_mat[i,])),i)
})

sp_lines <- SpatialLines(sp_lines_list, lonlat)
lines_data <- data.frame(distance=dists_df$distance, 
                         line_id=dists_df$ID)
sp_lines_df <- SpatialLinesDataFrame(sp_lines, lines_data)

output_dir_name <- '~/Dropbox/Myanmar-ADB/10-draft_report/data/power_assets/existing-grid/grid_indicators/'
writeOGR(sp_lines_df, output_dir_name, "shortest_lines", "ESRI Shapefile")
write.csv(dists_df, file.path(output_dir_name, "points_on_line.csv"), row.names=F)
write.csv(point_df, file.path(output_dir_name, "points_distances.csv"), row.names=F)




# require(sp); require(maptools); require(plyr)
# #Given a points matrix with Longitude and Latitude fields and
# #a foritifed line file with "long" and "lat" values specified
# #this function returns a minimum the points file but with an additional variable denoting the 
# #minimum distance to a vertex on the line considered 
# 
# minimum_distance_to_line_vertices <- function(points_full, lines){
#   #'points_full' variable looks like such:
#   #   Name  Latitude	Longitude	pop	ho_size	EI_SubArea	PLN_Cabang	PROVINSI	XY_Source	target_household_penetration_rate	full_population
#   #   1	530102100599	-9.7	119	1791	5.4	AreaSumba	AreaSumba	NUSA TENGGARA TIMUR	BPS_Centroids	0.99	1806
#   #   2	530102100601	-9.8	119	528	3.8	AreaSumba	AreaSumba	NUSA TENGGARA TIMUR	BIG_Settlements	1.00	528
#   
#   #Sample 'lines' variable looks like such
#   #   long	lat	order	piece	group	id
#   #   1	128	2.05	1	1	0.1	0
#   #   2	128	2.05	2	1	0.1	0
#   #   3	128	2.05	3	1	0.1	0
#   
#   #subset out points of interest
#   points <- points_full[c("Longitude","Latitude")]
#   
#   useful_existing <- as.matrix(lines[c("long","lat")])
#   
#   #establish vector to fill in later
#   distance_to_nearest_line <- NULL 
#   
#   for (i in 1:dim(points)[1]){
#     
#     nearest_dist <- min(spDistsN1(useful_existing,
#                                   as.numeric(points[i,]),
#                                   longlat = TRUE))
#     distance_to_nearest_line[i] <- nearest_dist
#   }
#   points_full$distance_to_nearest_line <- distance_to_nearest_line
#   
#   return(points_full)
# }