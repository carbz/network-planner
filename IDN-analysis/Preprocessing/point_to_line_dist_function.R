
require(sp)
require(maptools)
require(plyr)
#Given a points matrix with Longitude and Latitude fields and
#a foritifed line file with "long" and "lat" values specified
#this function returns a minimum the points file but with an additional variable denoting the 
#minimum distance to a vertex on the line considered 

minimum_distance_to_line_vertices <- function(points_full, lines){
  #'points_full' variable looks like such:
  #   Name  Latitude	Longitude	pop	ho_size	EI_SubArea	PLN_Cabang	PROVINSI	XY_Source	target_household_penetration_rate	full_population
  #   1	530102100599	-9.7	119	1791	5.4	AreaSumba	AreaSumba	NUSA TENGGARA TIMUR	BPS_Centroids	0.99	1806
  #   2	530102100601	-9.8	119	528	3.8	AreaSumba	AreaSumba	NUSA TENGGARA TIMUR	BIG_Settlements	1.00	528
  
  #Sample 'lines' variable looks like such
  #   long	lat	order	piece	group	id
  #   1	128	2.05	1	1	0.1	0
  #   2	128	2.05	2	1	0.1	0
  #   3	128	2.05	3	1	0.1	0
  
  #subset out points of interest
  points <- points_full[c("Longitude","Latitude")]
  
  useful_existing <- as.matrix(lines[c("long","lat")])
  
  #establish vector to fill in later
  distance_to_nearest_line <- NULL 
  
  for (i in 1:dim(points)[1]){
    
    nearest_dist <- min(spDistsN1(useful_existing,
                                  as.numeric(points[i,]),
                                  longlat = TRUE))
    distance_to_nearest_line[i] <- nearest_dist
  }
  points_full$distance_to_nearest_line <- distance_to_nearest_line
  
  return(points_full)
}