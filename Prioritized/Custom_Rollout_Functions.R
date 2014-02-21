#libraries that all may or may not be used in script
library(plyr)
library(ggplot2)
library(sp)
library(rgeos)
library(geosphere)
library(maptools)
library(stringr)
library(PBSmapping)
require(gdata)


#DOWNSTREAM.SUM.CALCULATOR IS A FUNCTION THAT APPENDS CUMMULATIVE DOWNSTREAM VALUES FOR MV DISTANCE
#AND KWH CONSUMED.  
# INPUTS: (1) Greedy.Grid is a dataframe with unqiue "branch" variable defined for the network structure, "dist"
#          and a "Demand...Projected.nodal.demand.per.year" for kWh consumption
#Sample input of GREEDY.GRID dataframe
#Settlement.id  long  lat	Name	Metric...System	Demographics...Projected.household.count	Demand..household....Target.household.count	Demand...Projected.nodal.demand.per.year	System..grid....Transformer.cost	Demographics...Population.count	Demographics...Projected.population.count	order	piece	group	id	fake.node	root	branch	dist	MV.line.per.kwh	sequence	depth
# 1	924	40.18	-10.27	Mtwara	grid	0	0	4452249.9	0	0	0	1	1	233.1	233	FALSE	2	2	38174.695	0.0085742480	1	0
#OUTPUT has 2 new columns added "Total.Downstream.Demand.kWh" & "Total.Downstream.Network.Extent.m"
#Settlement.id  long	lat	Name	Metric...System	Demographics...Projected.household.count	Demand..household....Target.household.count	Demand...Projected.nodal.demand.per.year	System..grid....Transformer.cost	Demographics...Population.count	Demographics...Projected.population.count	order	piece	group	id	fake.node	root	branch	dist	MV.line.per.kwh	sequence	depth	Total.Downstream.Demand.kWh	Total.Downstream.Network.Extent.m	
#1	768	17.98	-32.90	Vredenburg	grid	0	0	2143619.1	0	0	0	2	1	433.1	433	FALSE	1	1-1-1-1-1-1-1-2-1-1-1-1-2-1-2-2-1-2-1-1-1-1-1-1-1-1-1-1-2-1-1-1-1-1-1-1-2-1-1-1-1-1-2-1-1-1-1-1-2-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-2-2-1-1-1-1-2-1-1-1-1-1-1-1-1-2-1-1-2-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-2-1-1-1-1	13090.933	0.0061069306	647	108	2143619.1	13090.933
downstream.sum.calculator <- function(greedy.grid)
{
  greedy.grid$depth <- str_count(greedy.grid$branch, "-")
    
  #Arrange this sorted and branch ID'd grid by depth
  gg <- arrange(greedy.grid, desc(greedy.grid$depth))
  
  #future Indexing will be done by the branch ID 
  rownames(gg) <-gg$branch
  
  #Establish downstream variables of interest starting with 0 values
  gg$Total.Downstream.Demand.kWh <- 0
  gg$Total.Downstream.Network.Extent.m <- 0
  
  for (i in 1:nrow(gg)) 
  {
    #Finalize the cummulative network sums now for "i"
    gg[i, "Total.Downstream.Demand.kWh"] <- (gg[i, "Total.Downstream.Demand.kWh"] + 
                                               gg[i, "Demand...Projected.nodal.demand.per.year"])
    
    gg[i, "Total.Downstream.Network.Extent.m"] <- (gg[i, "dist"] + 
                                                     gg[i, "Total.Downstream.Network.Extent.m"])
    
    #Locate the parent of "i" and attribute length and demand values to him
    child_branch <- gg[i, "branch"]
    dash.locations <- as.data.frame(str_locate_all(child_branch, "-"))
    
    if (dim(dash.locations)[1]>0)#Check to see if "i" is a child node
    {
      end.dash.location <- as.integer(tail(dash.locations,1)[1])
      
      #determine parent_branch ID row 
      parent_branch <- str_sub(child_branch, end = end.dash.location -1)
      
      #Attribute the child's "i" cummulative network sums to the respective parent
      gg[parent_branch, "Total.Downstream.Demand.kWh"] <- (gg[i, "Total.Downstream.Demand.kWh"] + 
                                                             gg[parent_branch, "Total.Downstream.Demand.kWh"])
      
      gg[parent_branch, "Total.Downstream.Network.Extent.m"] <- (gg[i, "Total.Downstream.Network.Extent.m"] + 
                                                                   gg[parent_branch, "Total.Downstream.Network.Extent.m"])
    }
  }
  
  #HACK: remove row that is added of all NA values
  gg <- gg[!is.na(gg[,"depth"]),] 
  
  #Output a greedy grid now with cummulative downstream demand & distances for all ranked nodes
  gg <- arrange(gg, sequence)
  
  return(gg)
}