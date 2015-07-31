#May 1, 2015
#Example networkplanner.R library use to develop a basic sequence of NetworkPlanner outputs...
#Sustainable Engineering Lab


# #If needed, one time install of below commented out code is needed
# install.packages('devtools')
# library(devtools)
# install_github("SEL-Columbia/networkplanner.R")


#Load the library first for custom functions
library(networkplanner)

# Set the directory containing the output of a Network Planner
# scenario.  Assumes networkplanner directory is on local machine
base_dir <- "~/Dropbox/Nigeria-NEAP-GIS/Scenarios/NewClusteredPU_SetCover500m_All_Kedco/777_780_781/"

# Generate NetworkPlan object with directed igraph of proposed network
# and nodes from Network Planner
np <- read_networkplan(base_dir)

# Sequence the NetworkPlan object via the 'mv_v_dmd_sequence_model'
np <- sequence_plan_far(np, sequence_model=mv_v_dmd_sequence_model)

# Write sequenced NetworkPlan to a directory as nodes (csv) and a network (shp)
write.NetworkPlan(np, base_dir)
