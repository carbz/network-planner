


#Jonathan's Directory 
path_name <-"~/Dropbox/MMR-Training-docs/data/3305/"

#Install fresh package
install.packages('devtools')
library(devtools)
install_github("SEL-Columbia/networkplanner.R")
library(networkplanner)

#Load the library first for custom functions
library(networkplanner)

# Set the directory containing the output of a Network Planner
# scenario.  Assumes networkplanner directory is on local machine
base_dir <- "~/Dropbox/Myanmar_GIS/Modeling/GAD&MIMU_Scenarios_docs/500kWh_Scenarios/160-Chin/"

# Generate NetworkPlan object with directed igraph of proposed network
# and nodes from Network Planner
np <- read_networkplan(path_name)

# Sequence the NetworkPlan object via the 'mv_v_dmd_sequence_model'
np <- sequence_plan_far(np, sequence_model=mv_v_dmd_sequence_model)

# Write sequenced NetworkPlan to a directory as nodes (csv) and a network (shp)
write.NetworkPlan(np, path_name)
