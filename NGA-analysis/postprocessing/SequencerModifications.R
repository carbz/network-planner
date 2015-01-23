#merge metrics local with ranked grid nodes
shared_column_names <- c("long","lat")
merged <- merge(proposed.fortified, local.grid, by = shared_column_names, all=T)

merged <- merge(proposed.fortified, local.grid, all=T)

##Develop a Unique lat/long based ID to intelligently merge by
local.grid$XYID <- paste0(str_sub(as.character(local.grid$long*100000),end=7L),
                          str_sub(as.character(local.grid$lat*100000),end=7L))
proposed.fortified$XYID <- paste0(str_sub(as.character(proposed.fortified$long*100000),end=7L),
                                  str_sub(as.character(proposed.fortified$lat*100000),end=7L))

##Sort
local.grid <- local.grid[order(local.grid$XYID),]#get grids to the top
proposed.fortified <- proposed.fortified[order(proposed.fortified$XYID),]#get grids to the top


#Now try the merge
merged <- merge(proposed.fortified, local.grid, by = 'XYID', all.x = T)




duplicates <- local_all_orig[which(duplicated(local_all_orig$XYID, 
                                              fromLast=FALSE)),]#minimize grid nodes being removed
uniques <- local_all_orig[which(!(duplicated(local_all_orig$XYID, 
                                             fromLast=FALSE))),]#minimize grid nodes being removed


##Proposed Grid has some repeat nodes

uniques <- ghost.nodes[which(!(duplicated(ghost.nodes$group, 
                                             fromLast=FALSE))),]#minimize grid nodes being removed

i=1
# Build network until candidate_df is exhausted of options
while (nrow(candidate_df) > 0) 
{  
  candidate <- subset(candidate_df, MV.line.per.kwh == min(MV.line.per.kwh))[1,]
  candidate <- mutate(candidate, 
                      sequence = sequence)
  sequence <- sequence + 1
  ## 8.2 remove candidate observation from candidate.nodes
  candidate_df <- subset(candidate_df, !(Settlement.id %in% candidate$Settlement.id))
  ## place candidate in next order of ranked.settlements and assign incremental value for "sequence"
  ranked.settlements <- rbind(ranked.settlements, candidate)
  
  ## 8.3 re-build candidate.nodes by pulling in any settlements that share "group" with candidate observation now in ranked.settlements while assigning it the same "branch.id" as candidate    
  #identify all new lines associated with candidate settlement 
  new.segments <- non_candidate_df$id[which((non_candidate_df$Settlement.id == candidate$Settlement.id))] 
  if (length(new.segments)>0) 
  {
    #creates 12 variable dataframe such that lines connect to candidate node and settlements are not the candidate node themself
    new.candidate.nodes2 <- as.data.frame(subset(non_candidate_df, (id %in% new.segments) &
                                                   (Settlement.id != candidate$Settlement.id)))
    #ensure unique branch labeling scheme is in place showing ancestory 
    #v1 <- 1:length(new.segments)
    v1 <- 1:dim(new.candidate.nodes2)[1]
    new.candidate.nodes2$branch <- str_c(candidate$branch,"-",v1)
    new.candidate.nodes2$root <- (candidate$root)
    
    #remove non_candidate_df that are now candidates 
    non_candidate_df <- subset(non_candidate_df, !(id %in% new.segments))  
    
    #calculate the distance to the new.candidate.nodes
    #^. use the new distance function 
    new.dist <-  dist_fun(candidate[,c("long", "lat")],
                          new.candidate.nodes2[,c("long", "lat")], proj_var) #new distance function by Zaiming!
    
    new.dist <- data.frame(dist = new.dist, Settlement.id = new.candidate.nodes2[,"Settlement.id"])
    
    #makes compatible 15 variable dataframe for candidate.nodes
    new.candidate.nodes2 <- merge(new.candidate.nodes2, new.dist)    
    ##so here is is an issue where 2 disimilar types are being combined to make a list and not a dataframe
    new.candidate.nodes2 <- mutate(new.candidate.nodes2, 
                                   MV.line.per.kwh = dist/Demand...Projected.nodal.demand.per.year
    )                              
    candidate_df <- rbind(candidate_df, new.candidate.nodes2)
    ## 12.4 Return to step 12.1 until merged dataframe is completely sorted into ranked.settlement 
  }
  i=i+1
}  
