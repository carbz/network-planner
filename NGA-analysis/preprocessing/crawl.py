
def SetClustering(inputShapeFile, outputDir, coverDist):
    '''
        Calculate the SetCover clustering of an input node shapefile
        
        INPUT: Function takes 3 input parameters from user
        (1)Input (node) ShapeFile, ex: r'/Users/carbz/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/All_points_final-corrected20150130.shp'
        must have a X, Y, ID, pop and name field define
        (2)Output Director, ex: r'/Users/carbz/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/clustering-set_cover/'         '
        (3)Set Cover Search Radius in meters, ex: 500 # specified in meters as numeric
        
        OUTPUT: a csv of all input nodes but with new attribute categorizing the central cluster
        '''
    
    tick = time.time()
    
    #Read the big file in
    nodes=generateNodeDictFromShp(inputShapeFile,outputDir)
    
    #adjacency matrix essentially...
    nodesByClusterID=generateClusterDicts(nodes,coverDist)
    
    # List of header fields
    fieldnames = ['cluster_id', 'central_node', 'x', 'y', 'node_id', 'weight_households']
    
    #meaningful output directory and name
    statsFile= (outputDir + os.sep +
                'All_Points-SetCovering-search_radius_'+
                str(coverDist)+
                'm.csv')
        
        
    with open(statsFile, 'w') as csvfile:
                    
        writer = csv.writer(csvfile)
        #Writing header row with column names to the csv output
        writer.writerow(fieldnames)
                            
        i=0
        while nodesByClusterID:
                                    
        #     for cluster in nodesByClusterID:
            cluster_id = i
            maxID=findTheBiggestCluster(nodesByClusterID)
                                            
            for house in nodesByClusterID[maxID]:
                                                
            # extract hh properties
                x = house.getX()
                y = house.getY()
                name = house.getName()
                weight = house.getWeight()
                                                                
                #write data to csv row
                writer.writerow([cluster_id, maxID, x, y, name, weight])
                    
                #once households are written, remove them from node set
                nodesByClusterID=updateDicts(nodesByClusterID,maxID)
                i += 1
                                                                    
tock = time.time()
    run_time = tock - tick
    print "Total Running Time:", run_time
#     return()


'''
    ~~~~~~Someday....~~~~~~~~~~~~~
    I would like to have Selin's code output be a straight pandasdateframe.
    Input: (i) pd dataframe or nodal shpaefile with n points & (ii) search radius in m
    Output: pd dataframe of nodes grouped by cluster with n points
    
    starting block code I'd use is below... 
    '''
# columns = ['cluster_gis','x', 'y']
# df = pd.DataFrame(columns=columns)

# clusters = haversine_cluster(X, 75, 1)

# i=0
# for data in clusters:
#     cluster_gis = data[0]
#     x = data[1][0]
#     y = data[1][1]
#     #print cluster_gis, x, y

#     summary = pd.Series({'cluster_gis':cluster_gis, 
#                          'x':x, 
#                          'y':y})
#     df.loc[i] = summary
#     i=i+1
