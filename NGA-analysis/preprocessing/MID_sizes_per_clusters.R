

setwd('~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/')
heavy1 <- read.csv('Buf1/All_points-500m_cluster-aggregations-20150108.csv')
heavy2 <- read.csv('Buf2/All_points-500m_cluster-aggregations-20150109.csv')
heavy3 <- read.csv('Buf3/All_points-500m_cluster-aggregations-20150109.csv')




lite <- read.csv('~/Dropbox/Nigeria-NEAP-GIS/Cluster_vs_PollingUnits_Analysis/Buf1/All_points-50m_cluster-aggregations-20150112.csv')
names(lite)
attach(lite)
hist(compounds_qty,col=1,breaks=2)
hist(compounds_qty,col=1,breaks=2)


hist(mst_length_per_compound_m, col = 311, breaks=22)

hist(mst_length_per_compound_m, 
    breaks= c(0,1,2,4,5,6,7,8,10,20,40))
plot(lite, compounds_qty)


attach(heavy)
plot(compounds_qty,mst_length_per_compound_m)
hist(compounds_qty,col=132,breaks=2)
hist(compounds_qty,col=1,breaks=2)

par(mfrow=c(5,3))

hist(heavy1$mst_length_per_compound_m, 
     col = 31122, 
     breaks=28,
     #breaks=22, 
     main="Region 1\n Heavy Clustering (DBSCAN eps=500m)",
     ylabels =T, xlab='MID [m]',
     xlim=c(0,300))
hist(heavy2$mst_length_per_compound_m, col = 31122, breaks=28,ylabels =T,
     xlab='MID [m]',
     xlim=c(0,300),
     main="Region 2\n Heavy Clustering (DBSCAN eps=500m)")
hist(heavy3$mst_length_per_compound_m, col = 31122, breaks=28,ylabels =T,
     xlab='MID [m]',
     xlim=c(0,300), 
     main="Region 3\n Heavy Clustering (DBSCAN eps=500m)")


##Dist of HH Sizes
hist(heavy1$compounds_qty, 
     col = 31122, 
     breaks=28,
     #breaks=22, 
     main="", 
     xlab='Cluster Size [HHs]',
     xlim=c(0,300))
hist(heavy2$compounds_qty, 
     col = 31122, breaks=28,
     xlab='Cluster Size [HHs]',
     main="")
hist(heavy3$compounds_qty, col = 31122, breaks=28,
     xlab='Cluster Size [HHs]',xlim =c(0,250),
     main="")

attach(heavy1)
scatter.smooth(x=mst_length_per_compound_m,
               y=compounds_qty,
               xlim=c(0,300),ylim=c(0,500),
               xlab='MID [m]', ylab='Settlement Size [HHs]')
attach(heavy2)
scatter.smooth(x=mst_length_per_compound_m,
               y=compounds_qty,
               xlim=c(0,300),ylim=c(0,500),
               xlab='MID [m]', ylab='Settlement Size [HHs]')
attach(heavy3)
scatter.smooth(x=mst_length_per_compound_m,
               y=compounds_qty,
               xlim=c(0,300),ylim=c(0,500),
               xlab='MID [m]', ylab='Settlement Size [HHs]')

#Density Scatterplots
Lab.palette <- colorRampPalette(c("white", "orange", "red"),space='rgb')
xlim=c(0,300)
ylim=c(0,500)
colramp = Lab.palette

attach(heavy1)
smoothScatter(x=mst_length_per_compound_m,
               y=compounds_qty,
              colramp = Lab.palette, xlim = xlim, ylim = ylim,
               xlab='MID [m]', ylab='Settlement Size [HHs]')
attach(heavy2)
smoothScatter(x=mst_length_per_compound_m,
               y=compounds_qty,
              colramp = Lab.palette, xlim = xlim, ylim = ylim,
              xlab='MID [m]', ylab='Settlement Size [HHs]')
attach(heavy3)
smoothScatter(x=mst_length_per_compound_m,
               y=compounds_qty,
              colramp = Lab.palette, xlim = xlim, ylim = ylim,
              xlab='MID [m]', ylab='Settlement Size [HHs]')


##CDFs 

xlim = c(0,400)
heavy1.ecdf = ecdf(heavy1$mst_length_per_compound_m)
plot(heavy1.ecdf, xlab = 'Mean Inter-compound Distances [m]',
     xlim=xlim,
     ylab = '% of Clusters', main = 'Empirical cdf')

heavy2.ecdf = ecdf(heavy2$mst_length_per_compound_m)
plot(heavy2.ecdf, xlab = 'Mean Inter-compound Distances [m]',
     xlim=xlim,
     ylab = '% of Clusters', main = 'Empirical cdf')


heavy3.ecdf = ecdf(heavy3$mst_length_per_compound_m)
plot(heavy3.ecdf, xlab = 'Mean Inter-compound Distances [m]', 
     xlim=xlim,
     ylab = '% of Clusters', main = 'Empirical cdf')





# #Total LV per compound size
# 
# xlim=c(0,20000)
# ylim=c(0,600)
# attach(heavy1)
# scatter.smooth(x=mst_length_m,
#               y=compounds_qty,
#               colramp = Lab.palette,xlim = xlim, ylim = ylim,
#               xlab='Total LV length  [m]', ylab='Settlement Size [HHs]')
# attach(heavy2)
# scatter.smooth(x=mst_length_m,
#               y=compounds_qty,
#               colramp = Lab.palette, xlim = xlim, ylim = ylim,
#               xlab='Total LV length  [m]', ylab='Settlement Size [HHs]')
# attach(heavy3)
# scatter.smooth(x=mst_length_m,
#               y=compounds_qty,
#               colramp = Lab.palette, xlim = xlim, ylim = ylim,
#               xlab='Total LV length [m]', ylab='Settlement Size [HHs]')




 hist(mst_length_per_compound_m, 
     breaks= c(0,1,2,4,5,6,7,8,10,20,40))
