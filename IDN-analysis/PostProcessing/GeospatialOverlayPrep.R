#April 7, 2014
#Vijay wants to wrap up core messages from IDN work in a Geospatial Overlay

require(WriteXLS)
require(ggplot2)
require(plyr)

metrics_local <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-metrics-local-lite.csv')
rollout <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-networks-proposed-with-rollout.csv')

#combining sequenced grid settlements and non-grid points all together
all_settlements <- merge(metrics_local, rollout, by = 'Name',all=T)

#settlements outside 1km buffer
all_settlements_1kmbuffer <- subset(all_settlements, Metric...System == 'grid')

#Order by Cabang then sequence
all_settlements <- all_settlements_1kmbuffer[with(all_settlements_1kmbuffer, order(Pln_cabang, seq_fs)),]

#Recalculate Cumulative Averages
all_settlements$CumulativeHH <- NA
all_settlements$CumulativeMV <- NA
Cabangs <- unique(all_settlements$Pln_cabang)

all_settlements$dist.N.19.11[which(is.na(all_settlements$dist.N.19.11))] <- 0 


for (j in 1:length(Cabangs)){
#define cumulative household
  all_settlements$CumulativeHH[which(all_settlements$Pln_cabang == Cabangs[j])] <-
    cumsum(all_settlements$Demographics...Projected.household.count[which(all_settlements$Pln_cabang == Cabangs[j])])
  #define cumulative MV installed
  all_settlements$CumulativeMV[which(all_settlements$Pln_cabang == Cabangs[j])] <- 
    cumsum(all_settlements$dist.N.19.11[which(all_settlements$Pln_cabang == Cabangs[j])])
  
}
all_settlements <- mutate(all_settlements, averageMVperHH =CumulativeMV/CumulativeHH)

#Write for XLS
WriteXLS("all_settlements_1kmbuffer","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-for-Electrification.xls")
# # 

h = c(12,15,18) # Y intercept Cutoff

plot_pln_cabangs <- 
  ggplot(data= all_settlements, 
         aes(x=PercentOfN.N.19.15, 
             y=averageMVperHH, 
             colour = Pln_cabang)) +
  geom_line() +
  labs(title = "MV Line Per Household", 
       x = "Percent of New Households Connected [%]", 
       y="Cumulative Moving Average of MV Line [m/HH]", 
       colour = "PLN Cabang") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) + 
  geom_hline(yintercept=c(h,15,18), colour="#636363", linetype="dashed") + 
  geom_text(aes(0.25, h, label = h, vjust = -1), size = 12)