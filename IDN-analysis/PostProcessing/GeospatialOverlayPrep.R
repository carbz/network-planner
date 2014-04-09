#April 7, 2014
#Vijay wants to wrap up core messages from IDN work in a Geospatial Overlay

require(WriteXLS)
require(ggplot2)
require(plyr)

#metrics_local <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-metrics-local-lite.csv')
metrics_local <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-metrics-local-lite-V2.csv')
rollout <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/ALL-networks-proposed-with-rollout.csv')

#combining sequenced grid settlements and non-grid points all together
all_settlements <- merge(metrics_local, rollout, by = 'Name',all=T)

#settlements outside 1km buffer
all_settlements_1kmbuffer <- subset(all_settlements, Metric...System == 'grid')



#Order by Cabang then sequence
all_settlements <- all_settlements_1kmbuffer[with(all_settlements_1kmbuffer, order(Pln_cabang, seq_fs)),]

#Assign Scenario tag each settlement
all_settlements$scenario <- as.character(all_settlements$Pln_cabang)
replacements <- as.character(all_settlements$Ei_subarea[which(all_settlements$Pln_cabang == "Cabang Tual")])
all_settlements$scenario[which(all_settlements$Pln_cabang == "Cabang Tual")] <- replacements


#Recalculate Cumulative Averages
all_settlements$CumulativeHH <- NA
all_settlements$CumulativeMV <- NA
Cabangs <- unique(all_settlements$Pln_cabang)
EI_areas <- unique(all_settlements$Ei_subarea)
scenarios <- unique(all_settlements$scenario)

all_settlements$dist.N.19.11[which(is.na(all_settlements$dist))] <- 0 

for (j in 1:length(scenarios)){
  #define cumulative household
  all_settlements$CumulativeHH[which(all_settlements$scenario == scenarios[j])] <-
    cumsum(all_settlements$Demand..household....Target.household.count[which(all_settlements$scenario == scenarios[j])])
  #define cumulative MV installed
  all_settlements$CumulativeMV[which(all_settlements$scenario == scenarios[j])] <- 
    cumsum(all_settlements$dist.N.19.11[which(all_settlements$scenario == scenarios[j])])
  
}
all_settlements <- mutate(all_settlements, averageMVperHH =CumulativeMV/CumulativeHH,
                          MVperHH = dist.N.19.11/Demand..household....Target.household.count,
                          averageMVperHH_old = Cumulative/Cumulati_1)
#Improve Labeling
all_settlements$scenario <- as.character(all_settlements$scenario)
all_settlements$scenario[which(all_settlements$scenario == 'AMBON')] <- 'Ambon'
all_settlements$scenario[which(all_settlements$scenario == 'Area Kupan')] <- 'Kupang'
all_settlements$scenario[which(all_settlements$scenario == 'AreaSumba')] <- 'Sumba'
all_settlements$scenario[which(all_settlements$scenario == 'FloresBara')] <- 'Flores Barat'
all_settlements$scenario[which(all_settlements$scenario == 'FloresTimu')] <- 'Flores Timur'

#Write for XLS
WriteXLS("all_settlements","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-for-ElectrificationV20140408.xls")
## 

h = 12#c(12,15,18) # Y intercept Cutoff

plot_pln_cabangs <- 
  ggplot(data= all_settlements, 
         aes(x=PercentOfN.N.19.15, 
             y=averageMVperHH, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Per Household", 
       x = "Percent of New Households Connected [%]", 
       y="MV Line [m/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) + 
  geom_hline(yintercept=c(h), colour="#636363", linetype="dashed") #+
  #geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs

plot_pln_cabangs_cost <- 
  ggplot(data= all_settlements, 
         aes(x=PercentOfN.N.19.15, 
             y=averageMVperHH*30, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Cost Per Household", 
       x = "Percent of New Households Connected [%]", 
       y="MV Line Cost per Household [$USD/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(0,1), #x=0=left, y=1=top
        legend.justification=c(0,1)) + 
  geom_hline(yintercept=c(h*30), colour="#636363", linetype="dashed") #+
#geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs_cost

plot_pln_cabangs_HH <- 
  ggplot(data= all_settlements, 
         aes(x=CumulativeHH, 
             y=averageMVperHH, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Per Household", 
       x = "Number of Households Connected [qty]", 
       y="MV Line [m/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(1,1), #x=0=left, y=1=top
        legend.justification=c(1,1)) + 
  geom_hline(yintercept=c(h), colour="#636363", linetype="dashed") #+
#geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs_HH

plot_pln_cabangs_HHcost <- 
  ggplot(data= all_settlements, 
         aes(x=CumulativeHH, 
             y=averageMVperHH*30, 
             colour = scenario)) +
  geom_line(size =3) +
  labs(title = "Cumulative Average of MV Line Cost Per Household", 
       x = "Percent of New Households Connected [%]", 
       y="MV Line Cost per Household [$USD/HH]", 
       colour = "Geospatial Modeling Area") +
  theme(text=element_text(size=40),
        legend.text = element_text(size=30),
        axis.text = element_text(size=20),
        axis.ticks=element_blank(), 
        panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position=c(1,1), #x=0=left, y=1=top
        legend.justification=c(1,1)) + 
  geom_hline(yintercept=c(h*30), colour="#636363", linetype="dashed") 
#geom_text(aes(0,h,label = h, vjust = -1))
plot_pln_cabangs_HHcost



#Determine the geospatial overlay categorization

for (j in 1:length(scenarios)){
  #Seqence spots more than 
  
  all_settlements[which(all_settlements$scenario == scenarios[j]),] <- 
    mutate(all_settlements[which(all_settlements$scenario == scenarios[j]),],
           
           
           
           )
  
  
  for (j in 1:length(scenarios)){
    #define cumulative household
    all_settlements$CumulativeHH[
      which(all_settlements$scenario == scenarios[j])[
        which]
      ]
  }
  
  
  all_settlements$seq_fs[which(all_settlements$averageMVperHH_old >12 == scenarios[j])] <-
    cumsum(all_settlements$Demand..household....Target.household.count[which(all_settlements$scenario == scenarios[j])])
  #define cumulative MV installed
  all_settlements$CumulativeMV[which(all_settlements$scenario == scenarios[j])] <- 
    cumsum(all_settlements$dist.N.19.11[which(all_settlements$scenario == scenarios[j])])
  
}


#Load categorized 
grid_ranked <- read.csv('~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-for-ElectrificationV20140409.csv')

#combining sequenced grid settlements and non-grid points all together
nongrid_settlements <- merge(metrics_local, rollout, by = 'Name',all=T)
nongrid_settlements <- subset(nongrid_settlements, Metric...System != 'grid')

#Combine nongrid and on-grid all together
all_settlements <- rbind.fill(grid_ranked, nongrid_settlements)

NonGridFields <- c("Geospatial.Overlay...15m.Threshold",
                   "Geospatial.Overlay...10m.Threshold",         
                   "Geospatial.Overlay...12m.Threshold",
                   "Geospatial.Overlay...18m.Threshold")   

#Define phases as a factor to better mesh in the NAs when applicable 
all_settlements$Phase <- as.factor(all_settlements$Phase)

new_factors <- c(levels(all_settlements$Geospatial.Overlay...15m.Threshold),levels(all_settlements$Phase)) 

levels(all_settlements$Geospatial.Overlay...15m.Threshold) <- new_factors
levels(all_settlements$Geospatial.Overlay...10m.Threshold) <- new_factors
levels(all_settlements$Geospatial.Overlay...12m.Threshold) <- new_factors
levels(all_settlements$Geospatial.Overlay...18m.Threshold) <- new_factors

#Replace NAs with original phase categorization 
replacements<-  all_settlements[which(is.na(all_settlements$Geospatial.Overlay...15m.Threshold)),'Phase']        
all_settlements[which(is.na(all_settlements$Geospatial.Overlay...15m.Threshold)),NonGridFields] <- replacements

#Output Singular Desa Field
all_settlements$Desa <- substr(all_settlements$Name,1,10)

all_settlements <- all_settlements[with(all_settlements,order(seq_fs,Phase.C.5)),]
allsettlements.desas <- ddply(all_settlements, c("Desa"), subset, Name==(Name)[1])

WriteXLS("allsettlements.desas","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Desas-CategorizedForStrategicGuidance-V20140409V2.xls")
write.csv(allsettlements.desas,"~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Desas-CategorizedForStrategicGuidance-V20140409V2.csv")

WriteXLS("all_settlements","~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-CategorizedForStrategicGuidance-V20140409.xls")
write.csv(all_settlements,"~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/April 2014/All-Settlements-CategorizedForStrategicGuidance-V20140409.csv")


#bad
Figure29 + geom_line(aes(group=factor(tradlib),size=2)) + facet_grid(regionsFull~., scales="free_y", labeller=reg_labeller) + scale_colour_brewer(type = "div") +
  theme(axis.text.x = element_text(colour = 'black', angle = 90, size = 13, hjust = 0.5, vjust = 0.5),axis.title.x=element_blank()) + 
  ylab("FSI (%Change)") + theme(axis.text.y = element_text(colour = 'black', size = 12), axis.title.y = element_text(size = 12, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, face = 'bold'))

#good
Figure29 + geom_line(aes(group=factor(tradlib)),size=1) + facet_grid(regionsFull~., scales="free_y") + scale_colour_brewer(type = "div") +
  theme(axis.text.x = element_text(colour = 'black', angle = 90, size = 13, hjust = 0.5, vjust = 0.5),axis.title.x=element_blank()) + 
  ylab("FSI (%Change)") + theme(axis.text.y = element_text(colour = 'black', size = 12), axis.title.y = element_text(size = 12, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, face = 'bold'))


Figure29 <- ggplot(data=fsi.wht, aes(x=factor(sres),y=value,colour=factor(tradlib)))

Figure29 + geom_line(aes(group=factor(tradlib),size=2)) + facet_grid(regionsFull~., scales="free_y", labeller=reg_labeller) + scale_colour_brewer(type = "div") +
  theme(axis.text.x = element_text(colour = 'black', angle = 90, size = 13, hjust = 0.5, vjust = 0.5),axis.title.x=element_blank()) + 
  ylab("FSI (%Change)") + theme(axis.text.y = element_text(colour = 'black', size = 12), axis.title.y = element_text(size = 12, hjust = 0.5, vjust = 0.2)) + 
  theme(strip.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, face = 'bold'))

