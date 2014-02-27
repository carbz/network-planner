#^. TO USE THIS FILE CHANGE THE WORKING DIRECTORY to the scenario of interest

library(gdata)
library(plyr)
library(stringr)
library(data.table)

###^. Comment in/out the appropriate ADDRESS below s.t. setwd("FULL PATH OF YOUR FOLDER") is the folder of interest
##Jonathan's directory
setwd("~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/")
mainDir <- "~/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/January-2014/"

##Edwin's directory
#setwd("C:/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/February-2013/256-HHDem240/")  
#mainDir <- "C:/Dropbox/Indonesia Geospatial Analysis/Data Modeling and Analysis/NPoutputs/R scripts/JC-working"

#Load local metircs data
local <- read.csv("metrics-local.csv", skip=1)

#Replace 0 Population values with NA value, 0 populations were removed clusters
#I do this because populations that are aready grid connected and not under analysis...
#had populations manually set to 0, but NA is a better representative in R
local$Demographics...Projected.household.count[which(local$Demographics...Projected.household.count==0)] <- NA

###Bins by Equal HHs Successful Attempt - Works!
#Sort local dataframe by HHold size
local <- ddply(local, "Demographics...Projected.household.count")
#add new column "HHcumsum" stores the cummulative count of HHolds
local$Demographics...Projected.household.count.HHcumsum <- 
  cumsum(local$Demographics...Projected.household.count)
#add new column "HHBin" and assign bins of 10 parts
bin.count <- 10  #defining number of bins desired
local$Demographics...Projected.household.count.HHbin <- 
  cut(local$Demographics...Projected.household.count.HHcumsum, 
      breaks = seq(0, sum(local$Demographics...Projected.household.count, na.rm=T), 
                   by=sum(local$Demographics...Projected.household.count, na.rm=T)/10)
  )

BinSummaryEqualHH <- ddply(local, .(local$Demographics...Projected.household.count.HHbin), summarize, 
                           Settlement.Size = max(Demographics...Projected.household.count),
                           TOTAL.HHolds = sum(Demographics...Projected.household.count, na.rm=T), 
                           Settlements = nobs(Demographics...Projected.household.count, na.rm=T), 
                           Grid = sum(Demographics...Projected.household.count[which(Metric...System=="grid")]), 
                           Mini.Grid = sum(Demographics...Projected.household.count[which(Metric...System=="mini-grid")]), 
                           SHS = sum(Demographics...Projected.household.count[which(Metric...System=="off-grid")])
)

#output binned summary to new sub-directory 

subDir <- "outputDirectory"
writ

setwd(mainDir)

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

write.csv(phase_summary_perHH, "BinSummaryEqualHH.csv", row.names=F)

##Create Graph summary of output
library(ggplot2)
metrics = ggplot(local, aes(x=Demographics...Projected.household.count.HHbin, 
                            y=Demographics...Projected.household.count, 
                            fill=Metric...System)) + geom_bar()
print(metrics)

qplot(factor(Demographics...Projected.household.count.HHbin), 
      sum(Demographics...Projected.household.count, na.rm=T), 
      data=local, fill=Metric...System, geom_bar()
)

metrics = ggplot(local, aes(x=Demographics...Projected.household.count.HHbin, 
                            fill=Metric...System)) + geom_bar()

metrics2 = ggplot(local, aes(x=Demographics...Projected.household.count.equalbins, 
                            fill=Metric...System)) + geom_bar()



#Replace 0 Population values with NA value, 0 populations were removed clusters
#I do this because populations that are aready grid connected and not under analysis...
#had populations manually set to 0, but NA is a better representative in R
local$Demographics...Projected.household.count[which(local$Demographics...Projected.household.count==0)] <- NA

##Assign bins to original dataset based on fixed predefined thresholds for households/settlement - works
local$Demographics...Projected.household.count.predefinedbin <- 
  cut(local$Demographics...Projected.household.count, 
      c(0, 11, 21, 51, 101, 250, 501, 1000, Inf))
#summarize number of HHolds (sum) and number of settlements (observations) per bin as specified by us
HHoldBins <- ddply(local, .(Demographics...Projected.household.count.predefinedbin), summarize,
                   HHold.Sum = sum(Demographics...Projected.household.count, na.rm=T),
                   Settlements = nobs(Demographics...Projected.household.count, na.rm=T),
                   Grid = sum(Demographics...Projected.household.count[which(Metric...System=="grid")]), 
                   Mini.Grid = sum(Demographics...Projected.household.count[which(Metric...System=="mini-grid")]), 
                   SHS = sum(Demographics...Projected.household.count[which(Metric...System=="off-grid")])
)

###summarize number of settlements in bins sized by equal number of Settlements-works
#Determine HHsize/settlement breaks that split settlements into specified percentages
HHoldBinsEqualSettlementQty <- quantile(local$Demographics...Projected.household.count, 
                                        probs = c(0, .2, .4, .6, .8, 1), na.rm=T)#break settlements into quantiles @ 20, 40, 60, 80 & 100%
#Determine Settlement Bins                        
local$Demographics...Projected.household.count.SettlementBin <- 
  cut(local$Demographics...Projected.household.count, HHoldBinsEqualSettlementQty, include.lowest = TRUE)

EqualBins <- ddply(local, .(Demographics...Projected.household.count.SettlementBin), summarize, 
                   Settlements = nobs(Demographics...Projected.household.count, na.rm=T), 
                   HHold.Sum = sum(Demographics...Projected.household.count, na.rm=T))

##using cut function to divide values increments that are 10 equal parts based on the max value(HHolds per settlement)
##works but not too useful as high outlier throws off bins
##most community sizes are small and end up being in first bin
local$Demographics...Projected.household.count.equalbins <- 
  cut(local$Demographics...Projected.household.count, 
      10, include.lowest = TRUE)
BinSummaryEqualSettlements <- ddply(local, .(Demographics...Projected.household.count.equalbins), summarize,
                                    HHold.Sum = sum(Demographics...Projected.household.count, na.rm=T),
                                    Settlements = nobs(Demographics...Projected.household.count, na.rm=T)
)

###Bins by Equal HHs Successful Attempt - Works!
#Sort local dataframe by HHold size
local <- ddply(local, "Demographics...Projected.household.count")
#add new column "HHcumsum" stores the cummulative count of HHolds
local$Demographics...Projected.household.count.HHcumsum <- 
  cumsum(local$Demographics...Projected.household.count)
#add new column "HHBin" and assign bins of 10 parts
bin.count <- 10  #defining number of bins desired
local$Demographics...Projected.household.count.HHbin <- 
  cut(local$Demographics...Projected.household.count.HHcumsum, 
      breaks = seq(0, sum(local$Demographics...Projected.household.count, na.rm=T), 
                   by=sum(local$Demographics...Projected.household.count, na.rm=T)/10)
  )

BinSummaryEqualHH <- ddply(local, .(local$Demographics...Projected.household.count.HHbin), summarize, 
                           Hold.Sum = sum(Demand..household....Target.household.count, na.rm=T),
                           Settlements = nobs(Demand..household....Target.household.count, na.rm=T), 
                           Grid = sum(Demographics...Projected.household.count[which(Metric...System=="grid")]), 
                           Mini.Grid = sum(Demographics...Projected.household.count[which(Metric...System=="mini-grid")]), 
                           SHS = sum(Demographics...Projected.household.count[which(Metric...System=="off-grid")])
)

ggplot() + 
  geom_bar(data = local, aes(x=Demographics...Projected.household.count.SettlementBin, 
                             fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank()) +
  labs(title = "Households per Settlement", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")


HouseHoldBinChart <- function(local, bin=factor('Demographics...Projected.household.count.SettlementBin'))  {
  ggplot() + 
  geom_bar(data = local, aes(x=bin, fill=Metric...System)) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#abdda4", "#ffffbf"), labels=c("Grid", "Mini Grid", "Off Grid", "Pre-electrified")) +
  theme(axis.ticks=element_blank(), panel.grid=element_blank(),panel.background=element_blank())}

HouseHoldBinChart(local, Demographics...Projected.household.count.SettlementBin)
  labs(title = "Households per Settlement", x = "Desnity Bin of Households/Settlement", y="Number of Settlements", color = "Electrification Tech.")
  
  
###summarize number of HHolds (sum) and number of settlements (AKA observations/nobs) per bin as was originally done in Excel for scenario 230
###still needs work if want cross-comparison... 
#Specify the Bin categories of ClustHouseholds(Edwin did in original 230 analysis) 
local$Clusthhold.bin <- 
  cut(local$Demand..household....Target.household.count, c(1, 10, 20, 50, 100, 250, 500, 1000, Inf))
ClusthholdBins <- ddply(local, .(local$Clusthhold.bin), summarize,
                        HHold.Sum = sum(Demand..household....Target.household.count, na.rm=T),
                        Settlements = nobs(Demand..household....Target.household.count, na.rm=T)
)

