## 9/29/14
##quickly scan the header rows of the NIG population data

#V1 = State
#v2 = LGA
#V3 = wards (?) 
#V4 = polling unit  , https://nigeriadecide.org/polling_unit_category.php?state=Sokoto&lga=Tambuwal&ward=Jabo/kagara
#V6 = polling Polling Unit code


#sample: nig <- read.csv('~/Downloads/sad_aa.csv')
unique_files <- (list.files(path = '~/Dropbox/sad/'))

headers <- as.data.frame(NULL)
header <- as.data.frame(read.csv(paste0('~/Dropbox/sad/',unique_files[1]),
                   nrows = 1))

year_max <- 0
year_min <- 0
row_count <- 0
data <- as.data.frame(NULL)
for (i in 1:length(unique_files)){
  
  header <- as.data.frame(read.csv(paste0('~/Dropbox/sad/',unique_files[i]),
                                   header = FALSE))
                                   #nrows = 100 #only want first row
  temp <- ddply(header, .(V3), summarize,
                State = header$V1[1],
                LGA = header$V2[1],
                Ward = header$V3[1],
                Polling_unit = header$V4[1],
                PU_code = header$V6[1],
                voters_total = nrow(header),
                voters_males = nrow(header[which(header$V13=='MALE')]),
                voters_females = nrow(header[which(header$V13=='FEMALE')]))
  data <- rbind.fill(temp, data)
  
#   row_count <- dim(header)[1] + row_count
#   print(names(header))
#   print(dim(header))  
}

data <- ddply(header, .(V3), summarize,
              State = header$V1[1],
              LGA = header$V2[1],
              Ward = header$V3[1],
              Polling_unit = header$V4[1],
              PU_code = header$V6[1],
              total_voters = nrow(header),
              males = nrow(header[which(header$V13=='MALE')]),
              females = nrow(header[which(header$V13=='FEMALE')]))
              
              
              ) 
