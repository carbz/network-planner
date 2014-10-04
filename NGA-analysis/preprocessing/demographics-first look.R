## 9/29/14
##quickly scan the header rows of the NIG population data

#V1 = State
#v2 = LGA
#V3 = wards (?) 
#V4 = polling unit  , https://nigeriadecide.org/polling_unit_category.php?state=Sokoto&lga=Tambuwal&ward=Jabo/kagara
#V6 = polling Polling Unit code
#V7-9 = NAMEs 1, 2, 3
#V10 = Birth Year
#V11 = Birth Month, V12 = Birth date
#V14 = SEX
#V14 = Address
#V15 = profession
#V16 = binary... (?) 

#sample: nig <- read.csv('~/Downloads/sad_aa.csv')
unique_files <- (list.files(path = '~/Dropbox/Nigeria-NEAP-GIS/Data from Eric -09292014/'))

headers <- as.data.frame(NULL)
header <- as.data.frame(read.csv(paste0('~/Dropbox/sad/',unique_files[1]),
                   nrows = 1))

year_max <- 0
year_min <- 0
row_count <- 0
data <- as.data.frame(NULL)

tabsep <- gsub('"',',','"')
sep <- gsub('\"',',','\"')


last <- as.data.frame(read.csv(paste0('~/Dropbox/Nigsad/tmg_data.csv'),
                                    stringsAsFactors = FALSE,
                                    header = FALSE))
#tabsep <-  '\",\"' # 'invalid 'sep' value: must be one byte'

for (i in 1:length(unique_files)){
  
  header <- as.data.frame(read.csv(paste0('~/Dropbox/sad/',unique_files[i]),
                                   header = FALSE),
                          colClasses="character")
  states <- unique(header$V1)
  kano <- 'ZAMFARA'%in%states
  
                                   #nrows = 100 #only want first row
    header2 <- as.data.frame(read.table(paste0('~/Dropbox/sad/',unique_files[i]),
                                        sep = tabsep,
                                        header = FALSE,
                                        nrows = 31339,
                                        stringsAsFactors = FALSE))
  
  colClass <- c(rep("character", 8),
                  "character",
                  "character","character","character",
                  "character",
                rep("NULL",3),
                'NULL')
                  
                  
                  
                  factor	factor	factor	factor	factor	factor	logical	integer	integer	integer	factor	factor	factor	integer
  
  header4 <- as.data.frame(read.table(paste0('~/Dropbox/sad/sad_ag-edited.csv'),
                                    sep = tabsep,
                                    stringsAsFactors = FALSE,
                                   header = FALSE))
  header4 <- as.data.frame(read.table(paste0('~/Dropbox/sad/sad_ak_mod.csv'),
                                      sep = tabsep,
                                      stringsAsFactors = FALSE,
                                      #nrows = 31339,
                                      header = FALSE))
  
  
  #nrows = 100 #only want first row
  temp2 <- ddply(header, .(V4), summarize,
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
