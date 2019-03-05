################################################################################
#
# Cleans raw data from Riverguages.com and puts into a cleaned csv file
#
# Zac Barker, December 2016
#
################################################################################

CLEAN <- function(FlowFile, StageFile, HeaderSize){
     
     # Reads in the two files associate with the gauge
     #flow <- read.xls(FlowFile, sheet = 1, perl = "C:/Perl/bin/perl.exe") # For PC
     #stage <- read.xls(StageFile, sheet = 1, perl = "C:/Perl/bin/perl.exe") # For PC
  
     flow <- read.xls(FlowFile, sheet = 1, perl = "/usr/bin/perl") # Changed for MAC
     stage <- read.xls(StageFile, sheet = 1, perl = "/usr/bin/perl") # Changed For MAC

     # Drop 3rd column
     flow[,3] <- NULL
     stage[,3] <- NULL
     
     # Drop last row which do not contain data
     flow <- head(flow, -1)
     stage <- head(stage, -1)
     
     # Drop the header
     flow <- tail(flow, -HeaderSize)
     stage <- tail(stage, -HeaderSize)
     
     # Formats the time stamps to POSIX
     flow[,1] <- as.character(flow[,1])
     flow[,1] <- substr(flow[,1],1,nchar(flow[,1])-3)
     flow[,1] <- as.POSIXct(flow[,1], format = "%Y-%m-%d %H:%M:%S")
     stage[,1] <- as.character(stage[,1])
     stage[,1] <- substr(stage[,1],1,nchar(stage[,1])-3)
     stage[,1] <- as.POSIXct(stage[,1], format = "%Y-%m-%d %H:%M:%S")
     
     # Convert the flow and stage to numeric
     flow[,2] <- as.numeric(gsub(",", "", flow[,2]))
     stage[,2] <- as.numeric(gsub(",", "", stage[,2]))
     
     names(flow) <- c("DateTime", "Flow")
     names(stage) <- c("DateTime", "Stage")
     
     # Merge flow and stage into one data frame
     df <- merge(flow, stage, by = "DateTime", all = T)
     
     # Rename the headers
     names(df) <- c("DateTime", "Flow", "Stage")
     
     # Adds month column
     df$Month <- months(df$DateTime)
     
     return(df)
     
}

CLEAN_RIO <- function(USGS.Guage.Data){
  
  library(dplyr) # used to mutate date
  library(lubridate) # used to pull out year and months
  
  
  #flow <- read.xls(FlowFile, sheet = 1, perl = "/usr/bin/perl") # Changed for MAC
  #stage <- read.xls(StageFile, sheet = 1, perl = "/usr/bin/perl") # Changed For MAC
  
  # Allows you to work with UTC Timestamp
  USGS.Guage.Data$UTCTimeStamp <- as.POSIXct(USGS.Guage.Data$UTCTimeStamp)
  
  # Seperate 30 years of data,  10-01-1986 to 09-30-2016
  USGS.Guage.Data_30year.i<- subset(USGS.Guage.Data,UTCTimeStamp >= as.POSIXct('1986-10-01 07:00:00') & UTCTimeStamp <= as.POSIXct('2016-09-30 07:00:00') )
  
  # Removes unecessary columns, leaves UTC Time Stamp and Value (Streamflow)
  USGS.Guage.Data_30year <- USGS.Guage.Data_30year.i[,c("UTCTimeStamp","Value")]
  
  # Renames Value to become streamflow
  names(USGS.Guage.Data_30year)[names(USGS.Guage.Data_30year) == 'Value'] <- 'streamflow'
  
  # Adds a column with months and one with years
  USGS.Guage.Data_years_months <- dplyr::mutate(USGS.Guage.Data_30year, year = year(USGS.Guage.Data_30year$UTCTimeStamp),month = month(USGS.Guage.Data_30year$UTCTimeStamp)) # add year and month 
  
  # Reorders the columns
  USGS.Guage.Data_years_months <- USGS.Guage.Data_years_months[c("UTCTimeStamp","year","month","streamflow")]
  
 df <- USGS.Guage.Data_years_months
 df$year <- NULL
 df$month <- NULL
 
 
  
  # Rename the headers
  names(df) <- c("DateTime", "Flow")
  
  # Adds month column
  df$Month <- months(df$DateTime)
  
  return(df)
  
}