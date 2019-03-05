# Clean Data
# Script includes function to add consumptions scenarios
# Script includes function to melt the data for plotting purposes

# Load required libraries
library(dplyr)
library(lubridate)

# Cleans USGS streamgauge data from 10/01/1986 untill 09/30/2016
Clean_Data_30yr <- function(USGS.Guage.Data) {
  
  # Import necessary libraries
  library(dplyr) # used to mutate date
  library(lubridate) # used to pull out year and months
  
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
  
  #Outputs cleaned data frame
  Clean_Data_30yr <- USGS.Guage.Data_years_months
  return(Clean_Data_30yr)
}

# Only used for the Rio Grande Assessment
Clean_Data_40yr <- function(USGS.Guage.Data) {
  
  # Import necessary libraries
  library(dplyr) # used to mutate date
  library(lubridate) # used to pull out year and months
  
  # Allows you to work with UTC Timestamp
  USGS.Guage.Data$UTCTimeStamp <- as.POSIXct(USGS.Guage.Data$UTCTimeStamp)
  
  # Seperate 30 years of data,  10-01-1986 to 09-30-2016
  USGS.Guage.Data_30year.i<- subset(USGS.Guage.Data,UTCTimeStamp >= as.POSIXct('1976-01-01 07:00:00') & UTCTimeStamp <= as.POSIXct('2016-12-31 07:00:00') )
  
  # Removes unecessary columns, leaves UTC Time Stamp and Value (Streamflow)
  USGS.Guage.Data_30year <- USGS.Guage.Data_30year.i[,c("UTCTimeStamp","Value")]
  
  # Renames Value to become streamflow
  names(USGS.Guage.Data_30year)[names(USGS.Guage.Data_30year) == 'Value'] <- 'streamflow'
  
  # Adds a column with months and one with years
  USGS.Guage.Data_years_months <- dplyr::mutate(USGS.Guage.Data_30year, year = year(USGS.Guage.Data_30year$UTCTimeStamp),month = month(USGS.Guage.Data_30year$UTCTimeStamp)) # add year and month 
  
  # Reorders the columns
  USGS.Guage.Data_years_months <- USGS.Guage.Data_years_months[c("UTCTimeStamp","year","month","streamflow")]
  
  #Outputs cleaned data frame
  Clean_Data_40yr <- USGS.Guage.Data_years_months
  return(Clean_Data_40yr)
}

# Cleans and reduces streamlflow depending on scalars
Rio_consumption_scenarios_df <- function(Gauge_Data,Scalars) {
  
  # Requirescleaned functions from above
  
  library (dplyr)
  
  # Clean the streamflow data (Function above)
  x <- Clean_Data_30yr(Gauge_Data)
  
  # Copy the data frame to be edited below
  df <- x
  
  # Calculate number of scalars, number of rows in original dataframe (should be 4), and the number of consumption scenarios
  # Ability to make more generic
  ncs <- 3 #number of consumption scenarios
  n.scalar <- length(Scalars)
  n.rows <- length(names(x))
  
  #Add Uniform Consumption To data Frame
  for(i in 1:n.scalar) {
    df[,n.rows+i] <- x$streamflow-Scalars[i]
    df[,n.rows+i] <- ifelse(df[,n.rows+i] < 0, 0, df[,n.rows+i])
  }
  
  # Loop to add winter consumption
  for(i in 1:n.scalar) {
    df[,n.rows+n.scalar+i] <- transmute(df, x = ifelse(month == 1 | month == 2 | month == 3, streamflow - Scalars[i], streamflow))
    df[,n.rows+n.scalar+i] <- ifelse(df[,n.rows+n.scalar+i] < 0, 0, df[,n.rows+n.scalar+i])
  }
  
  #Loop to add summer consumption
  for(i in 1:n.scalar) {
    df[,n.rows+(2*n.scalar)+ i] <- transmute(df, t = ifelse(month == 6 | month == 7 | month == 8, streamflow - Scalars[i], streamflow))
    df[,n.rows+2*n.scalar+i] <- ifelse(df[,n.rows+2*n.scalar+i] < 0, 0, df[,n.rows+2*n.scalar+i])
  }
  
  # Ability to make more generic
  colnames(df)[5:13] <- c("Uniform_9CFS","Uniform_45CFS","Uniform_90CFS",
                          "Winter_9CFS","Winter_45CFS","Winter_90CFS",
                          "Summer_9CFS","Summer_45CFS","Summer_90CFS")
  
  return(df)
}


# Melt the data into one dataframe for ggplot2
Melt_Rio_Gauges_For_Plotting <- function(Gauge.Names,Data_Frame.1,Data_Frame.2,Data_Frame.3, POF = TRUE) {
  
  # i=1 represents melting for stakeholder metrics 
  # i= 0 represents melting for t-statistic
  i <- ifelse(POF == TRUE,1,0)
  
  x <- Gauge.Names
  
  # Melt Data for 9 CFS
  winter <- c(Data_Frame.1[4+i],Data_Frame.2[4+i],Data_Frame.3[4+i])
  summer <- c(Data_Frame.1[7+i],Data_Frame.2[7+i],Data_Frame.3[7+i])
  uniform <- c(Data_Frame.1[1+i],Data_Frame.2[1+i],Data_Frame.3[1+i])
  
  begin_melt_9CFS <- data.frame(x=x,winter=winter,summer =summer,uniform = uniform)
  melted_9CFS <- melt(begin_melt_9CFS,id ="x")
  
  # Melt data for 45 CFS
  winter <- c(Data_Frame.1[5+i],Data_Frame.2[5+i],Data_Frame.3[5+i])
  summer <- c(Data_Frame.1[8+i],Data_Frame.2[8+i],Data_Frame.3[8+i])
  uniform <- c(Data_Frame.1[2+i],Data_Frame.2[2+i],Data_Frame.3[2+i])
  
  begin_melt_45CFS <- data.frame(x=x,winter=winter,summer =summer,uniform = uniform)
  melted_45CFS <- melt(begin_melt_45CFS,id ="x")
  
  # For 90 CFS
  winter <- c(Data_Frame.1[6+i],Data_Frame.2[6+i],Data_Frame.3[6+i])
  summer <- c(Data_Frame.1[9+i],Data_Frame.2[9+i],Data_Frame.3[9+i])
  uniform <- c(Data_Frame.1[3+i],Data_Frame.2[3+i],Data_Frame.3[3+i])
  
  begin_melt_90CFS <- data.frame(x=x,winter=winter,summer =summer,uniform = uniform)
  melted_90CFS<-melt(begin_melt_90CFS,id ="x")
  
  # Combine melted Scenarios
  melted_together <- rbind(melted_9CFS,melted_45CFS,melted_90CFS)
  
  #CMS 
  Consumption <- c(rep("10% ADF (.255 cms)",length(melted_9CFS$value)),rep("50% ADF (1.28 cms)",length(melted_45CFS$value)),rep("100% ADF (2.55 cms)",length(melted_90CFS$value)))
  melted_together <- cbind(melted_together,Consumption)
  
  # factor the consumption column
  melted_together$Consumption = factor(melted_together$Consumption, levels=c('10% ADF (.255 cms)','50% ADF (1.28 cms)','100% ADF (2.55 cms)'))
  
  # POF requires a percentage, so multiply by 100
  percent<- ifelse(i==T,100,1)
  
  melted_together$value <- percent*melted_together$value # Multiplied by 100 to change from ratio to percent
  
  # Create a column with POF for no consumption 
  if(i==1) {
    Isleta_abline.df <- Rio_consumption_scenarios_df(Isleta,c(0,0,0))
    san_acacia_abline.df <- Rio_consumption_scenarios_df(san_acacia,c(0,0,0))
    san_marcial_abline.df <- Rio_consumption_scenarios_df(san_marcial,c(0,0,0))
    
    Isleta_abline <- Probability_of_Failure_50CFS(Isleta_abline.df)
    san_acacia_abline <- Probability_of_Failure_50CFS(san_acacia_abline.df)
    san_marcial_abline <- Probability_of_Failure_50CFS(san_marcial_abline.df)
    
    # Melt data
    melted_together$noconsumption <- Consumption
    melted_together$noconsumption <- ifelse(melted_together$x =="Isleta",percent*Isleta_abline,melted_together$noconsumption)
    melted_together$noconsumption <- ifelse(melted_together$x == "San Acacia",percent*san_acacia_abline,melted_together$noconsumption)
    melted_together$noconsumption <- ifelse(melted_together$x == "San Marcial",percent*san_marcial_abline,melted_together$noconsumption)
    melted_together$noconsumption <- as.numeric(melted_together$noconsumption)
    
  }
  
  # River miles from consumption to gage
  RM_Below_SWRP <- c(8,61,100)
  
  melted_together$river_mile <- 0
  melted_together$river_mile <- ifelse(melted_together$x =="Isleta",RM_Below_SWRP[1],melted_together$river_mile)
  melted_together$river_mile <- ifelse(melted_together$x =="San Acacia",RM_Below_SWRP[2],melted_together$river_mile)
  melted_together$river_mile <- ifelse(melted_together$x =="San Marcial",RM_Below_SWRP[3],melted_together$river_mile)
  
  return(melted_together)
}




