################################################################################
#
# Work flow for RW downstream analysis
#
# Brendan Purcell and Zac Barker, March 2019

################################################################################

# Clear global environment
rm(list=ls())

# Load necessary libraries
library("gdata")                             # gdata must have perl library installed

################################ EDIT ########################################
# Load Additional Libraries
library("ggplot2")     
library("reshape2")    
 
setwd("~/Desktop/Downstream_Effects_Purcell/Downstream_Effects_R") # sets working directory as current workspace

# # Load user defined functions
source("CODE/Illinois_River_CleanAndFormat.R")
source("CODE/Illinois_River_Consumption.R")
source("CODE/Illinois_River_FlowDurationCurve.R")
source("CODE/Illinois_River_RatingCurve.R")
source("CODE/Illinois_River_Metrics.R")
source("CODE/Illinois_River_Patterns.R")
source("CODE/Illinois_River_HistogramAndQQ.R")


# Initialize metrics output dataframes
Gauge <- c("Dresden", "Marseilles", "Starved Rock", "Peoria", "La Grange")
Station <- c(271, 245, 231, 158, 80)
tTestTemp <- data.frame(Gauge, Station)
pFailTemp <- data.frame(Gauge, Station)
rLostTemp <- data.frame(Gauge, Station)
rMaxTemp <- data.frame(Gauge, Station)
rMinTemp <- data.frame(Gauge, Station)
LOFTemp <- data.frame(Gauge, Station)
AFMTemp <- data.frame(Gauge, Station)

# Load, clean & format data
setwd("DATA/Illinois_River_Data/")

Dresden <- CLEAN("Dresden_Flow.xls", "Dresden_Stage.xls", 12)
Marseilles <- CLEAN("Marseilles_Flow.xls", "Marseilles_Stage.xls", 17)
StarvedRock <- CLEAN("StarvedRock_Flow.xls", "StarvedRock_Stage.xls", 21)
Peoria <- CLEAN("Peoria_Flow.xls", "Peoria_Stage.xls", 19)
LaGrange <- CLEAN("LaGrange_Flow.xls", "LaGrange_Stage.xls", 21)
LaGrange$Stage <- sapply(LaGrange$Stage, function(x){                           # Guage datum switched during the record
  if(x<=100 && !is.na(x)){
    x+413.5
  }else{x}
})
setwd("../..")

# Remove unreasonable Stage Data for Marseiles and la grange
Marseilles <- Marseilles[!(Marseilles$Stage < 450),] 
LaGrange <- LaGrange[!LaGrange$Stage < 400,]

# Rating curve
s_Dresden <- RATING_CURVE("Dresden", Dresden)
s_Marseilles <- RATING_CURVE("Marseilles", Marseilles)
s_StarvedRock <- RATING_CURVE("Starved Rock", StarvedRock)
s_Peoria <- RATING_CURVE("Peoria", Peoria)
s_LaGrange <- RATING_CURVE("La Grange", LaGrange)

# Load consumption patterns
Patterns <- read.csv("DATA/Illinois_River_Data/ConsumptionPatterns.csv", header = T)
Scalers <- c(0,70,350,700)

# Load values at each gauge
Value <- read.csv("DATA/Illinois_River_Data/ValuePerTon.csv")


# Immersion factor, assuming 15 barges with dimensions 195' x 35'
I = 266


# Get price for each gauge
v_Dresden <- c(Value[which(Value$Lock == "Dresden"),]$Average, 
               Value[which(Value$Lock == "Dresden"),]$Max, 
               Value[which(Value$Lock == "Dresden"),]$Min)
v_Marseilles <- c(Value[which(Value$Lock == "Marseilles"),]$Average, 
                  Value[which(Value$Lock == "Marseilles"),]$Max, 
                  Value[which(Value$Lock == "Marseilles"),]$Min)
v_StarvedRock <- c(Value[which(Value$Lock == "Starved Rock"),]$Average, 
                   Value[which(Value$Lock == "Starved Rock"),]$Max, 
                   Value[which(Value$Lock == "Starved Rock"),]$Min)
v_Peoria <- c(Value[which(Value$Lock == "Peoria"),]$Average, 
              Value[which(Value$Lock == "Peoria"),]$Max, 
              Value[which(Value$Lock == "Peoria"),]$Min)
v_LaGrange <- c(Value[which(Value$Lock == "La Grange"),]$Average, 
                Value[which(Value$Lock == "La Grange"),]$Max, 
                Value[which(Value$Lock == "La Grange"),]$Min)


# #QQ plots and histograms, currently commented out to decrease run time

# my_histogram_qqplots(Dresden$Flow,"Dresden")
# my_histogram_qqplots(LaGrange$Flow,"LaGrange")
# my_histogram_qqplots(Marseilles$Flow,"Marseilles")
# my_histogram_qqplots(Peoria$Flow,"Peoria")
# my_histogram_qqplots(StarvedRock$Flow,"StarvedRock")

# Loop through consumption scaler
for(Scaler in Scalers) {
  
  # Loop through consumption scenarios
  for(i in 2:ncol(Patterns)){
    
    # Isolate a scenario
    Scenario <- data.frame(Patterns$Month, Patterns[,i])
    if(colnames(Patterns)[i] == "Current"){
      Name <- "Current"
    } else {
      Name <- paste0(colnames(Patterns)[i],Scaler)
    }
    colnames(Scenario) <- c("Month", Name)
    
    # Calculate consumption scenario
    Dresden <- CONSUMPTION(Dresden, Scenario, Scaler, s_Dresden, 482.8, I, v_Dresden)
    Marseilles <- CONSUMPTION(Marseilles, Scenario, Scaler, s_Marseilles, 458.5, I, v_Marseilles)
    StarvedRock <- CONSUMPTION(StarvedRock, Scenario, Scaler, s_StarvedRock, 440.3, I, v_StarvedRock)
    Peoria <- CONSUMPTION(Peoria, Scenario, Scaler, s_Peoria, 430.0, I, v_Peoria)
    LaGrange <- CONSUMPTION(LaGrange, Scenario, Scaler, s_LaGrange, 419.6, I, v_LaGrange)
    
    # T test
    t_Dresden <- T_TEST(Dresden, Name)
    t_Marseilles <- T_TEST(Marseilles, Name)
    t_StarvedRock <- T_TEST(StarvedRock, Name)
    t_Peoria <- T_TEST(Peoria, Name)
    t_LaGrange <- T_TEST(LaGrange, Name)
    
    # Probability of failure
    pf_Dresden <- P_FAIL(Dresden, s_Dresden, 482.8, Name)
    pf_Marseilles <- P_FAIL(Marseilles, s_Marseilles, 458.5, Name)
    pf_StarvedRock <- P_FAIL(StarvedRock, s_StarvedRock, 440.3, Name)
    pf_Peoria <- P_FAIL(Peoria, s_Peoria, 430.0, Name)
    pf_LaGrange <- P_FAIL(LaGrange, s_LaGrange, 419.6, Name)
    
    # Length of failure
    LOF_Dresden <- LOF(Dresden, s_Dresden, 482.8, Name)
    LOF_Marseilles <- LOF(Marseilles, s_Marseilles, 458.5, Name)
    LOF_StarvedRock <- LOF(StarvedRock, s_StarvedRock, 440.3, Name)
    LOF_Peoria <- LOF(Peoria, s_Peoria, 430.0, Name)
    LOF_LaGrange <- LOF(LaGrange, s_LaGrange, 419.6, Name)
    
    # Failure Magnitude
    AFM_Dresden <- AFM(Dresden, s_Dresden, 482.8, Name)
    AFM_Marseilles <- AFM(Marseilles, s_Marseilles, 458.5, Name)
    AFM_StarvedRock <- AFM(StarvedRock, s_StarvedRock, 440.3, Name)
    AFM_Peoria <- AFM(Peoria, s_Peoria, 430.0, Name)
    AFM_LaGrange <- AFM(LaGrange, s_LaGrange, 419.6, Name)
    
    
    # Avg value lost, nt used in the latest draft of paper
    c_Dresden <- COST(Dresden, Name, "AvgValue")
    c_Marseilles <- COST(Marseilles, Name, "AvgValue")
    c_StarvedRock <- COST(StarvedRock, Name, "AvgValue")
    c_Peoria <- COST(Peoria, Name, "AvgValue")
    c_LaGrange <- COST(LaGrange, Name, "AvgValue")
    
    # Max value lost, not used in the latest draft of paper
    cMax_Dresden <- COST(Dresden, Name, "MaxValue")
    cMax_Marseilles <- COST(Marseilles, Name, "MaxValue")
    cMax_StarvedRock <- COST(StarvedRock, Name, "MaxValue")
    cMax_Peoria <- COST(Peoria, Name, "MaxValue")
    cMax_LaGrange <- COST(LaGrange, Name, "MaxValue")
    
    # Min value lost, not used in the latest draft of paper
    cMin_Dresden <- COST(Dresden, Name, "MinValue")
    cMin_Marseilles <- COST(Marseilles, Name, "MinValue")
    cMin_StarvedRock <- COST(StarvedRock, Name, "MinValue")
    cMin_Peoria <- COST(Peoria, Name, "MinValue")
    cMin_LaGrange <- COST(LaGrange, Name, "MinValue")
    
    # Metrics output temporary dataframes
    tTestTemp$Scaler <- Scaler
    tTestTemp[,colnames(Patterns)[i]] <- c(t_Dresden,t_Marseilles,t_StarvedRock,t_Peoria,t_LaGrange)
    pFailTemp$Scaler <- Scaler
    pFailTemp[,colnames(Patterns)[i]] <- c(pf_Dresden,pf_Marseilles,pf_StarvedRock,pf_Peoria,pf_LaGrange)
    LOFTemp$Scaler <- Scaler
    LOFTemp[,colnames(Patterns)[i]] <- c(LOF_Dresden,LOF_Marseilles,LOF_StarvedRock,LOF_Peoria,LOF_LaGrange)
    AFMTemp$Scaler <- Scaler
    AFMTemp[,colnames(Patterns)[i]] <- c(AFM_Dresden,AFM_Marseilles,AFM_StarvedRock,AFM_Peoria,AFM_LaGrange)
    rLostTemp$Scaler <- Scaler
    rLostTemp[,colnames(Patterns)[i]] <- c(c_Dresden,c_Marseilles,c_StarvedRock,c_Peoria,c_LaGrange)
    rMaxTemp$Scaler <- Scaler
    rMaxTemp[,colnames(Patterns)[i]] <- c(cMax_Dresden,cMax_Marseilles,cMax_StarvedRock,cMax_Peoria,cMax_LaGrange)
    rMinTemp$Scaler <- Scaler
    rMinTemp[,colnames(Patterns)[i]] <- c(cMin_Dresden,cMin_Marseilles,cMin_StarvedRock,cMin_Peoria,cMin_LaGrange)
    
  }
  
  #Populates the master output dataframes with that scalers runs
  if(exists("tTest")) {
    tTest <- rbind(tTest, tTestTemp)
    pFail <- rbind(pFail, pFailTemp)
    
    lof <- rbind(lof, LOFTemp) # Doesnt work becasue LOF is a function
    afm <- rbind(afm, AFMTemp)
    
    rLost <- rbind(rLost, rLostTemp)
    rMax <- rbind(rMax, rMaxTemp)
    rMin <- rbind(rMin, rMinTemp)
    
  } else {
    tTest <- tTestTemp
    pFail <- pFailTemp
    rLost <- rLostTemp
    rMax <- rMaxTemp
    rMin <- rMinTemp
    lof <- LOFTemp
    afm <- AFMTemp
  }
  
}

# Plot flow duration curves
FDC("Dresden", Dresden)
FDC("Marseilles", Marseilles)
FDC("Starved Rock", StarvedRock)
FDC("Peoria", Peoria)
FDC("La Grange", LaGrange)

# Plot metrics
PLOT_TTEST(tTest)

PLOT_Metrics(pFail,'POF')
PLOT_Metrics(afm,'AFM')
PLOT_Metrics(lof,'LOF')

