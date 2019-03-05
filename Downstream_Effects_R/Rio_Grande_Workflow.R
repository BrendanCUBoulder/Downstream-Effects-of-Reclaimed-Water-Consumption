# Rio Grande Workflow

# Clear global environment
rm(list=ls())

# Read in required libraries
library(tidyr) 
library(reshape2)
library(ggplot2)

#Set Working Directory, this will change depending on what computer you use
setwd("~/Desktop/Downstream_Effects_Purcell/Downstream_Effects_R") # sets working directory as current workspace

# Read in required functions
source("./CODE/Rio_Grande_CleanData.R")
source("./CODE/Rio_Grande_Tstatistic.R")
source("./CODE/Rio_Grande_StakeholderMetrics.R")
source("./CODE/Rio_Grande_CompactAssessment.R")

# Read in data for each gauge
Otowi <- read.csv("./DATA/Rio_Grande_Data/otowi_bridge.csv", skip = 2)
Isleta <- read.csv("./DATA/Rio_Grande_Data/rio_isleta.csv",header=TRUE, skip = 2)
san_acacia <- read.csv("./DATA/Rio_Grande_Data/rio_san_acacia.csv",header=TRUE, skip = 2)
san_marcial <- read.csv("./DATA/Rio_Grande_Data/rio_san_marcial.csv",header=TRUE, skip = 2)

# Determine 3 scalars (in CFS) to reduce streamflow by.
scalars <- c(9,45,90)

# Create a dataframe with cleaned data and reduced flows by scalars
Isleta.df <- Rio_consumption_scenarios_df(Isleta,scalars)
san_acacia.df <- Rio_consumption_scenarios_df(san_acacia,scalars)
san_marcial.df <- Rio_consumption_scenarios_df(san_marcial,scalars)

# Determine t-statistic for each gauge
# Create empty lists to be filled in below
t_isleta <- c()
t_sa <- c()
t_sm <- c()

# Fills in empty lists with t statistics
for( i in 1:(length(names(Isleta.df))-4)) {
  t_isleta[i] <- my_boxcox_and_ztest(Isleta.df$streamflow,Isleta.df[,i+4])
  t_sa[i] <- my_boxcox_and_ztest(san_acacia.df$streamflow,san_acacia.df[,i+4])
  t_sm[i] <- my_boxcox_and_ztest(san_marcial.df$streamflow,san_marcial.df[,i+4])
}

# Check QQ Plots and Histograms, currently edited out for run time
# GOF_Test_Isleta <- my_histogram_qqplots(Isleta.df$streamflow,"Isleta")
# GOF_Test_SanAcacia<- my_histogram_qqplots(san_acacia.df$streamflow,"San Acacia")
# GOF_Test_SanMarcial <- my_histogram_qqplots(san_marcial.df$streamflow, "San Marcial")

# Detrmine ability to meet Compact
# Rio Grande Uniform 90CFS Consumption
Rio_Grande_Compact_35YearAssessment(Otowi,90)

# Plot t statistic
gauges <- c("Isleta","San Acacia","San Marcial")
Melted_tstat <- Melt_Rio_Gauges_For_Plotting(gauges,t_isleta,t_sa,t_sm, POF = F)
PLOT_TSTAT(Melted_tstat)

# Create D dataframe for stakeholder metrics
D_Isleta <- Create_D_dataframe(Isleta.df,50)
D_San_Acacia <- Create_D_dataframe(san_acacia.df,50)
D_San_Marcial <- Create_D_dataframe(san_marcial.df,50)

# Calculate Magnitude of Failure 
VUL_D_Isleta <- VUL_D(D_Isleta,50)
VUL_D_San_Acacia <- VUL_D(D_San_Acacia,50)
VUL_D_San_Marcial <- VUL_D(D_San_Marcial,50)

# Calculate Probability of Failure
POF_D_Isleta <- POF_D(D_Isleta)
POF_D_San_Acacia <- POF_D(D_San_Acacia)
POF_D_San_Marcial <- POF_D(D_San_Marcial)

# Calculate Average Length of Failure
LOF_D_Isleta <- LOF_D(D_Isleta)
LOF_D_San_Acacia <- LOF_D(D_San_Acacia)
LOF_D_San_Marcial <- LOF_D(D_San_Marcial)

# Melt Data for plotting
POF_melt<- Melt_Metric(gauges,POF_D_Isleta,POF_D_San_Acacia,POF_D_San_Marcial,Type = 'POF')
AFD_melt<- Melt_Metric(gauges,LOF_D_Isleta,LOF_D_San_Acacia,LOF_D_San_Marcial,Type = 'LOF')
VUL_melt<- Melt_Metric(gauges,VUL_D_Isleta,VUL_D_San_Acacia,VUL_D_San_Marcial,Type = 'VUL')
melts_combined <- Combine_Melts_AFD(POF_melt,AFD_melt,VUL_melt)

# Plot the Stakeholder merics
PLOT_single_metric(melts_combined,Metric = 'POF')
PLOT_single_metric(melts_combined,Metric = 'VUL')
PLOT_single_metric(melts_combined,Metric = 'AFD')
