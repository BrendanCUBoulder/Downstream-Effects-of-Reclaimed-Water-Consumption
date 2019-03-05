# Rio Grande Metrics

# Load in Required libraries
library(reshape2)
library(ggplot2)

# Determines the propability that the flow at a certain gauge is below 50 CFS
Probability_of_Failure_50CFS <- function(consumption.scenario.dataframe) {
  
  # Rename to df 
  df <- consumption.scenario.dataframe
  
  # Create an empty list to fill in with probability of failure
  pf <- c()
  
  # Determine Probability of failure
  for(i in 1:length(names(df))-3) {
    xx <- df[,3+i]
    pf[i] <- length( which(xx < 50)) / length(xx)
  }
  return(pf)
}

# Employ boxcox transformation on data, and determine the t statistic
# Flow is the observed historical streamflow
# Flow2 is the engineered streamflow
my_boxcox_and_ztest <- function(Flow,Flow2) {
  
  x <- na.exclude(Flow)
  x[x<=0] <- 0
  
  y <- na.exclude(Flow2)
  y[y<=0] <-0
  
  lambda <- .3 # Determined For Rio Gages using Maximum Liklihood Estimation
  lambda2 <- 1 # Adds a constant value of 1 because we have streamflow values of 0
  
  # Trasnsformation
  xtrans <-  ((x + lambda2) ^ lambda - 1) / lambda
  ytrans <-  ((y + lambda2) ^ lambda - 1) / lambda
  
  # Conducts a 2 sample t-test comparing the transformed streamflow data
  x1 <- mean(xtrans,na.rm = T)
  x2 <- mean(ytrans,na.rm =T)
  sig.x1 <- sd(xtrans,na.rm = T)
  sig.x2 <- sd(ytrans,na.rm =T)
  n1 <- length(xtrans)
  n2 <- length(ytrans)
  
  t <- (x1-x2) / sqrt( ((sig.x1^2)/n1) + ((sig.x2^2) / n2))
  tstat <- t
  
  return(tstat)
}

my_histogram_qqplots <- function(Observed_Flow,Gage) {
  
  library('ADGofTest')
  x <- na.exclude(Observed_Flow)
  x[x<=0] <- 0
  
  lambda <- .3 # Determined For Rio Gages using Maximum Liklihood Estimation
  lambda2 <- 1 # Adds a constant value of 1 because we have streamflow values of 0
  
  xtrans <-  ((x + lambda2) ^ lambda - 1) / lambda
  
  qqnorm(y=x, pch = 1, frame = FALSE, main = paste("Observed Streamflow at",Gage))
  qqline(y=x, col = "steelblue", lwd = 2)
  
  qqnorm(y=xtrans, pch = 1, frame = FALSE, main = paste("Transformed Streamflow at",Gage))
  qqline(y=xtrans, col = "steelblue", lwd = 2)
  
  sf <- x*.0283168 # Converts streamflow (CFS) into cms
  
  hist(sf, main = paste("Observed Streamflow at",Gage), xlab = "Streamflow (cms)")
  hist(xtrans, main = paste("Transformed Streamflow at",Gage), xlab = "Transformed Streamflow")
  
  AD_Observed <- ad.test(x,pnorm)
  P_Observed <- as.numeric(AD_Observed$p.value)
  AD_Observed
  
  AD_Transformed <- ad.test(xtrans,pnorm)
  P_Transformed <- as.numeric(AD_Transformed$p.value)
  AD_Transformed
  
  ks_observed <- ks.test(x, "pnorm",mean(x),sd(x))
  ks_observed
  
  ks_transformed <- ks.test(xtrans, "pnorm",mean(xtrans),sd(xtrans))
  ks_transformed
  
  df <- data.frame("Gage" = Gage, "AD_Observed"=P_Observed, "AD_Transformed"=P_Transformed,
                   "KS_Observed D" = ks_observed$statistic,"KS_Transformed D" = ks_transformed$statistic )
  return(df)
}

# Plotting functions for probability of failure
PLOT_POF <- function(Melted.Data) {
  
  # Colorblind pallete
  cbbPalette <- c("#56B4E9","#E69F00", "#009E73")
  
  Melted.Data$Gauge <- paste(Melted.Data$x , "(",round(Melted.Data$river_mile*1.60934,digits = 0) ,")")
  
  ggplot(Melted.Data, aes(x=Gauge, y=value, fill=variable))+ 
    geom_bar(stat="identity",position = "dodge", alpha=.6)+ 
    facet_grid(. ~ Consumption) +ggtitle("") + 
    xlab("Gauge (River Kilometers Downstream Of SWRP)") +ylab("Probability of Failure (%)") +
    theme_bw()+
    scale_fill_manual(values=cbbPalette)+
    theme(legend.title=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = 'top',legend.text=element_text(size=10))+
    geom_point(aes(x = Gauge[1], y = Melted.Data$noconsumption[1]), shape = 95, size =20)  +
    geom_point(aes(x = Gauge[2], y = Melted.Data$noconsumption[2]),shape = 95, size =20) + 
    geom_point(aes(x = Gauge[3], y = Melted.Data$noconsumption[3]),shape = 95, size =20) +
    guides(fill = guide_legend(override.aes = list(shape = NA)))+
    theme(legend.key.size = unit(0.01, "cm"))
  
}

# Plotting function for the t statistics
PLOT_TSTAT <- function(Melted.Data) {
  
  # used for plotting the "threshold value" when alpha = .05 (Z = 1.96)
  x_thresh <- c(0, 0)
  y_thresh <- c(1.96, 1.96)
  threshold.df <- data.frame(x_thresh, y_thresh)
  
  # Add kilometers
  Melted.Data$river_km <- Melted.Data$river_mile * 1.60934
  
  # Color blind pallete for plotting
  cbbPalette <- c("#56B4E9","#E69F00", "#009E73")
  
  # Plots t statistic
  ggplot(Melted.Data, aes(x=river_km, y=value, group=variable, variable, color=variable))+ 
    geom_line(linetype = 2) +
    geom_point(shape = 17 , size =3)+
    theme_bw()+
    scale_colour_manual(values=cbbPalette)+
    facet_grid(. ~ Consumption) +ggtitle("") + 
    xlab("River Kilometers Downstream of SWRP") + ylab("t-statistic")  +
    geom_hline(data = threshold.df, aes(yintercept=y_thresh), size=1) + 
    #theme(legend.title=element_blank(), legend.position = "top")
    theme(legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.5)),
          axis.text.x = element_text(size=12,hjust =.85),
          axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          legend.position = "top")
  
}




