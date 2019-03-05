my_histogram_qqplots <- function(Observed_Flow,Gage) {
  
  library('ADGofTest')
  x <- na.exclude(Observed_Flow)
  x[x<=0] <- 0
  
  lambda <- .5 # Determined For Each Rio Gauge using Maximum Liklihood Estimation
  # The lambda value is likly to be different for each streamgauge
  
  lambda2 <- 1 # Adds a constant value of 1 because we have streamflow values of 0
  
  xtrans <-  ((x + lambda2) ^ lambda - 1) / lambda
  
  qqnorm(y=x, pch = 1, frame = FALSE, main = paste("Observed Streamflow at",Gage))
  qqline(y=x, col = "steelblue", lwd = 2)
  
  qqnorm(y=xtrans, pch = 1, frame = FALSE, main = paste("Transformed Streamflow at",Gage))
  qqline(y=xtrans, col = "steelblue", lwd = 2)
  
  sf <- x*.0283168
  
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
