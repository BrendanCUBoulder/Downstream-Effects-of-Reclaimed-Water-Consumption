# Create T-Test

my_boxcox_and_ttest <- function(Flow,Flow2) {
  
  #library(BSDA)
  x <- na.exclude(Flow)
  x[x<=0] <- 0
  
  y <- na.exclude(Flow2)
  y[y<=0] <-0
  
  lambda <- .5 # This wil change depending on streamflow
  lambda2 <- 1 # because we have values of 0
  
  # Tranformation
  xtrans <-  ((x + lambda2) ^ lambda - 1) / lambda
  ytrans <-  ((y + lambda2) ^ lambda - 1) / lambda
  
  #my_z_test(x,y)
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
