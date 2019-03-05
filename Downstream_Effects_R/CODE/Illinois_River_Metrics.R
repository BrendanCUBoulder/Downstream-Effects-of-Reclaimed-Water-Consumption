################################################################################
#
# Statistical metrics used for assesing the upstream consumption
# 
# Zac Barker, December 2016
# 
################################################################################

# T test
T_TEST <- function(df, name){
  
  source("./Code/Illinois_River_DataTransformandTtest.R")
  library(BSDA)
  # Create dataframe from columns needed
  consumption <- paste0(name,"Consumption")
  flow2 <- paste0(name,"Flow")
  df2 <- data.frame(df$Flow, df[,flow2], df[,consumption])
  names(df2) <- c("Flow", "Flow2", "Consumption")
  
  # Subset based on when there is actually consumption
  df2 <- df2[df2$Consumption > 0,]
  
  # Perform t test
  if(nrow(df2) > 0){
    
    #t test following boxcox
    tStat <- my_boxcox_and_ttest(df2$Flow,df2$Flow2)
    
  } else {
    tStat <- 0
  }
  return(tStat)
}

# Average Length of Failure
LOF <- function(df, slope, threshold, name){
  
  # Create dataframe from columns needed
  heading1 <- paste0(name, "Consumption")
  heading2 <- paste0(name, "Stage")
  df2 <- data.frame(df[,heading1], df[,heading2])
  names(df2) <- c("Consumption", "Stage")
  
  # Subset based on when there is actually consumption
  #df2 <- df2[df2$Consumption > 0,]
  
  # Remove NAs
  stageWOna <- df2$Stage[!is.na(df2$Stage)]
  
  # Calculate probability of failure
  if(nrow(df2) > 0){
    
    # Develop "D" for each row
    stageWOna_D <- ifelse(stageWOna >= threshold, 0, threshold - stageWOna)
    
    a<-c()
    b<-c()
    stageWOna_D[(length(stageWOna_D)+1)] <- 0
    
    for(j in 1:(length(stageWOna_D)-1))  {
      
      a[j] = ifelse(stageWOna_D[j] > 0 & stageWOna_D[j+1] > 0,1,0)
      b[j] = ifelse(stageWOna_D[j] > 0 & stageWOna_D[j+1] == 0,1,0)
      
    }
    
    LOFout = 1+(sum(a)/sum(b))
    
  } else {
    LOFout <- 0
  }
  return(LOFout)
}

# Average Failure Magnitude
AFM <- function(df, slope, threshold, name){
  
  # Create dataframe from columns needed
  heading1 <- paste0(name, "Consumption")
  heading2 <- paste0(name, "Stage")
  df2 <- data.frame(df[,heading1], df[,heading2])
  names(df2) <- c("Consumption", "Stage")
  
  # Subset based on when there is actually consumption
  #df2 <- df2[df2$Consumption > 0,]
  
  # Remove NAs
  stageWOna <- df2$Stage[!is.na(df2$Stage)]
  
  if(nrow(df2) > 0){
    
    # Develop "D" for each row
    stageWOna_D <- ifelse(stageWOna >= threshold, 0, threshold - stageWOna)
    
    AFMout =sum(stageWOna_D) / length(which(stageWOna_D>0))
    
  } else {
    AFMout <- 0
  }
  return(AFMout)
}


P_FAIL <- function(df, slope, threshold, name){
  
  # Create dataframe from columns needed
  heading1 <- paste0(name, "Consumption")
  heading2 <- paste0(name, "Stage")
  df2 <- data.frame(df[,heading1], df[,heading2])
  names(df2) <- c("Consumption", "Stage")
  
  # Subset based on when there is actually consumption
  #df2 <- df2[df2$Consumption > 0,]
  
  # Remove NAs
  stageWOna <- df2$Stage[!is.na(df2$Stage)]
  
  # Calculate probability of failure
  if(nrow(df2) > 0){
    countFail <- length(which(stageWOna<threshold))
    countLength <- length(stageWOna)
    pFailOut <- (countFail/countLength)*100
  } else {
    pFailOut <- 0
  }
  return(pFailOut)
}

# 

# Total revenue lost for each scenario, not used in current draft of paper
COST <- function(df, name, quant) {
  heading1 <- paste0(name, quant)
  total <- sum(df[,heading1], na.rm = T)
  return(total/(length(df[,heading1])/365.25))
}

# Plot t test
PLOT_TTEST <- function(df){
  
  # Remove current 
  dd <- df[ , !(names(df) %in% c("Current"))]
  
  # Remove 0 consumption
  dd <- dd[!(dd$Scaler == 0),]
  
  # EDIT to make cms instead of MGD
  dd$Scaler <- signif(dd$Scaler * .0438126,digits = 2)
  
  # Add MGD to scaler
  dd$Scaler <- paste(dd$Scaler,"cms")
  
  # add Vector of lables for plotting, only use if using 3 scenarios at 10,50 and 100% ADF
  label_vec <- c(paste("10% ADF (", dd$Scaler[1],")"),
            paste("50% ADF (", dd$Scaler[1+length(unique(dd$Gauge))],")"),
            paste("100% ADF (", dd$Scaler[1+2*length(unique(dd$Gauge))],")"))
  names(label_vec) <- unique(dd$Scaler)      
  
  # Edit Station to be below stuckney
  dd$Station <- 319 - dd$Station
  
  # Edit Station to be in Kilometers
  dd$Station <- dd$Station * 1.60934
                 
  # Reshape to plot
  dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
  
  ################## EDIT ###################
  # Make the Scaler a factor to avoid sorting
  #dd$Scaler <- factor(dd$Scaler, levels = dd$Scaler)
  
  dd$Scaler <- factor(dd$Scaler, levels = unique(dd$Scaler))
  
  #dd$Scaler <- factor(dd$Scaler)
  ############################################
  
  # color blind color palette
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73")
  
  
  # t Stat threshold
  x <- c(0, 0)
  y <- c(1.96, 1.96)
  threshold <- data.frame(x, y)
  
  # Plot
  p <- ggplot(dd) + geom_line(aes(x=Station, y=value, colour=variable), size=1,linetype = 2)+
    geom_point(aes(x=Station, y=value, colour=variable), shape = 17 , size =3)+
    facet_grid(.~Scaler, labeller = labeller(Scaler = label_vec))+
    theme_bw()+
    scale_colour_manual(values=cbbPalette)+
    geom_hline(data = threshold, aes(yintercept=y), size=1)+
    xlab("River Kilometers Downstream of Stickney WWTP")+
    ylab("t-statistic")+
    theme(legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.5)),
          axis.text.x = element_text(size=12,hjust =.85),
          axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          legend.position = "top")
  
  print(p)
}

PLOT_PFAIL <- function(df){
  
  current <- df[,c("Gauge","Current")]
  
  # Remove current 
  dd <- df[ , !(names(df) %in% c("Current"))]
  
  # Remove 0 consumption
  dd <- dd[!(dd$Scaler == 0),]
  
  # Edit
  dd$Scaler <- signif(dd$Scaler * .0438126,digits = 2)
  
  # Add CFS to scaler
  dd$Scaler <- paste(dd$Scaler,"cms")
  
  ################## EDIT ###################
  # Make the Scaler a factor to avoid sorting
  #dd$Scaler <- factor(dd$Scaler, levels = dd$Scaler)
  
  dd$Scaler <- factor(dd$Scaler, levels = unique(dd$Scaler))
  
  #dd$Scaler <- factor(dd$Scaler)
  
  ###########################################
  # Make Gauge into kilometers
  dd$Gauge <- paste(dd$Gauge , "(",round(1.60934* (319 - dd$Station )),")")
  
  
  # Reshape to plot
  dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
  
  ############# EDIT ###############
  
  # Add 0 consumption abline
  # color blind color palette
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73")
  
  # Labels for Plotting
  label_vec <- c(paste("10% ADF (", dd$Scaler[1],")"),
                 paste("50% ADF (", dd$Scaler[1+length(unique(dd$Gauge))],")"),
                 paste("100% ADF (", dd$Scaler[1+2*length(unique(dd$Gauge))],")"))
  names(label_vec) <- unique(dd$Scaler)
  
  
  
  p <- ggplot(dd, aes(x = factor(Gauge,levels = c(dd$Gauge[1], dd$Gauge[2], dd$Gauge[3],dd$Gauge[4],dd$Gauge[5])), y = value, fill = variable)) + geom_bar(stat = "identity", position = "dodge", alpha = .6) +
    facet_grid(.~Scaler,labeller = labeller(Scaler = label_vec))+
    theme_bw()+
    scale_fill_manual(values=cbbPalette)+
    xlab("Gauge (River Kilometers Downstream of Stickney WWTP)")+
    ylab("Probability of Failure (%)")+
    geom_point(aes(x = dd$Gauge[5], y = current$Current[5]), shape = 95, size =15)  +
    geom_point(aes(x = dd$Gauge[4], y = current$Current[4]), shape = 95, size =15) +
    geom_point(aes(x = dd$Gauge[3], y = current$Current[3]), shape = 95, size =15) +
    geom_point(aes(x = dd$Gauge[2], y = current$Current[2]), shape = 95, size =15) +
    geom_point(aes(x = dd$Gauge[1], y = current$Current[1]), shape = 95, size =15) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    theme(legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.4)),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          legend.position = 'top'
         )
  print(p)
}

# Use this function to plot each of the stakeholder metrics
PLOT_Metrics <- function(df,Metric){
  
  if(Metric == 'POF') {
    ylabel <- "Probability of Failure (%)"
  }
  
  if(Metric == 'AFM') {
    ylabel <- "Average Failure Magnitude (meters)"
    df[,4:7] <- df[,4:7]*.3048 # ft to meters
    
  }
  
  if(Metric == 'LOF') {
    ylabel <- "Average Failure Duration (Days)"
  }
  
  current <- df[,c("Gauge","Current")]
  
  # Remove current 
  dd <- df[ , !(names(df) %in% c("Current"))]
  
  # Remove 0 consumption
  dd <- dd[!(dd$Scaler == 0),]
  
  # Edit
  dd$Scaler <- signif(dd$Scaler * .0438126,digits = 2)
  
  # Add CFS to scaler
  dd$Scaler <- paste(dd$Scaler,"cms")
  
  
  dd$Scaler <- factor(dd$Scaler, levels = unique(dd$Scaler))
  

  # Make Gauge into kilometers
  dd$Gauge <- paste(dd$Gauge , "(",round(1.60934* (319 - dd$Station )),")")
  
  # Reshape to plot
  dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
 
  # color blind color palette
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73")
  
  # Labels for Plotting
  label_vec <- c(paste("10% ADF (", dd$Scaler[1],")"),
                 paste("50% ADF (", dd$Scaler[1+length(unique(dd$Gauge))],")"),
                 paste("100% ADF (", dd$Scaler[1+2*length(unique(dd$Gauge))],")"))
  names(label_vec) <- unique(dd$Scaler)
  
  p <- ggplot(dd, aes(x = factor(Gauge,levels = c(dd$Gauge[1], dd$Gauge[2], dd$Gauge[3],dd$Gauge[4],dd$Gauge[5])), y = value, fill = variable)) + geom_bar(stat = "identity", position = "dodge", alpha = .6) +
    facet_grid(.~Scaler,labeller = labeller(Scaler = label_vec))+
    theme_bw()+
    scale_fill_manual(values=cbbPalette)+
    xlab("Gage (River Kilometers Downstream of Consumption)")+
    ylab(ylabel)+
    geom_point(aes(x = dd$Gauge[5], y = current$Current[5]), shape = 95, size =15)  +
    geom_point(aes(x = dd$Gauge[4], y = current$Current[4]), shape = 95, size =15) +
    geom_point(aes(x = dd$Gauge[3], y = current$Current[3]), shape = 95, size =15) +
    geom_point(aes(x = dd$Gauge[2], y = current$Current[2]), shape = 95, size =15) +
    geom_point(aes(x = dd$Gauge[1], y = current$Current[1]), shape = 95, size =15) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    theme(legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.4)),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          legend.position = 'top'
    )
  print(p)
}











# Plot revenue lost
PLOT_RLOST <- function(df, min, max){
  
  # Remove current 
  dd <- df[ , !(names(df) %in% c("Current"))]
  min <- min[ , !(names(min) %in% c("Current"))]
  max <- max[ , !(names(max) %in% c("Current"))]
  
  # Remove 0 consumption
  dd <- dd[!(dd$Scaler == 0),]
  min <- min[!(min$Scaler == 0),]
  max <- max[!(max$Scaler == 0),]
  
  # Add MGD to scaler
  dd$Scaler <- paste(dd$Scaler,"MGD")
  
  ################## EDIT ###################
  # Make the Scaler a factor to avoid sorting
  #dd$Scaler <- factor(dd$Scaler, levels = dd$Scaler)
  
  dd$Scaler <- factor(dd$Scaler, levels = unique(dd$Scaler))
  
  #dd$Scaler <- factor(dd$Scaler)
  ############################################
  
  
  # Reshape to plot
  dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
  
  # Add min and max to dd
  min <- melt(min, id=c("Gauge", "Station", "Scaler"))
  max <- melt(max, id=c("Gauge", "Station", "Scaler"))
  dd$min <- min$value
  dd$max <- max$value
  
  # Convert to millions of dollars
  dd$value <- dd$value/1000000
  dd$min <- dd$min/1000000
  dd$max <- dd$max/1000000
  
  # Plot
  p <- ggplot(dd, aes(x = factor(Gauge), y = value)) + geom_bar(stat = "identity") +
    geom_errorbar(aes(x = factor(Gauge), ymin = min, ymax = max), stat = "identity", size = .75, width=0.25) + 
    facet_grid(variable~Scaler)+
    theme_bw()+
    xlab("Gauge")+
    ylab("Value lost (Million $/year)")+
    ggtitle("Maximum consumption per day")+
    theme(legend.justification=c(0,1), 
          legend.position=c(0,1),
          legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)))
  
  print(p)
}





# Plot revenue lost
PLOT_RLOST <- function(df, min, max){
  
  # Remove current 
  dd <- df[ , !(names(df) %in% c("Current"))]
  min <- min[ , !(names(min) %in% c("Current"))]
  max <- max[ , !(names(max) %in% c("Current"))]
  
  # Remove 0 consumption
  dd <- dd[!(dd$Scaler == 0),]
  min <- min[!(min$Scaler == 0),]
  max <- max[!(max$Scaler == 0),]
  
  # Add MGD to scaler
  dd$Scaler <- paste(dd$Scaler,"MGD")
  
  ################## EDIT ###################
  # Make the Scaler a factor to avoid sorting
  #dd$Scaler <- factor(dd$Scaler, levels = dd$Scaler)
  
  dd$Scaler <- factor(dd$Scaler, levels = unique(dd$Scaler))
  
  #dd$Scaler <- factor(dd$Scaler)
  ############################################
  
  
  # Reshape to plot
  dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
  
  # Add min and max to dd
  min <- melt(min, id=c("Gauge", "Station", "Scaler"))
  max <- melt(max, id=c("Gauge", "Station", "Scaler"))
  dd$min <- min$value
  dd$max <- max$value
  
  # Convert to millions of dollars
  dd$value <- dd$value/1000000
  dd$min <- dd$min/1000000
  dd$max <- dd$max/1000000
  
  # Plot
  p <- ggplot(dd, aes(x = factor(Gauge), y = value)) + geom_bar(stat = "identity") +
    geom_errorbar(aes(x = factor(Gauge), ymin = min, ymax = max), stat = "identity", size = .75, width=0.25) + 
    facet_grid(variable~Scaler)+
    theme_bw()+
    xlab("Gauge")+
    ylab("Value lost (Million $/year)")+
    ggtitle("Maximum consumption per day")+
    theme(legend.justification=c(0,1), 
          legend.position=c(0,1),
          legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)))
  
  print(p)
}

# Plot revenue lost
PLOT_NETRLOST <- function(df, min, max){
  
  # Remove 0 consumption
  df <- df[!(df$Scaler == 0),]
  min <- min[!(min$Scaler == 0),]
  max <- max[!(max$Scaler == 0),]
  
  # Add MGD to scaler
  df$Scaler <- paste(df$Scaler,"MGD")
  
  ################## EDIT ###################
  # Make the Scaler a factor to avoid sorting
  #df$Scaler <- factor(df$Scaler, levels = df$Scaler)
  
  df$Scaler <- factor(df$Scaler, levels = unique(df$Scaler))
  
  #df$Scaler <- factor(df$Scaler)
  ###########################################
  
  # Calculate the net loss using the current scenario as baseline
  scenarios <- ncol(df)
  for(scenario in 5:scenarios){
    df[,scenario] <- df[,scenario] - df[,"Current"]
    min[,scenario] <- min[,scenario] - min[,"Current"]
    max[,scenario] <- max[,scenario] - max[,"Current"]
  }
  df$Current <- 0
  df2 <- df
  
  # Remove current 
  dd <- df[ , !(names(df) %in% c("Current"))]
  min <- min[ , !(names(min) %in% c("Current"))]
  max <- max[ , !(names(max) %in% c("Current"))]
  
  # Reshape to plot
  dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
  
  # Add min and max to dd
  min <- melt(min, id=c("Gauge", "Station", "Scaler"))
  max <- melt(max, id=c("Gauge", "Station", "Scaler"))
  dd$min <- min$value
  dd$max <- max$value
  
  # Convert to millions of dollars
  dd$value <- dd$value/1000000
  dd$min <- dd$min/1000000
  dd$max <- dd$max/1000000
  
  # Plot
  p <- ggplot(dd, aes(x = factor(Gauge), y = value)) + geom_bar(stat = "identity") +
    geom_errorbar(aes(x = factor(Gauge), ymin = min, ymax = max), stat = "identity", size = .75, width=0.25) + 
    facet_grid(variable~Scaler)+
    theme_bw()+
    xlab("Gauge")+
    ylab("Increase of value lost (Million $/year)")+
    ggtitle("Maximum consumption per day")+
    theme(legend.justification=c(0,1), 
          legend.position=c(0,1),
          legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)))
  
  print(p)
  return(df2)
}
