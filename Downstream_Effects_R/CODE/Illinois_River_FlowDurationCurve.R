################################################################################
#
# Builds two flow duration curves from a data frame (date, stage, and flow) and 
#    consumption; signifying the shift.
#
# Note: There are no labels on the plots. The top most bold line is the de facto 
#         scenario (no consumption). The bottom bold line represents the worst
#         case scenario (highest uniform consumption).
#
# Zac Barker, December 2016
#
################################################################################

library("ggplot2")
library("reshape2")

FDC <- function(guage_name, df) {
     
  # Get list of scenarios to plot
  scenarios <- colnames(df)[grepl("Flow", names(df))]
  
  # Apply to each flow scenario
  for(i in scenarios){
    
    # Headings
    heading1 <- paste0(i,"Rank")
    heading2 <- paste0(i, "Exceedence")
    
    # Sort flows
    df <- df[order(df[,i]),]
    df[,heading1] <- order(df[,i])
    df[,heading2] = 100-(100*(df[,heading1]/(length(df[,heading1])+1)))
    
  }
  
  # Subset to only graphing parameters
  exceedence <- df[grepl("Exceedence", names(df))]
  flow <- df[scenarios]
  
  # Upper bound data (current)
  ub <- data.frame(flow[,1], exceedence[,1])
  colnames(ub) <- c("flow", "exceedence")
  
  # Lower bound data (Minimum total flow)
  min <- colSums(flow, na.rm = T)
  min <- data.frame(keyName=names(min), value=min, row.names=NULL)
  min <- min[which(min$value == min(min$value, na.rm = TRUE)), ]
  minName <- as.character(min$keyName[1])
  minName <- substr(minName, 1, (nchar(minName)-4))
  
  minExceedence <- exceedence[,paste0(minName,"FlowExceedence")]
  minFlow <- flow[,paste0(minName,"Flow")]
  lb <- data.frame(minFlow, minExceedence)
  
  # Reshape to plot
  exceedence <- melt(exceedence)
  flow <- melt(flow)
  
  flow$value <- flow$value * .0283168
  
  dd <- data.frame(flow, exceedence)
  
  ##
  ub$flow <- ub$flow * .0283168
  lb$minFlow <- lb$minFlow * .0283168
  
  
  Lb_df <- data.frame(variable = "MinFlow", value = lb$minFlow, variable.1 = "MinflowExceedence", value.1=lb$minExceedence)
  
  DFtranspose <- rbind(dd,Lb_df)
  
  
  # Add lowerbound (lb) data to dd
  #   variable = "MinFLow", value = lb$minFlow, variabe.1 = MinFlow Exceedence, value.1 = lb$minExceedence
  # Add single column to all of dd with common term for plotting
  # In plotting, make color = "common term"
  
  
  
  curve <- ggplot(dd) + 
    geom_line(aes(x=value.1, y=value,colour = variable)) +
    scale_color_manual(values = rep("gray",15))+
    geom_line(data = ub, aes(x = exceedence, y = flow), size = 1) + 
    # geom_line(data = lb, aes(x = minExceedence, y = minFlow), size = 1) + 
    scale_y_log10()+
    theme_bw()+
    xlab("Exceedence Probability (%)")+
    ylab("Flow  (cms)")+
    #ggtitle(paste(guage_name, "Flow Duration Curve"))+
    theme(legend.position="none",
          plot.title = element_text(size = rel(2)),
          axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)))
  
  print(curve)
  

}
