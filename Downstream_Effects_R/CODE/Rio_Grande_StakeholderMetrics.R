# Potential Metrics

Create_D_dataframe <- function(Gauge_df,threshold) {
  
  length_df <- ncol(Gauge_df)
  n <- 4 #Number of rows that arent consumption scenarios
  
  D_temp <- Gauge_df[n:length_df]
  D <- D_temp
  
  for(i in 1:(length_df - (n-1))) {
    D[,i] <- ifelse(D_temp[,i] >= threshold, 0, threshold - D_temp[,i])
  }
  return(D)
}

POF_D <- function(D_dataframe) {
  
  # Rename to df 
  df <- D_dataframe
  # Create an empty list to fill in with probability of failure
  pf <- c()
  
  # Determine Probability of failure
  for(i in 1:ncol(df)) {
    xx <- df[,i]
    pf[i] <- length( which(xx > 0)) / length(xx)
  }
  return(pf)
}

# Resilience, not used in the latest darft of the paper
RES_D <- function(D_dataframe) {
  
  # Rename df 
  df <- D_dataframe
  
  # Create an empty list to fill in with probability of failure
  res <- c()
  
  # Determine Resilience
  for(i in 1:ncol(df)) {
    
    zz <- df[,i]
    xx <- df[1:(nrow(df)-1),i]
    yy <- df[2:nrow(df),i]
    
    dif = dplyr::data_frame(xx,yy)
    
    res[i] = length(which(dif$xx>0 & dif$yy==0)) / length(which(zz>0))
  }
  return(res)
}

# Vulnerability
VUL_D <- function(D_dataframe,threshold) {
  
  df <- D_dataframe
  vul = c()
  
  for(i in 1:ncol(df)) {
    
    zz <- df[,i]
    vul[i] = (sum(zz) / length(which(zz>0)))* 0.028316847 # Change CFS to cms
  }
  
  return(vul)
  
}

# Average length of a failure period
LOF_D <- function(D_dataframe) {
  
  df <- D_dataframe
  LOF <- c()
  
  for(i in 1:ncol(df)) {
    
    a <- c()
    b<-c()
    df_temp <- df[,i]
    df_temp[(length(df_temp)+1)] <- 0
    
    for(j in 1:(length(df_temp)-1))  {
      
      a[j] = ifelse(df_temp[j] > 0 & df_temp[j+1] > 0,1,0)
      b[j] = ifelse(df_temp[j] > 0 & df_temp[j+1] == 0,1,0)
      
    }
    
    LOF[i] = 1+(sum(a)/sum(b))
  }
  
  return(LOF)
}

# Plot stakeholder metrics
PLOT_single_metric <- function(Melted.Data,Metric) {
  
  # Resilience is not used in the latest draft
  if(Metric == 'RES' || Metric == 'Resilience') {
    colnames(Melted.Data)[which(names(Melted.Data) == "Resilience_Value")] <- "Value"
    colnames(Melted.Data)[which(names(Melted.Data) == "No_Consumption_Resilience")] <- "noconsumption"
    metric_type = 'Resilience'
    
  }
  
  if(Metric == 'POF' || Metric == 'Probability of failure') {
    colnames(Melted.Data)[which(names(Melted.Data) == "POF_Value")] <- "Value"
    colnames(Melted.Data)[which(names(Melted.Data) == "No_Consumption_POF")] <- "noconsumption"
    metric_type = 'Probability of Failure'
    ylabel = 'Probability of Failure (%)'
  }
  
  if(Metric == 'VUL' || Metric == 'Vulnerability') {
    colnames(Melted.Data)[which(names(Melted.Data) == "Vulnerability_Value")] <- "Value"
    colnames(Melted.Data)[which(names(Melted.Data) == "No_Consumption_Vulnerability")] <- "noconsumption"
    metric_type = 'Vulnerability'
    ylabel = 'Average Failure Magnitude (cms)'
  }
  
  if(Metric == 'AFD' || Metric == 'LOF' | Metric == 'Average Failure Duration') {
    colnames(Melted.Data)[which(names(Melted.Data) == "AFD_Value")] <- "Value"
    colnames(Melted.Data)[which(names(Melted.Data) == "No_Consumption_AFD")] <- "noconsumption"
    metric_type = 'Average Failure Duration'
    ylabel = 'Average Failure Duration (Days)'
  }
  
  # Color blind pallete
  cbbPalette <- c("#E69F00","#56B4E9", "#009E73")
  
  # Add distance to Gauge Namee
  Melted.Data$Gauge <- paste(Melted.Data$Gauge , "(",Melted.Data$River_Kilometer,")")
  
  ggplot(Melted.Data, aes(x=Gauge, y=Value, fill=Scenario))+ 
    geom_bar(stat="identity",position = "dodge", alpha=.6)+ 
    facet_grid(. ~ Consumption) +ggtitle("") + 
    xlab("Gage (River Kilometers Downstream Of Consumption)") +
    #ylab(paste(metric_type,'(%)')) +
    ylab(ylabel)+
    theme_bw()+
    scale_fill_manual(values=cbbPalette)+
    theme(legend.title=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = 'top',legend.text=element_text(size=10))+
    guides(fill = guide_legend(override.aes = list(shape = NA)))+
    #theme(legend.key.size = unit(0.01, "cm"))+
    theme(legend.title=element_blank(), 
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(size = rel(1.4)),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          legend.position = 'top'
    )+
    geom_point(aes(x = Gauge[1], y = Melted.Data$noconsumption[1]), shape = 95, size =20)+
    geom_point(aes(x = Gauge[2], y = Melted.Data$noconsumption[2]),shape = 95, size =20) + 
    geom_point(aes(x = Gauge[3], y = Melted.Data$noconsumption[3]),shape = 95, size =20) 
  
}


# Melt Each set of metrics individually, then combine
Melt_Metric <- function(Gauge.Names,Data_Frame.1,Data_Frame.2,Data_Frame.3, Type) {
  
  # Currenty only works for 3 consumptions/3 scenarios + the historical observations

  x <- Gauge.Names
  
  # Used for looping
  i=1 
  
  # Melt Data for 9 CFS
  winter <- c(Data_Frame.1[4+i],Data_Frame.2[4+i],Data_Frame.3[4+i])
  summer <- c(Data_Frame.1[7+i],Data_Frame.2[7+i],Data_Frame.3[7+i])
  uniform <- c(Data_Frame.1[1+i],Data_Frame.2[1+i],Data_Frame.3[1+i])
  begin_melt_9CFS <- data.frame(x=x,summer =summer,winter=winter,uniform = uniform)
  melted_9CFS <- melt(begin_melt_9CFS,id ="x")
  
  # Melt data for 45 CFS
  winter <- c(Data_Frame.1[5+i],Data_Frame.2[5+i],Data_Frame.3[5+i])
  summer <- c(Data_Frame.1[8+i],Data_Frame.2[8+i],Data_Frame.3[8+i])
  uniform <- c(Data_Frame.1[2+i],Data_Frame.2[2+i],Data_Frame.3[2+i])
  begin_melt_45CFS <- data.frame(x=x,summer =summer,winter=winter,uniform = uniform)
  melted_45CFS <- melt(begin_melt_45CFS,id ="x")
  
  # For 90 CFS
  winter <- c(Data_Frame.1[6+i],Data_Frame.2[6+i],Data_Frame.3[6+i])
  summer <- c(Data_Frame.1[9+i],Data_Frame.2[9+i],Data_Frame.3[9+i])
  uniform <- c(Data_Frame.1[3+i],Data_Frame.2[3+i],Data_Frame.3[3+i])
  begin_melt_90CFS <- data.frame(x=x,summer =summer,winter=winter,uniform = uniform)
  melted_90CFS<-melt(begin_melt_90CFS,id ="x")
  
  # Combine melted Scenarios
  melted_together <- rbind(melted_9CFS,melted_45CFS,melted_90CFS)
  
  # Add consumption column
  Consumption <- c(rep("10% ADF (.255 cms)",length(melted_9CFS$value)),rep("50% ADF (1.28 cms)",length(melted_45CFS$value)),rep("100% ADF (2.55 cms)",length(melted_90CFS$value)))
  melted_together <- cbind(melted_together,Consumption)
  melted_together$Consumption = factor(melted_together$Consumption, levels=c('10% ADF (.255 cms)','50% ADF (1.28 cms)','100% ADF (2.55 cms)'))
  
  # Baseline will be used for plotting
  Isleta_abline <- Data_Frame.1[i]
  San_Acacia_abline <- Data_Frame.2[i]
  San_Marcial_abline <- Data_Frame.3[i]
  
  # Determine no conusmption baseline  value
  melted_together$noconsumption <- Consumption
  melted_together$noconsumption <- ifelse(melted_together$x =="Isleta",Isleta_abline,melted_together$noconsumption)
  melted_together$noconsumption <- ifelse(melted_together$x == "San Acacia",San_Acacia_abline,melted_together$noconsumption)
  melted_together$noconsumption <- ifelse(melted_together$x == "San Marcial",San_Marcial_abline,melted_together$noconsumption)
  melted_together$noconsumption <- as.numeric(melted_together$noconsumption)
  
  # River Miles Below WWTP (Will be Changed to Kilometers in plots)
  RM_Below_SWRP <- c(8,61,100)
  
  melted_together$river_mile <- 0
  melted_together$river_mile <- ifelse(melted_together$x =="Isleta",RM_Below_SWRP[1],melted_together$river_mile)
  melted_together$river_mile <- ifelse(melted_together$x =="San Acacia",RM_Below_SWRP[2],melted_together$river_mile)
  melted_together$river_mile <- ifelse(melted_together$x =="San Marcial",RM_Below_SWRP[3],melted_together$river_mile)
  
  if(Type == 'RES' || Type == 'Resilience') {
    melted_together$value <- melted_together$value* 100 
    melted_together$noconsumption <- melted_together$noconsumption *100
    colnames(melted_together)[which(names(melted_together) == "value")] <- "Resilience_Value"
    colnames(melted_together)[which(names(melted_together) == "noconsumption")] <- "No_Consumption_Resilience"
  }
  
  if(Type == 'LOF' || Type == 'Length of failure' || Type == 'AFD') {
    melted_together$value <- melted_together$value # * 100 for RES but not FOr LOF
    melted_together$noconsumption <- melted_together$noconsumption #* 100for RES but not FOr LOF
    colnames(melted_together)[which(names(melted_together) == "value")] <- "AFD_Value"
    colnames(melted_together)[which(names(melted_together) == "noconsumption")] <- "No_Consumption_AFD"
  }
  
  if(Type == 'POF' || Type == 'Probability of failure') {
    
    melted_together$value <- melted_together$value * 100
    melted_together$noconsumption <- melted_together$noconsumption * 100
    colnames(melted_together)[which(names(melted_together) == "value")] <- "POF_Value"
    colnames(melted_together)[which(names(melted_together) == "noconsumption")] <- "No_Consumption_POF"
  }
  
  if(Type == 'VUL' || Type == 'Vulnerability') {
    melted_together$value <- melted_together$value
    melted_together$noconsumption <- melted_together$noconsumption
    colnames(melted_together)[which(names(melted_together) == "value")] <- "Vulnerability_Value"
    colnames(melted_together)[which(names(melted_together) == "noconsumption")] <- "No_Consumption_Vulnerability"
  }
  
  return(melted_together)
}

Combine_Melts <- function(POF_melt,Res_melt,Vul_melt) {
  
  df <- POF_melt
  df$Resilience_Value <- Res_melt$Resilience_Value
  df$No_Consumption_Resilience <- Res_melt$No_Consumption_Resilience
  df$Vulnerability_Value <- Vul_melt$Vulnerability_Value
  df$No_Consumption_Vulnerability <- Vul_melt$No_Consumption_Vulnerability
  
  
  colnames(df)[which(names(df) == "x")] <- "Gauge"
  colnames(df)[which(names(df) == "variable")] <- "Scenario"
  
  df$river_mile <- round(df$river_mile*1.60934,0)
  colnames(df)[which(names(df) == "river_mile")] <- "River_Kilometer"
  
  # Re organize the dataframe to match Illinois Case study
  df <-df[,c(1,2,4,6,5,3,8,7,10,9)]
  
  return(df)
}

# Following functions are not used in the latest draft of the paper
Combine_Melts_AFD <- function(POF_melt,AFD_melt,Vul_melt) {
  
  df <- POF_melt
  df$AFD_Value <- AFD_melt$AFD_Value
  df$No_Consumption_AFD <- AFD_melt$No_Consumption_AFD
  df$Vulnerability_Value <- Vul_melt$Vulnerability_Value
  df$No_Consumption_Vulnerability <- Vul_melt$No_Consumption_Vulnerability
  
  
  colnames(df)[which(names(df) == "x")] <- "Gauge"
  colnames(df)[which(names(df) == "variable")] <- "Scenario"
  
  df$river_mile <- round(df$river_mile*1.60934,0)
  colnames(df)[which(names(df) == "river_mile")] <- "River_Kilometer"
  
  df <-df[,c(1,2,4,6,5,3,8,7,10,9)]
  
  return(df)
}

PLot_Single_Gauge <- function(Melted.Data,Gauge_Name) {
  
  df <- dplyr::filter(Melted.Data, Gauge == Gauge_Name)
  df$Consumption <- lapply(df$Consumption, as.character)
  df[df == '10% ADF (.255 cms)'] <- 10
  df[df == '50% ADF (1.28 cms)'] <- 50
  df[df == '100% ADF (2.55 cms)'] <- 100
  df$Consumption <- unlist(df$Consumption, use.names=FALSE)
  
  # Color blind pallete for plotting
  cbbPalette <- c("#E69F00","#56B4E9", "#009E73")
  
  ggplot(df, aes(x= Consumption))+
    geom_line(aes(x= Consumption,y = POF_Value, group = Scenario,color = Scenario, linetype= 'POF_Value'))+
    geom_line(aes(x= Consumption,y = Resilience_Value, group = Scenario,color = Scenario, linetype= "Resilience_Value"))+
    geom_line(aes(x= Consumption,y = Vulnerability_Value, group = Scenario,color = Scenario, linetype= "Vulnerability_Value"))+
    theme_bw()+
    ggtitle(Gauge_Name)+
    xlab("Consumption (% ADF)") + ylab("Performance Criteria (%)")
  
}

Plot_Single_Guage_and_Scenario <-function(Melted.Data,Gauge_Name,Consumption_Scenario) {
  
  df <- dplyr::filter(Melted.Data, Gauge == Gauge_Name)
  df <- dplyr::filter(df, Scenario == Consumption_Scenario)
  
  df$Consumption <- lapply(df$Consumption, as.character)
  df[df == '10% ADF (.255 cms)'] <- 10
  df[df == '50% ADF (1.28 cms)'] <- 50
  df[df == '100% ADF (2.55 cms)'] <- 100
  df$Consumption <- unlist(df$Consumption, use.names=FALSE)
  
  # Color blind pallete for plotting
  cbbPalette <- c("#E69F00","#56B4E9", "#009E73")
  
  ggplot(df, aes(x= Consumption))+
    geom_line(aes(x= Consumption,y = POF_Value,color = 'POF_Value'))+
    geom_line(aes(x= Consumption,y = Resilience_Value,color = "Resilience_Value"))+
    geom_line(aes(x= Consumption,y = Vulnerability_Value,color = "Vulnerability_Value"))+
    theme_bw()+
    ggtitle(paste(Gauge_Name,Consumption_Scenario))+
    xlab("Consumption (% ADF)") + ylab("Performance Criteria (%)")+
    ylim(c(0,100))
  
}

Plot_Single_Gauge_and_Metric <-function(Melted.Data,Gauge_Name,Metric_Value) {
  
  df <- dplyr::filter(Melted.Data, Gauge == Gauge_Name)
  colnames(df)[which(names(df) == Metric_Value)] <- "value"
  
  df$Consumption <- lapply(df$Consumption, as.character)
  df[df == '10% ADF (.255 cms)'] <- 10
  df[df == '50% ADF (1.28 cms)'] <- 50
  df[df == '100% ADF (2.55 cms)'] <- 100
  df$Consumption <- unlist(df$Consumption, use.names=FALSE)
  
  # Color blind pallete for plotting
  cbbPalette <- c("#E69F00","#56B4E9", "#009E73")
  
  ggplot(df, aes(x= Consumption))+
    geom_line(aes(x= Consumption,y = value,group=Scenario,color = Scenario))+
    theme_bw()+
    ggtitle(paste(Gauge_Name,Metric_Value))+
    xlab("Consumption (% ADF)") + ylab("Performance metric (%)")+
    ylim(c(0,100))
  
}


