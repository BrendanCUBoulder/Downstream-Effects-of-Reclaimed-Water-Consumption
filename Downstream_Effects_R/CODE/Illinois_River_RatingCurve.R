################################################################################
#
# Derives the rating curve from flow and stage data
#
# Zac Barker, December 2016
#
################################################################################

library("ggplot2")

RATING_CURVE <- function(guage_name, df){
     
     # Subset lower 50th percentile of stream flow
     q <- quantile(df$Flow, .5, na.rm = T)
     df <- df[df$Flow < q,]
     
     # Fit linear model to subset
     reg = lm(Stage ~ Flow, data = df)
     Slope_Intercept = c(as.numeric(reg$coefficients[2]),as.numeric(reg$coefficients[1]))
     
     # Construct equation
     eq <- as.character(as.expression(paste0("y = ",format(reg$coefficients[2], digits =2),"x + ",
                   format(reg$coefficients[1], digits=3),sep ="")))

     # Define where the equation should display
     yPos1 <- max(df$Flow, na.rm = T)*Slope_Intercept[1]+Slope_Intercept[2]
     yPos2 <- max(df$Stage, na.rm = T)
     if (yPos1 < yPos2){
          yPos <- yPos2
     } else {
          yPos <- yPos1
     }

     # Construct graph
     curve <- ggplot(df) + geom_point(aes(Flow,Stage)) +
          geom_smooth(method=lm, aes(Flow,Stage))+
          theme_bw()+
          xlab("Flow  (CFS)")+
          ylab("Water Surface Elevation (ft)")+
          ggtitle(paste(guage_name, "Rating Curve"))+
          geom_text(aes(x = 0, y = yPos, hjust = 0, vjust = 1, label = eq, parse = T), size = rel(5))+
          theme(legend.justification=c(1,1),
                legend.position=c(1,1),
                legend.title=element_blank(),
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(2)),
                axis.text = element_text(size = rel(1.2)),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))

     print(curve)
     
     slope <- Slope_Intercept[1]
     return(slope)
     
}