################################################################################
#
# Simple plotting function for upstream consumption patterns
#
# Zac Barker, January 2017
#
################################################################################

library("ggplot2")
library("reshape2")

PLOT_PATTERNS <- function(df){
     
     dd <- melt(df, id = "Month")
     
     # make months an ordered factor
     dd$Month <- factor(dd$Month, levels = dd$Month)
     
     # Plot
     patterns <- ggplot(dd)+ 
          geom_bar(aes(x=Month, y=value, fill=variable),stat = "identity", position = "identity", alpha = .4, width = 1)+
          theme_bw()+
          xlab("Month")+
          ylab("Relative Consumption")+
          ggtitle("Consumption Patterns")+
          theme(legend.justification=c(1,1), 
                legend.position=c(1,1),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(2)),
                axis.text = element_text(size = rel(1.2)),
                axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(patterns)
     
     return(dd)
     
}