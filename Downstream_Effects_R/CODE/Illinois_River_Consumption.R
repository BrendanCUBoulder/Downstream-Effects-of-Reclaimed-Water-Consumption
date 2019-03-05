################################################################################
#
# Calculates the hypothetical downstream flow given historical data and a 
#    consumption pattern.
#
# df = data frame of guage
# Patterns = data frame dictionary of upstream consumption patterns
# Type = String of which pattern to use
# Scaler = Numeric on which to scale the pattern
#
# Zac Barker, December 2016
#
################################################################################

CONSUMPTION <- function(df, scenario, scaler, slope, criticalLevel, immersion, 
                        price){
     
     # Format month as character
     scenario$Month <- as.character(scenario$Month)
     
     # Heading titles
     heading1 <- paste0(colnames(scenario)[2],"Consumption")
     heading2 <- paste0(colnames(scenario)[2],"Flow")
     heading3 <- paste0(colnames(scenario)[2],"Stage")
     heading4 <- paste0(colnames(scenario)[2],"AvgValue")
     heading5 <- paste0(colnames(scenario)[2],"MaxValue")
     heading6 <- paste0(colnames(scenario)[2],"MinValue")
     
     # Calculate consumption based on patterns
     df[,heading1] <- scenario[,2][match(df$Month, scenario$Month)]
     
     # Scale consumption
     df[,heading1] <- df[,heading1]*scaler
     
     # Subtract consumption from flow
     df[,heading2] <- df$Flow - df[,heading1]
     
     # Calculate new stage
     df[,heading3] <- df$Stage-(df[,heading1]*slope)
     
     # Calculate AVG value lost
     cost <- (criticalLevel - df[,heading3]) * immersion * price[1]
     df[,heading4] <- ifelse(df[,heading3] >= criticalLevel, 0, cost)
     
     # Calculate MAX value lost
     cost <- (criticalLevel - df[,heading3]) * immersion * price[2]
     df[,heading5] <- ifelse(df[,heading3] >= criticalLevel, 0, cost)
     
     # Calculate MIN value lost
     cost <- (criticalLevel - df[,heading3]) * immersion * price[3]
     df[,heading6] <- ifelse(df[,heading3] >= criticalLevel, 0, cost)
     
     return(df)
}