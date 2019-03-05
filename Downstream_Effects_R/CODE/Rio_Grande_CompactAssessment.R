
# Determine the needed marcial flow as a function of the otowi flow data
needed_marcial_flow <- function(otowi_index,single_otowi_flow_value) {
  
  # create a vector that can b filled with needed flow and the rest zeros
  a <- c()
  
  # seperate otowi index into otow flow(cuts) and resulting san marciaal flow (results)
  cuts <- otowi_index$Otowi
  results <- otowi_index$SanMarcial
  
  # fills the a vector with the result or zeros
  for(i in 1:length(cuts)) {
    slope <- (results[i+1]-results[i])/(cuts[i+1]-cuts[i])
    
    a[i] <- ifelse(single_otowi_flow_value == cuts[i],results[i], 
                   ifelse(single_otowi_flow_value > cuts[i] & single_otowi_flow_value < cuts[i+1],
                          (slope*(single_otowi_flow_value-cuts[i])+results[i]),0))
    
  }
  
  # takes the max value (only non zero) which is the needed flow
  needed_marcial_flow <- max(a)
}

Rio_Grande_Compact_35YearAssessment <- function(otowi_flow, uniformconsumptionCFS) {
  
  # Requires above function and function from CleanData
  #source('./CODE/Rio_Grande_CompactAssessment.R') 
  #source('./CODE/Rio_Grande_CleanData.R') 
  
  # Read In Otowi index
  otowi_index <- read.csv("./DATA/Rio_Grande_Data/Otowi_index.csv")
  
  # Clean Data
  otowi_cleaned <- Clean_Data_40yr(otowi_flow)
  otowi <- filter(otowi_cleaned, month != 7,month != 8, month != 9)
  
  # Read in then clean San Marcial
  san_marcial_flow <- read.csv("./DATA/Rio_Grande_Data/rio_san_marcial.csv", skip = 2)
  san_marical_cleaned <- Clean_Data_40yr(san_marcial_flow)
  san_marcial <- filter(san_marical_cleaned, month != 7,month != 8, month != 9)
  
  # Create Vector afer 90 CFS Consumption
  san_marcial_minus90 = san_marcial
  san_marcial_minus90$streamflow = san_marcial_minus90$streamflow-uniformconsumptionCFS
  san_marcial_minus90$streamflow <- ifelse(san_marcial_minus90$streamflow < 0,0,san_marcial_minus90$streamflow)
  
  # initialize yearly streamflow vectors
  yearly_streamflow_sanmarcial <- c()
  yearly_streamflow_otowi <- c()
  yearly_streamflow_sanmarcial_minus90 <- c()
  years_vec <- c()
  
  # Create Streanflow vectors
  for( i in 1:35) {
    marcial_in_loop <- filter(san_marcial,year == 1979 + i )
    otowi_in_loop <- filter(otowi, year == 1979+i)
    marcial_minus90_in_loop <- filter(san_marcial_minus90, year == 1979+i)
    
    yearly_streamflow_sanmarcial[i] <- sum(marcial_in_loop$streamflow)
    yearly_streamflow_otowi[i] <- sum(otowi_in_loop$streamflow)
    yearly_streamflow_sanmarcial_minus90[i] <- sum(marcial_minus90_in_loop$streamflow)
    years_vec[i] <- unique(marcial_in_loop$year)
  }
  
  # Rounabout way to convert to Acre-Ft
  # Data must be in acre feet to determine required san marcial flow
  sanmarcial_acreft <- yearly_streamflow_sanmarcial * 273 * 86400 *.0000229568 / 1000000 
  otowi_acreft <- yearly_streamflow_otowi * 273 * 86400 *.0000229568 / 1000000
  sanmarcial_minus90_acreft <- yearly_streamflow_sanmarcial_minus90 * 273 * 86400 *.0000229568 / 1000000
  
  # determined required flow at Marcial Guage for each year to meet compact
  marcial_required <- c()
  for( i in 1:35) {
    marcial_required[i] <- needed_marcial_flow(otowi_index,otowi_acreft[i])
  }
  
  # Convert acre feet to meters cubed
  sanmarcial_acreft_SI <- sanmarcial_acreft*1230
  marcial_required_SI <- marcial_required*1230
  sanmarcial_minus90_SI <- sanmarcial_minus90_acreft*1230
  
  # PLot comparison of marcial streamflow to required marcial streamflow
  plot( years_vec[1:35],sanmarcial_acreft_SI, type = "b", col = "blue",
        xlab = "Years",ylab = "Streamflow (cubic meters)", pch =19, ylim = c(500,500000))
  points(years_vec[1:35],marcial_required_SI, type = "b", pch =19)
  points(years_vec[1:35],sanmarcial_minus90_SI, type = "b", col = "red", pch = 19)
  legend('topright', legend = c("San Marcial Flow","San Marcial Flow with Uniform 100% ADF Consumption", "Required San Marcial Flow"),
         pch = 19, col = c("blue", 'red',1), cex = 1)

}
  
  
  
  
  
  
  
