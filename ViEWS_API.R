
#install.packages(c("httr","jsonlite"))
library(httr)
library(jsonlite)
#
#   Fatality predictions for next 36 months
#

data <- GET( link )

fatalities_data1 <- fromJSON(rawToChar(data$content))
fatalities_data_formatted <- fatalities_data1$data

next_API_address <- fatalities_data1$next_page

while (next_API_address!=""){
   
   # Get data from API
   fatalities_data <- GET(paste(next_API_address))
   fatalities_data1 <- fromJSON(rawToChar(fatalities_data$content))
   fatalities_data_formatted <- rbind(fatalities_data1$data, fatalities_data_formatted)
   # Get API for next page from current API
   next_API_address <- fatalities_data1$next_page
   
}

save( fatalities_data_formatted, file = "../Data/RawData/prio_pred_fatalities.rdata" )


