################################################################################
################################################################################
##                                                                            ##
##                           Update Data next Year                            ##
##                                                                            ##
################################################################################
################################################################################

#### load packages 
library( dplyr )
library( readr )
library( haven )

#### read in data 
flow_old <- read_dta( "../Data/RawData/UNHCR_Flow.dta" )
gravity_old <- read.csv( "../Data/RawData/gravity.csv" )
flow_new <- read.csv( "../Data/RawData/Flow_Data_1962_2021.csv" )
stock_old <- read_dta( "../Data/RawData/UNHCR_Stock.dta" )
stock_new <- read_dta( "../Data/RawData/stock_data_1951_2021.dta" )
vda_old <- read_dta( "../Data/RawData/VDA_2017_2020.dta" )
vda_new <- read_dta( "../Data/RawData/VDA2021.dta" )


#### update data sets 
### flow data 
flow <- flow_new %>% group_by( originiso, asylumiso, year ) %>% 
                     summarise( newarrival = sum( count )) %>% 
                     ungroup() %>%
                     select( originiso, asylumiso, year, newarrival ) %>% 
                     rename( iso_o = originiso, 
                             iso_d = asylumiso ) %>% 
                     mutate( Id = paste0( iso_o, iso_d )) %>% 
                     filter( year %in% c( 2000:2021 ) & iso_o != iso_d ) %>% 
                     left_join( flow_old, by = c( "iso_o", "iso_d", "year" ), suffix = c( "", ".y" )) %>% 
                     select( -c( newarrival.y, Id.y ))

### stock data 
stock_new <- stock_new %>% 
             select( year, countryorigincode, countryasylumcode, ref, asy, vda ) %>% 
             rename( iso_o = countryorigincode, iso_d = countryasylumcode ) %>% 
             mutate( Id = paste0( iso_o, iso_d )) %>%
             filter( year == 2021 )              
   
stock <- stock_old %>% bind_rows( stock_new ) %>% 
                       arrange( iso_o )
   
   
### vda data
vda_new <- vda_new %>% 
           rename( iso_o = originiso, iso_d = asylumiso, new_displaced = count, VDA = vda ) %>%
           select( iso_o, iso_d, year, new_displaced, VDA ) 

vda <- vda_old %>% select( -percVDA ) %>% 
                   bind_rows( vda_new ) %>% 
                   arrange( iso_o )
           
   
write_csv( flow, file = "../Data/RawData/flowdata.csv" )
write_csv( stock, file = "../Data/RawData/stockdata.csv" )
write_csv( vda, file = "../Data/RawData/vdadata.csv" )

year_tots_flow_new <- flow %>% group_by( year ) %>% summarise( tot = sum( newarrival ))
year_tots_stock_new <- stock %>% group_by( year ) %>% summarise( tot_ref = sum( ref ), tot_asy = sum( asy ))

test <- list( year_tots_flow_new, year_tots_stock_new )
save( test, file = "../Results/checks.Rdata" )

