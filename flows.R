################################################################################
################################################################################
##                                                                            ##
##                            Update Flow Data                                ##
##                                                                            ##
################################################################################
################################################################################

### update refugees package to get the latest flow data from ASR
update.packages( 'refugees' )

### load libraries
library( 'dplyr' )
library( 'lubridate' )
library( 'refugees' )
library( 'tidyr' )

### generate data set 
dat <- refugees::flows %>% 
       filter( year %in% c( 2000:max( .$year ))) %>% 
       rename( iso_o = coo_iso, iso_d = coa_iso ) %>% 
       mutate_all( ~ifelse( is.na(.), 0, . )) %>% 
       mutate( newarrivals = refugees + asylum_seekers + oip ) %>% 
       select( iso_o, iso_d, year, newarrivals )
 
if( nowcasting == TRUE ){
   ## read in nowcasting data
   load( "../Data/RawData/nowcasting_flows.Rdata" )
   flow_data <- final_nowcasting_flow_data_23
   ## set new year 
   new_year <- max( dat$year ) + 1
   ## create data set 
   now_c <- flow_data %>% 
            mutate( year = year( date )) %>% 
            filter( year == new_year ) %>% 
            rename( iso_o = iso3_origin, iso_d = iso3_asylum ) %>% 
            group_by( iso_o, iso_d, year ) %>% 
            summarise( newarrivals = sum( arrivals )) %>% 
            ungroup()
   
   dat <- dat %>% 
          bind_rows( now_c )
}

dat$year <- as.factor( dat$year )
   
save( dat, file = "../Data/WorkData/flows.Rdata" )   

