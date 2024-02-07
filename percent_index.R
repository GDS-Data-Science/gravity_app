################################################################################
################################################################################
##                                                                            ##
##                  Generate percentages and index0asylum                     ##
##                                                                            ##
################################################################################
################################################################################


### load pacakages
library( CountryRodes )
library( dplyr )
library( readr )
library( refugees )
library( tidyr )


################################ index0asylum ##################################
### load data
# index from previous years 
load( "../Data/WorkData/impuData.Rdata" )
dat_index <- est_dat

## turn year into numeric variable 
dat_index$year <- as.numeric( as.character( dat_index$year ))

## select relevant variables and years
dat_index <- dat_index %>% 
             select( iso_o, iso_d, year, index0asylum ) %>% 
             filter( year %in% c( 2000:( last_year - 1 )))

## add another row to each country pair for new year
dat_index_ <- dat_index %>% 
              group_by( iso_o, iso_d ) %>%
              slice( n()) %>%
              mutate( year = year + 1 ) %>%
              bind_rows( dat_index, .) %>% 
              arrange( iso_o, iso_d, year )


## if nowcasting is FALSE, new index0asylum value replaces old values
if( nowcasting == FALSE ){
   # latest year for index0asylum
   path <- paste0( "../Data/RawData/ASR_pop/asr-refugees-", last_year, ".csv" )
   asr_index <- read_csv2( path )
   # replace old values of index0asylum with updated values
   dat_index_$index0asylum[ dat_index_$year == last_year ] <-
      ifelse( asr_index$groupRecognition > 0 | asr_index$temporaryProtection > 0, 1, 0 )
}

## turn year back to factor
dat_index$year <- as.factor( dat_index$year )

## save index0asylum values 
save( dat_index, file = "../Data/WorkData/asylum_index.Rdata" )


############################ acceptance percentages ############################ 

### get previous percentages 
load( "../Data/WorkData/stocks.Rdata" )

## generate data frames with same structure 
perc_dat <- lapply( stocks_dat, function(x) select( x, iso_o, iso_d, year, percVDA, 
                                                    deci_rate_d, deci_posi_rate_o ))
## remove all extra rows except for first 
perc_dat <- lapply( perc_dat, function(x){ x %>% 
                                           group_by( iso_o, iso_d ) %>%
                                           slice_head( n = 1 )})
## unlist list 
perc_dat <- do.call( rbind, perc_dat )

## add another row to each country pair for new year
perc_dat_ <- perc_dat %>% 
             group_by( iso_o, iso_d ) %>%
             slice( n()) %>%
             mutate( year = year + 1 ) %>%
             bind_rows( perc_dat, .) %>% 
             arrange( iso_o, iso_d, year )

if( nowcasting == FALSE ){
   # read in latest ASR data 
   path <- paste0( "../Data/RawData/ASR_pop/asr-rsd-", last_year, ".csv" )
   asr_perc <- read_csv2( path ) 
   
   # positive decision rates by country of origin
   deci_posi_rate <-  asr_perc %>% 
                      filter( is.na( ageRange )) %>% 
                      group_by( origin ) %>% 
                      summarise( deci_posi_rate_o_new = round(( sum( recognizedConvMandate, na.rm = T ) + 
                                                                sum( recognizedOther, na.rm = T ))/
                                                             (( sum( totalDecided, na.rm = T ))), 6 )) %>% 
                      ungroup() %>% 
                      mutate( deci_posi_rate_o_new = ifelse( deci_posi_rate_o_new > 1, 1, deci_posi_rate_o_new )) %>%  
                      mutate( year = last_year ) %>%  
                      filter( !is.na( deci_posi_rate_o_new )) %>%  
                      mutate( iso_o = iso_codes( origin, cm = "proGres_alpha3", type = "iso3_alpha3" ), 
                              iso_o = ifelse( origin == "KOS", "XKX", iso_o )) %>%  
                      filter( !is.na( iso_o )) %>% 
                      select( -origin )
   
   # merge with old data
   perc_dat_  <-      perc_dat_ %>% 
                      left_join( deci_posi_rate, by = c( "iso_o", "year" )) %>% 
                      mutate( deci_posi_rate_o = ifelse( !is.na( deci_posi_rate_o_new ), 
                                                                 deci_posi_rate_o_new, deci_posi_rate_o )) %>% 
                      select( -c( deci_posi_rate_o_new ))

   # positive decision rate by country of asylum 
   deci_rate  <-      asr_perc %>%  
                      filter( is.na( ageRange )) %>%  
                      group_by( asylum ) %>%  
                      summarise( deci_rate_d_new = round( sum( totalDecided, na.rm = T )/ 
                                                        ( sum( totalStartYear, na.rm = T ) + 
                                                          sum( appliedDuringTheYear, na.rm = T )), 6 )) %>%  
                      ungroup() %>% 
                      mutate( deci_rate_d_new = ifelse( deci_rate_d_new > 1, 1, deci_rate_d_new )) %>%  
                      mutate( year = last_year ) %>%  
                      filter( !is.na( deci_rate_d_new )) %>% 
                      mutate( iso_d = iso_codes( asylum, type = "iso3_alpha3" )) %>%  
                      select( -asylum ) 
   
   # merge with old data
   perc_dat_  <-      perc_dat_ %>% 
                      left_join( deci_rate, by = c( "iso_d", "year" )) %>% 
                      mutate( deci_rate_d = ifelse( !is.na( deci_rate_d_new ), 
                                            deci_rate_d_new, deci_rate_d )) %>% 
                      select( -c( deci_rate_d_new ))
   
   # percent of Venezuelans
   df_VDA   <-        refugees::flows %>% 
                      filter( year == last_year ) %>% 
                      replace_na( list( refugees = 0, asylum_seekers = 0, oip = 0 )) %>% 
                      group_by( coa_iso ) %>% 
                      mutate( perc_VDA_new = oip/( refugees + asylum_seekers + oip )) %>% 
                      replace_na( list( perc_VDA_new = 0 )) %>% 
                      select( coo_iso, coa_iso, year, perc_VDA_new )
   
   # merge with old data
   perc_dat_  <-      perc_dat_ %>% 
                      left_join( df_VDA, by = c( "iso_o" = "coo_iso", "iso_d" = "coa_iso", "year" )) %>% 
                      mutate( percVDA = ifelse( !is.na( perc_VDA_new ), perc_VDA_new, percVDA )) %>% 
                      select( -c( perc_VDA_new ))
}


perc_dat_index <- perc_dat_ %>% 
                  left_join( dat_index_, by = c( "iso_o", "iso_d", "year" )) %>% 
                  replace_na( list( index0asylum = 0 ))

save( perc_dat_index, file = "../Data/WorkData/perc_index.Rdata" )













