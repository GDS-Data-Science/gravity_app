################################################################################
################################################################################
##                                                                            ##
##                         Calculate Stockfigures                             ##
##                                                                            ##
################################################################################
################################################################################

### load packages 
library( dplyr )
library( httr )
library( jsonlite )
library( lubridate )
library( purrr )
library( tidyr )
library( readr )
library( refugees )
library( zoo )


### read in data 
## stock data from refugees package 
stocks <- refugees::population
## stocks from nowcasting
apiKey = "CApUoilQ0HpCOOakja03vNT5hq8EwtGp"
nowcasting_api <- GET( "https://rstudio.unhcr.org/content/ff9c6ee1-975e-414d-a061-1febc9151c00/data_csv",
                   add_headers( Authorization = paste( "Key", apiKey )))
now_stocks <- content( nowcasting_api )
## percentages and index 
load( "../Data/WorkData/perc_index.Rdata" )


### organise data 
## refugee package extract stocks 
stocks <- stocks %>% 
          select( coo_iso, coa_iso, year, refugees, asylum_seekers, oip ) %>% 
          filter( coo_iso != "UNK" & year %in% c(( last_year - 3 ):last_year )) %>% 
          mutate( oip = replace_na( oip, 0 )) %>% 
          rename( iso_o = coo_iso, iso_d = coa_iso ) %>% 
          filter( iso_o != iso_d )

### add nowcasting stocks
if( nowcasting == TRUE ){
   # generate new last_year 
   last_year <- last_year + 1
   last_date <- as.Date( paste0( last_year, "-12-31" )) 
   # generate nowcasting stocks 
   now_stocks <- now_stocks %>% 
                 filter( date == last_date & iso3_origin != iso3_asylum & iso3_origin != "UKN" ) %>% 
                 mutate( year = year( date ), 
                         oip  = 0 ) %>% 
                 select( iso3_origin, iso3_asylum, year, refugees, asylum_seekers, oip ) %>% 
                 rename( iso_o = iso3_origin, iso_d = iso3_asylum )
   stocks <- stocks %>% 
             bind_rows( now_stocks )
}

### add index0asylum and percentages 
stocks <- stocks %>% 
          left_join( perc_dat_index, by = c( "iso_o", "iso_d", "year" )) %>% 
          arrange( iso_o, iso_d, year ) %>% 
          filter 

### data cleaning 
stocks <- stocks %>% 
          ## percentage VDA
          mutate( percVDA = ifelse( is.na( percVDA ) & oip == 0, 0, stocks$percVDA )) %>% 
          group_by( iso_o, iso_d ) %>% 
          mutate( percVDA = zoo::na.locf( percVDA, na.rm = FALSE )) %>% 
          ungroup() %>% 
          ## deci_rate_d
          group_by( iso_d, year ) %>% 
          mutate( deci_rate_d = zoo::na.locf( deci_rate_d, na.rm = FALSE )) %>% 
          mutate( deci_rate_d = zoo::na.locf( deci_rate_d, na.rm = FALSE, fromLast = TRUE )) %>% 
          ungroup() %>% 
          group_by( iso_o, iso_d ) %>% 
          mutate( deci_rate_d = zoo::na.locf( deci_rate_d, na.rm = FALSE )) %>% 
          mutate( deci_rate_d = zoo::na.locf( deci_rate_d, na.rm = FALSE, fromLast = TRUE )) %>% 
          replace_na( list( deci_rate_d = 0 )) %>% 
          group_by( iso_o, year ) %>% 
          mutate( deci_posi_rate_o = zoo::na.locf( deci_posi_rate_o, na.rm = FALSE )) %>% 
          mutate( deci_posi_rate_o = zoo::na.locf( deci_posi_rate_o, na.rm = FALSE, fromLast = TRUE )) %>% 
          ungroup() %>% 
          group_by( iso_o, iso_d ) %>% 
          mutate( deci_posi_rate_o = zoo::na.locf( deci_posi_rate_o, na.rm = FALSE )) %>% 
          mutate( deci_posi_rate_o = zoo::na.locf( deci_posi_rate_o, na.rm = FALSE, fromLast = TRUE )) %>% 
          mutate( index0asylum = zoo::na.locf( index0asylum, na.rm = FALSE )) %>% 
          mutate( index0asylum = zoo::na.locf( index0asylum, na.rm = FALSE, fromLast = TRUE )) %>%
          replace_na( list( deci_posi_rate_o = 0, index0asylum = 0 )) %>%
          ungroup() %>% 
          mutate( deci_rate_d = ifelse( deci_rate_d > 1, 1, deci_rate_d ), 
                  deci_posi_rate_o = ifelse( deci_posi_rate_o > 1, 1, deci_posi_rate_o ))  


### generate four stock prediction data sets 
## copy last row in each country pair group three times 
stocks_T <- stocks %>%
            arrange( iso_o, iso_d, year ) %>% 
            group_by( iso_o, iso_d ) %>%
            slice_tail( n = 1 ) %>%  
            bind_rows( replicate( 3, ., simplify = FALSE )) %>% 
            mutate( year = first( year ) + row_number() - 1 ) %>%
            bind_rows( stocks ) %>% 
            ungroup() %>% 
            arrange( iso_o, iso_d, year ) %>% 
            distinct() %>% 
            as.data.frame()

## generate list with four data sets 
stocks_dat <- seq_fun( df = stocks_T, t = "year", train_window = 4, 
                      test_window = 0, fix_start = FALSE )
stocks_dat <- stocks_dat$train 

## remove country pair groups with less than four rows
stocks_dat <- stocks_dat %>% 
              purrr::map( ~ group_by( .x, iso_o, iso_d ) %>%
              filter( n() >= 4 ))

## Set preditive cells to NA
stocks_dat <- stocks_dat %>%
      purrr::map( ~group_by( .x, iso_o, iso_d ) %>%
      mutate( 
         refugees = case_when(
            row_number() == n() - 2 ~ NA_real_,
            row_number() == n() - 1 ~ NA_real_,
            row_number() == n() ~ NA_real_,
            TRUE ~ refugees ), 
         asylum_seekers = case_when(
            row_number() == n() - 2 ~ NA_real_,
            row_number() == n() - 1 ~ NA_real_,
            row_number() == n() ~ NA_real_,
            TRUE ~ asylum_seekers ), 
         oip = case_when(
            row_number() == n() - 2 ~ NA_real_,
            row_number() == n() - 1 ~ NA_real_,
            row_number() == n() ~ NA_real_,
            TRUE ~ oip )) %>% 
         ungroup())  
         

save( stocks_dat, file = "../Data/WorkData/stocks.Rdata" )






   

