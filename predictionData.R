################################################################################
################################################################################
##                                                                            ##
##                         Generate prediction data set                       ##
##                                                                            ##
################################################################################
################################################################################


#### load packages 
library( Amelia )
library( CountryRodes )
library( haven )
library( readr )
library( readxl )
library( dplyr )
library( tibble )
library( tidyr )

#### read in data sets 
country_distances <- read_dta( "../Data/RawData/dist_cepii.dta" )
country_area_language <- read_dta( "../Data/RawData/geo_cepii.dta" )
country_pol_index <- read_excel( "../Data/RawData/Country_and_Territory_Ratings_and_Statuses_FIW1973-2021.xlsx", 
                                 sheet = 2, skip = 1 )
country_gdp_pp <- read_excel( "../Data/RawData/IMF2.xlsx" )
country_population <- read_excel( "../Data/RawData/Pop_and_GDP_2019.xlsx", sheet = 2, na = "." )
country_gdp_ppp <- read_excel( "../Data/RawData/Pop_and_GDP_2019.xlsx", sheet = 3 )
country_cpi <- read_excel( "../Data/RawData/WEOOct2020all.xlsx", n_max = 8776  )
country_geographics <- read_dta( "../Data/RawData/SPATIAL.dta" )
country_conflict <- read_dta( "../Data/RawData/ged201.dta" )
country_newarrival <- read_csv( "../Data/RawData/flowdata.csv" )

#### clean data sets 
## country_distances
# correct iso codes
country_distances$iso_o <- iso_codes( country_distances$iso_o, cm = 'iso3_alpha3', type = "iso3_alpha3" )
# correct iso codes
country_distances$iso_d <- iso_codes( country_distances$iso_d, cm = 'iso3_alpha3', type = "iso3_alpha3" )
# select variables and filter rows
country_distances <- country_distances %>% 
                     select( iso_o, iso_d, contig, comlang_off, comlang_ethno, 
                             colony, comcol, col45, smctry, dist ) %>% 
                     filter( iso_o != iso_d ) %>%
                     filter( !( iso_o %in% c( "YUG", "ZAR" )) & !( iso_d %in% c( "YUG", "ZAR" ))) %>% 
                     mutate( id = paste0( iso_o, iso_d )) %>% 
                     distinct( id, .keep_all = TRUE ) %>% 
                     select( -id )
                     
## country_area
# correct country codes
country_area_language$iso <- iso_codes( country_area_language$iso2, cm = 'iso3_alpha2', type = "iso3_alpha3" )
country_area_language$iso[ country_area_language$country == "Namibia" ] <- "NAM"
country_area_language$iso[ country_area_language$country == "Serbia and Montenegro" ] <- "SRB"
country_area_language$iso[ country_area_language$country == "Congo (Democratic Republic of the)" ] <- "COD"
# select variables and filter rows
country_area <- country_area_language %>% 
                select( iso, country, area, landlocked ) %>% 
                distinct() %>% 
                filter( country != "Sao Tome and Principe" )

## country_pol_index
# create new variable names
names <- c( "country" )
for( i in 1973:2020 ){
   tmp <- c( paste0( "PR_", i ), paste0( "CL_", i ), paste0( "Status_", i ))
   names <- c( names, tmp )
}
names( country_pol_index ) <- names 

# remove status and turn from wide to long format
country_pol_index <- country_pol_index %>% 
                     slice( -1 ) %>% 
                     select( -contains( "Status" )) %>% 
                     pivot_longer( cols = 2:97, 
                                   names_to = c( ".value", "year" ),
                                   names_pattern = "(.*)_(.*)" )
# turn character to numeric
country_pol_index <- within( country_pol_index, {
   year = as.numeric( year )
   PR = as.numeric( PR )
   CL = as.numeric( CL )
})

# create and correct iso codes 
country_pol_index$iso <- iso_codes( country_pol_index$country, type = "iso3_alpha3" )
country_pol_index$iso[ country_pol_index$country == "Congo (Kinshasa)" ] <- "COD"
country_pol_index$iso[ country_pol_index$country == "Congo (Brazzaville)" ] <- "COG"
country_pol_index <- filter( country_pol_index, !is.na( country_pol_index$iso ))

## country_gdp_pp
# select variables and turn format from wide to long
country_gdp_pp <- country_gdp_pp %>% 
                  select( ISO, Country, 4:28 ) %>% 
                  pivot_longer( cols = starts_with( "A" ), 
                  names_to = "year", 
                  names_prefix = "A",
                  values_to = "GDP_PP" )

# turn character to numeric
country_gdp_pp <- within( country_gdp_pp, {
   GDP_PP = as.numeric( GDP_PP )
   year = as.numeric( year )
}) 

## country_population
# select variables and turn format from wide to long
country_population <- country_population %>% 
                      select( ISO, Country, 5:29 ) %>% 
                      pivot_longer( cols = starts_with( "A" ), 
                                    names_to = "year", 
                                    names_prefix = "A",
                                    values_to = "pop" )
# turn character to numeric
country_population <- within( country_population, {
   pop = as.numeric( pop )
   year = as.numeric( year )
}) 


## country_gpd_ppp
# select variables and turn format from wide to long
country_gdp_ppp <- country_gdp_ppp %>% 
                   select( ISO, Country, 5:29 ) %>% 
                   pivot_longer( cols = starts_with( "A" ), 
                   names_to = "year", 
                   names_prefix = "A",
                   values_to = "GDP_PPP" ) 

# turn character to numeric
country_gdp_ppp <- within( country_gdp_ppp, {
   GDP_PPP = as.numeric( GDP_PPP )
   year = as.numeric( year )
}) 


## country_cpi
# select variables and turn format from wide to long
country_cpi <- country_cpi %>% 
               rename( subject_code = "WEO Subject Code" ) %>% 
               filter( subject_code == "PCPIEPCH" ) %>% 
               pivot_longer( cols = 10:55,
                             names_to = "year", 
                             values_to = "CPI" ) %>% 
               select( ISO, Country, year, CPI )

# turn character to numeric
country_cpi <- within( country_cpi, {
   CPI = as.numeric( CPI )
   year = as.numeric( year )
})

## country_geographics
# select variables and filter rows
country_geographics <- country_geographics %>% 
                       select( Origin, Year, island ) %>%
                       filter( !( Origin %in% c( "Bonaire", "Palestinian", "Saint Pierre et Miquelon", 
                                                 "Tibetan", "Yugoslavia" )))   
# create and correct iso codes 
country_geographics$iso <- iso_codes( country_geographics$Origin, type = "iso3_alpha3" )
country_geographics$iso[ country_geographics$Origin == "Dem. People's Korea Republic of" ] <- "PRK"
country_geographics$iso[ country_geographics$Origin == "Curasao" ] <- "CUW"
country_geographics$iso[ country_geographics$Origin == "Korea Republic of" ] <- "KOR"
country_geographics$iso[ country_geographics$Origin == "Macedonia" ] <- "MKD"
country_geographics$iso[ country_geographics$Origin == "Zaire" ] <- "COD"


## country_conflict
# create iso codes 
country_conflict$iso <- iso_codes( country_conflict$country, type = "iso3_alpha3" )

# select variables 
country_conflict <- country_conflict %>% 
                    select( iso, year, type_of_violence, best ) %>% 
                    group_by( iso, year ) %>% 
                    summarise( typeOfViolence = round( mean( type_of_violence ), 0 ), 
                               best_est = sum( best )) %>% 
                    ungroup() %>% 
                    mutate( idx = if_else( best_est >= 50, 1, 0 )) 

# create year count 
country_conflict <- country_conflict %>%
                    group_by( iso, grp = cumsum( idx == 0)) %>%
                    mutate( Nyear_conflict = cumsum( idx )) %>%
                    ungroup() %>%
                    select( -c( grp, idx )) 

## country_newarrival
# select variables 
country_newarrival <- country_newarrival %>% 
                      filter( !is.na( iso_o )) %>%    
                      select( iso_o, iso_d, year, Id, newarrival, index0asylum )


#### merge data sets 
# merge two dimensional matrices 
dat <- country_population %>% 
       left_join( country_cpi, by = c( "ISO", "year" )) %>%
       select( -c( Country.x, Country.y )) %>% 
       full_join( country_gdp_pp, by = c( "ISO", "year" )) %>% 
       select( -Country ) %>% 
       full_join( country_gdp_ppp, by = c( "ISO", "year" )) %>%
       left_join( country_geographics, by = c( "ISO" = "iso", "year" = "Year" )) %>% 
       left_join( country_area, by = c( "ISO" = "iso" )) %>% 
       left_join( country_pol_index, by = c( "ISO" = "iso", "year" )) %>% 
       left_join( country_conflict, by = c( "ISO" = "iso", "year" )) %>% 
       select( -c( Origin, country.x, country.y ))


#### data cleaning 
# replace missings with zeros 
impute.zero <- function(x) replace( x, is.na(x), 0 )
dat <- dat %>% mutate( typeOfViolence =
                        if_else( year < 2020 & best_est < 50, impute.zero( typeOfViolence ), typeOfViolence ),
                       best_est =
                        if_else( year < 2020, impute.zero( best_est ), best_est )) %>% 
               mutate( Nyear_conflict =
                        if_else( year < 2020 & best_est < 50, impute.zero( Nyear_conflict ), Nyear_conflict ))

# replace missings with value of 2020 
impute.2020 <- function(x) replace( x, c( 21:25 ), x[ c( 20 )])
dat <- dat %>% group_by( ISO ) %>% 
               mutate( typeOfViolence = impute.2020( typeOfViolence ), 
                       best_est = impute.2020( best_est ), 
                       Nyear_conflict = impute.2020( Nyear_conflict ),
                       CL = impute.2020( CL ),
                       PR = impute.2020( PR )) %>% 
               ungroup() 

#### create new variables
dat <- within( dat, {
   # dummy variables
   dead <- ifelse( best_est > 25, 1, 0 ) 
   Nyear_conf <- ifelse( Nyear_conflict == 0, 0, 1 ) 
   # log transformation
   dead_log <- ifelse( best_est == 0, 0, log( best_est ))
   Nyear_log <- ifelse( Nyear_conflict == 0, 0, log( Nyear_conflict ))
})

dat <- as.data.frame( dat )

#### imputation with Amelia
# set seed 
set.seed( 123 )

# create boundary matrix 
bounds <- matrix( c( 11, 1, 7, 
                     12, 1, 7 ), 
                  ncol = 3, byrow = TRUE )

# run amelia
impu <- amelia( dat, idvars = "Country", ts = "year", cs = "ISO", 
                sqrts = c( "GDP_PPP", "GDP_PP", "pop", "dead_log", "Nyear_log", 
                           "area", "best_est", "Nyear_conflict" ),
                noms = c(  "dead", "Nyear_conf","typeOfViolence", 
                           "island", "landlocked" ),
                bounds = bounds, p2s = 1 )

# round categorical variables to nearest value
impu <- transform( impu, 
                   CL = round( CL, 0 ), 
                   PR = round( PR, 0 ))

### evaluation 
# summary
summary( impu )

# density plot 
compare.density( impu, var = "GDP_PPP" )
compare.density( impu, var = "GDP_PP" )
compare.density( impu, var = "CL" )
compare.density( impu, var = "PR" )
compare.density( impu, var = "CPI" )
compare.density( impu, var = "Nyear_log" )
compare.density( impu, var = "dead_log" )


#### proceed merging
# merging function 
merge2 <- function( dat ){
# create three dimensional array
est_dat <- expand_grid( iso_o = unique( dat$ISO ), iso_d = unique( dat$ISO ), year = 2017:2024 ) %>% 
           filter( iso_o != iso_d ) %>% 
           left_join( dat, by = c( "iso_o" = "ISO", "year" )) %>%
           left_join( dat, by = c( "iso_d" = "ISO", "year" ), suffix = c( "_o", "_d" )) %>% 
           left_join( country_newarrival, by = c( "iso_o", "iso_d", "year" )) %>% 
           select( -Id ) %>% 
           mutate( newarrival = replace_na( newarrival, 0 ), 
                   Id = paste0( iso_o, iso_d )) %>%
           left_join( country_distances, by = c( "iso_o", "iso_d" )) 
}

# merging loop
impu_dat <- lapply( impu$imputations, merge2 )

### data cleaning and imputation 
## replace missings with mean values
# impute mean function 
impute.mean <- function(x) replace( x, is.na(x), round( mean( x, na.rm = TRUE ), 0 ))
# application of input.mean function 
cleanMean <- function( est_dat ){ 
est_dat <- est_dat %>% group_by( Id ) %>% 
                       mutate( contig = impute.mean( contig ),
                               comlang_off = impute.mean( comlang_off ),
                               comlang_ethno = impute.mean( comlang_ethno ),
                               colony = impute.mean( colony ),
                               comcol = impute.mean( comcol ),
                               col45 = impute.mean( col45 ),
                               smctry = impute.mean( smctry ),
                               dist = impute.mean( dist ), 
                               index0asylum = round( impute.mean( index0asylum ), 0 )) %>% 
                       ungroup() %>% 
                       mutate( contig = impute.mean( contig ),
                               comlang_off = impute.mean( comlang_off ),
                               comlang_ethno = impute.mean( comlang_ethno ),
                               colony = impute.mean( colony ),
                               comcol = impute.mean( comcol ),
                               col45 = impute.mean( col45 ),
                               smctry = impute.mean( smctry ),
                               dist = impute.mean( dist ), 
                               index0asylum = round( impute.mean( index0asylum ), 0 ))
}

# loop through list 
impu_clean <- lapply( impu_dat, cleanMean )

#### generate training and predictive data sets 
impu17 <- lapply( impu_clean, function(x) subset( x, year >= 2017 & year <= 2021 ))
impu22 <- lapply( impu_clean, function(x) subset( x, year >= 2021 ))

#### save data sets
save( impu17, file = "../Data/WorkData/impuData17.Rdata" )
save( impu22, file = "../Data/WorkData/impuData22.Rdata" )

stop()
################################################################################
##                                stock data                                  ##
################################################################################

#### read in data 
data_deci_o <- read_dta( "../Data/Rawdata/UNHCR_deci_posi_rate_o.dta" )
data_deci_d <- read_dta( "../Data/Rawdata/UNHCR_deci_rate_d.dta" )
data_vda <- read_csv( "../Data/Rawdata/vdadata.csv" )
data_flow <- read_csv( "../Data/Rawdata/flowdata.csv" )
data_stock <- read_csv( "../Data/Rawdata/stockdata.csv" )

#### create percVDA data
data_vda <- subset( data_vda, year == 2021 )
data_flow_sum <- data_flow %>% 
                 filter( year == 2021 & iso_d %in% data_vda$iso_d ) %>% 
                 group_by( iso_d ) %>% 
                 summarise( total = sum( newarrival, na.rm = TRUE ))

data_vda$percVDA <- data_vda$new_displaced/data_flow_sum$total 
data_vda <- select( data_vda, -c( year, new_displaced, VDA ))

#### correct problems with index0asylum 
data_deci_o$index0asylum[ data_deci_o$iso_o == "VEN" & data_deci_o$iso_d == "COL" ] <- 1 


#### merge data sets 
# merge 
dat_stock <- data_stock %>% 
             full_join( data_flow, by = c( "iso_o", "iso_d", "year" )) %>% 
             select( -c( Id.x, Id.y, index0asylum )) %>% 
             filter( !is.na( iso_o ) & !is.na( iso_d )) %>% 
             replace( is.na(.), 0 ) %>% 
             filter( year == 2021 ) %>% 
             group_by( iso_o, iso_d ) %>% 
             group_modify( ~ add_row( .x, year = 2022:2024 )) %>% 
             ungroup() %>% 
             left_join( data_vda, by = c( "iso_o", "iso_d" ))  %>%
             mutate( percVDA = replace( percVDA, is.na( percVDA ), 0 )) %>% 
             left_join( data_deci_d, by = c( "iso_d" )) %>% 
             left_join( data_deci_o, by = c( "iso_o", "iso_d" )) %>% 
             mutate( deci_rate_d = replace_na( deci_rate_d, mean( deci_rate_d, na.rm = TRUE )), 
                     deci_posi_rate_o = replace_na( deci_posi_rate_o, mean( deci_posi_rate_o, na.rm = TRUE )),
                     index0asylum = replace_na( index0asylum, round( mean( index0asylum, na.rm = TRUE ), 0 ))) %>%
             filter( iso_o != iso_d )
             
save( dat_stock, file = "../Data/WorkData/stock_calc.Rdata" )









