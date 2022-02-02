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
country_newarrival <- read_csv( "../Data/RawData/gravity.csv" )

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
                     filter( !( iso_o %in% c( "YUG", "ZAR" )) & !( iso_d %in% c( "YUG", "ZAR" )))
   

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
impute.2020 <- function(x) replace( x, c( 20:25 ), x[ c( 19 )])
dat <- dat %>% group_by( ISO ) %>% 
               mutate( typeOfViolence = impute.2020( typeOfViolence ), 
                       best_est = impute.2020( best_est ), 
                       Nyear_conflict = impute.2020( Nyear_conflict ),
                       CL = impute.2020( CL ),
                       PR = impute.2020( PR ))

#### create new variables
dat <- within( dat, {
   # dummy variables
   dead <- ifelse( best_est > 100, 0, 1 ) 
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
dat_o <- dat %>% filter( ISO %in% country_newarrival$iso_o ) %>%
                 rename( iso_o = ISO, pop_o = pop, CPI_o = CPI, Country_o = Country, 
                         GDP_PP_o = GDP_PP, GDP_PPP_o = GDP_PPP, island_o = island, 
                         area_o = area, landlocked_o = landlocked, PR_o = PR, CL_o = CL, 
                         type_of_violence_o = typeOfViolence, best_o = best_est, 
                         Nyear_conflict_o = Nyear_conflict, Nyear_log_o = Nyear_log, 
                         dead_log_o = dead_log, Nyear_conf_o = Nyear_conf, dead_o = dead )

dat_d <- dat %>% filter( ISO %in% country_newarrival$iso_d ) %>%
                 rename( iso_d = ISO, pop_d = pop, CPI_d = CPI, Country_d = Country, 
                         GDP_PP_d = GDP_PP, GDP_PPP_d = GDP_PPP, island_d = island, 
                         area_d = area, landlocked_d = landlocked, PR_d = PR, CL_d = CL, 
                         type_of_violence_d = typeOfViolence, best_d = best_est, 
                         Nyear_conflict_d = Nyear_conflict, Nyear_log_d = Nyear_log, 
                         dead_log_d = dead_log, Nyear_conf_d = Nyear_conf, dead_d = dead )

# join three dimensional arrays 
est_dat <- dat_o %>% 
           full_join( dat_d, by = "year" ) %>% 
           mutate( ID = paste0( iso_o, iso_d )) %>% 
           filter( ID %in% country_newarrival$Id ) %>% 
           left_join( country_newarrival, by = c( "iso_o", "iso_d", "year" )) %>% 
           select( iso_o, iso_d, year, ID, newarrival, index0asylum, pop_o, 
                   pop_d, CPI_o, CPI_d, GDP_PP_o, 
                   GDP_PP_d, GDP_PPP_o, GDP_PPP_d, island_o, island_d, area_o, 
                   area_d, landlocked_o, landlocked_d, PR_o, PR_d, CL_o, CL_d, 
                   type_of_violence_o, type_of_violence_d, best_o, best_d, 
                   Nyear_conflict_o, Nyear_conflict_d, Nyear_log_o, Nyear_log_d, 
                   dead_log_o, dead_log_d, Nyear_conf_o, Nyear_conf_d, dead_o, dead_d ) %>% 
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
est_dat <- est_dat %>% mutate( contig = impute.mean( contig ),
                               comlang_off = impute.mean( comlang_off ),
                               comlang_ethno = impute.mean( comlang_ethno ),
                               colony = impute.mean( colony ),
                               comcol = impute.mean( comcol ),
                               col45 = impute.mean( col45 ),
                               smctry = impute.mean( smctry ),
                               dist = impute.mean( dist ), 
                               index0asylum = impute.mean( index0asylum ))
}

# loop through list 
impu_clean <- lapply( impu_dat, cleanMean )

#### generate training and predictive data sets 
impu15 <- lapply( impu_clean, function(x) subset( x, year > 2014 & year < 2021 ))
impu21 <- lapply( impu_clean, function(x) subset( x, year > 2020 ))

#### save data sets
save( impu15, file = "../Data/WorkData/impuData15.Rdata" )
save( impu21, file = "../Data/WorkData/impuData21.Rdata" )


################################################################################
##                                stock data                                  ##
################################################################################

#### read in data 
data_deci_o <- read_dta( "../Data/Rawdata/UNHCR_deci_posi_rate_o.dta" )
data_deci_d <- read_dta( "../Data/Rawdata/UNHCR_deci_rate_d.dta" )
data_vda <- read_dta( "../Data/Rawdata/VDA_2017_2020.dta" )
data_flow <- read_dta( "../Data/Rawdata/UNHCR_Flow.dta" )
data_stock <- read_dta( "../Data/Rawdata/UNHCR_Stock.dta" )

#### merge data sets 
# merge 
dat_stock <- data_stock %>% 
             full_join( data_flow, by = c( "iso_o", "iso_d", "year" )) %>% 
             left_join( data_vda, by = c( "iso_o", "iso_d", "year" ))  %>% 
             select( -c( Id.x, Id.y, index0asylum )) %>% 
             replace( is.na(.), 0 ) %>% 
             filter( year == 2020 ) %>% 
             group_by( iso_o, iso_d ) %>% 
             group_modify( ~ add_row( .x, year = 2021:2025 )) %>% 
             ungroup() %>% 
             left_join( data_deci_d, by = c( "iso_d" )) %>% 
             left_join( data_deci_o, by = c( "iso_o", "iso_d" )) %>% 
             filter( iso_o != iso_d )
             
save( dat_stock, file = "../Data/WorkData/stock_calc.Rdata" )









