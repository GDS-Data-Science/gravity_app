################################################################################
################################################################################
##                                                                            ##
##                         Generate prediction data set                       ##
##                                                                            ##
################################################################################
################################################################################

#### load packages 
library( Amelia )
library( caret )
library( CountryRodes )
library( dplyr )
library( haven )
library( purrr )
library( readr )
library( readxl )
library( tibble )
library( tidyr )

#### read in data sets 
country_distances        <- read_dta( "../Data/RawData/dist_cepii.dta" )
country_area_language    <- read_dta( "../Data/RawData/geo_cepii.dta" )
country_pol_index        <- read_excel( "../Data/RawData/FreedomHouse_Country_and_Territory_Ratings_and_Statuses_FIW_1973-2023.xlsx", 
                                        sheet = 2, skip = 1 )
country_gdp_pp           <- read_excel( "../Data/RawData/IMF_NGDPRPPPPC.xls", na = "no data" )
country_population       <- read_excel( "../Data/RawData/IMF_LP.xls", na = "no data" )
country_gdp_ppp          <- read_excel( "../Data/RawData/IMF_PPPSH.xls", na = "no data" )
country_cpi              <- read_excel( "../Data/RawData/IMF_PCPIEPCH.xls", na = "no data"  )
country_geographics      <- read_dta( "../Data/RawData/SPATIAL.dta" )
country_conflict         <- read_excel( "../Data/RawData/UCDP_GEDEvent_v23_1.xlsx" )
load( "../Data/RawData/prio_pred_fatalities.rdata" )
country_conflict_predict <- fatalities_data_formatted
country_conflict_UCDP    <- read_csv( "../Data/RawData/UCDP_GEDEvent_v23_0_11.csv" )
load( "../Data/WorkData/asylum_index.Rdata" )
load( "../Data/WorkData/flows.Rdata" )
country_newarrival <- dat 

#dat_res <- read_csv( "../Data/WorkData/dat_res.csv" )

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
country_area_language$iso <- iso_codes( country_area_language$iso3, cm = 'iso3_alpha3', type = "iso3_alpha3" )
country_area_language$iso[ country_area_language$country == "Namibia" ] <- "NAM"
country_area_language$iso[ country_area_language$country == "Serbia and Montenegro" ] <- "SRB"
country_area_language$iso[ country_area_language$country == "Congo (Democratic Republic of the)" ] <- "COD"
# select variables and filter rows
country_area <- country_area_language %>% 
                select( iso, country, area, landlocked ) %>% 
                distinct() %>% 
                add_row( iso = "SSD", country = "South Sudan", 
                         area = 644329, landlocked = 1 ) %>% 
                add_row( iso = "MNE", country = "Montenegro", 
                         area = 13812, landlocked = 0 )

## country_pol_index
# create new variable names
names <- "country" 
for( i in 1973:( last_year - 1 )){
   tmp <- c( paste0( "PR_", i ), paste0( "CL_", i ), paste0( "Status_", i ))
   names <- c( names, tmp )
}
names( country_pol_index ) <- names 

# remove status and turn from wide to long format
country_pol_index <- country_pol_index %>% 
                     slice( -1 ) %>% 
                     select( -contains( "Status" ))
                     
country_pol_index <- country_pol_index %>% 
                     pivot_longer( cols = 2:ncol(.), 
                                   names_to = c( ".value", "year" ),
                                   names_pattern = "(.*)_(.*)" )

# turn character to numeric
country_pol_index <- within( country_pol_index, {
   year = as.numeric( year )
   PR   = as.numeric( PR )
   CL   = as.numeric( CL )
})

# create and correct iso codes 
country_pol_index$iso <- iso_codes( country_pol_index$country, type = "iso3_alpha3" )
country_pol_index$iso[ country_pol_index$country == "Congo (Kinshasa)" ] <- "COD"
country_pol_index$iso[ country_pol_index$country == "Congo (Brazzaville)" ] <- "COG"
country_pol_index <- filter( country_pol_index, !is.na( country_pol_index$iso ))

# data corrections 
country_pol_index$PR <- ifelse( country_pol_index$iso %in% c( "MNE", "SSD", "SRB" ) & 
                                is.na( country_pol_index$PR ), 0, country_pol_index$PR )
country_pol_index$CL <- ifelse( country_pol_index$iso %in% c( "MNE", "SSD", "SRB" ) & 
                                is.na( country_pol_index$CL ), 0, country_pol_index$CL )

## country_gdp_pp
# solve issues with country names 
country_gdp_pp <- rename( country_gdp_pp, 
                          country = "GDP per capita, current prices (Purchasing power parity; international dollars per capita)" )

country_gdp_pp$iso <- iso_codes( country_gdp_pp$country, type = "iso3_alpha3" )

# manually
country_gdp_pp$iso[ country_gdp_pp$country == "Burundi" ] <- "BDI"
country_gdp_pp$iso[ country_gdp_pp$country == "China, People's Republic of" ] <- "CHN"
country_gdp_pp$iso[ country_gdp_pp$country == "Congo, Dem. Rep. of the" ] <- "COD"
country_gdp_pp$iso[ country_gdp_pp$country == "Congo, Republic of" ] <- "COG"
country_gdp_pp$iso[ country_gdp_pp$country == "Hong Kong SAR" ] <- "HKG"
country_gdp_pp$iso[ country_gdp_pp$country == "Kosovo" ] <- "XKK"
country_gdp_pp$iso[ country_gdp_pp$country == "Lao P.D.R." ] <- "LAO"
country_gdp_pp$iso[ country_gdp_pp$country == "South Sudan, Republic of" ] <- "SSD"
country_gdp_pp$iso[ country_gdp_pp$country == "Türkiye, Republic of" ] <- "TUR"

# select variables and turn format from wide to long
country_gdp_pp <- country_gdp_pp %>% 
                  filter( !is.na( iso )) %>% 
                  select( iso, starts_with( "2" )) %>% 
                  select( -(( ncol(.) - 1 ):ncol(.))) %>% 
                  pivot_longer( cols = starts_with( "2" ), 
                  names_to = "year", 
                  values_to = "GDP_PP" )

# turn character to numeric
country_gdp_pp$year <- as.numeric( country_gdp_pp$year ) 
   

## country_population
# solve issues with country names 
country_population <- rename( country_population, 
                              country = "Population (Millions of people)" )

country_population$iso <- iso_codes( country_population$country, type = "iso3_alpha3" )

# manually
country_population$iso[ country_population$country == "Burundi" ] <- "BDI"
country_population$iso[ country_population$country == "China, People's Republic of" ] <- "CHN"
country_population$iso[ country_population$country == "Congo, Dem. Rep. of the" ] <- "COD"
country_population$iso[ country_population$country == "Congo, Republic of" ] <- "COG"
country_population$iso[ country_population$country == "Hong Kong SAR" ] <- "HKG"
country_population$iso[ country_population$country == "Kosovo" ] <- "XKK"
country_population$iso[ country_population$country == "Lao P.D.R." ] <- "LAO"
country_population$iso[ country_population$country == "South Sudan, Republic of" ] <- "SSD"
country_population$iso[ country_population$country == "Türkiye, Republic of" ] <- "TUR"

# select variables and turn format from wide to long
country_population <- country_population %>% 
                      filter( !is.na( iso )) %>% 
                      select( iso, starts_with( "2" )) %>% 
                      select( -(( ncol(.) - 1 ):ncol(.))) %>% 
                      pivot_longer( cols = starts_with( "2" ), 
                                    names_to = "year", 
                                    values_to = "pop" )

# turn character to numeric
country_population$year <- as.numeric( country_population$year ) 

## country_gpd_ppp
# solve issues with country names 
country_gdp_ppp <- rename( country_gdp_ppp, 
                           country = "GDP based on PPP, share of world (Percent of World)" )

country_gdp_ppp$iso <- iso_codes( country_gdp_ppp$country, type = "iso3_alpha3" )

# manually
country_gdp_ppp$iso[ country_gdp_ppp$country == "Burundi" ] <- "BDI"
country_gdp_ppp$iso[ country_gdp_ppp$country == "China, People's Republic of" ] <- "CHN"
country_gdp_ppp$iso[ country_gdp_ppp$country == "Congo, Dem. Rep. of the" ] <- "COD"
country_gdp_ppp$iso[ country_gdp_ppp$country == "Congo, Republic of" ] <- "COG"
country_gdp_ppp$iso[ country_gdp_ppp$country == "Hong Kong SAR" ] <- "HKG"
country_gdp_ppp$iso[ country_gdp_ppp$country == "Kosovo" ] <- "XKK"
country_gdp_ppp$iso[ country_gdp_ppp$country == "Lao P.D.R." ] <- "LAO"
country_gdp_ppp$iso[ country_gdp_ppp$country == "South Sudan, Republic of" ] <- "SSD"
country_gdp_ppp$iso[ country_gdp_ppp$country == "Türkiye, Republic of" ] <- "TUR"

# select variables and turn format from wide to long
country_gdp_ppp <- country_gdp_ppp %>% 
                   filter( !is.na( iso )) %>% 
                   select( iso, starts_with( "2" )) %>% 
                   select( -(( ncol(.) - 1 ):ncol(.))) %>% 
                   pivot_longer( cols = starts_with( "2" ), 
                                 names_to = "year", 
                                 values_to = "GDP_PPP" )

# turn character to numeric
country_gdp_ppp$year <- as.numeric( country_gdp_ppp$year ) 


## country_cpi
# solve issues with country names 
country_cpi <- rename( country_cpi, 
                       country = "Inflation rate, end of period consumer prices (Annual percent change)" )

country_cpi$iso <- iso_codes( country_cpi$country, type = "iso3_alpha3" )

# manually
country_cpi$iso[ country_cpi$country == "Burundi" ] <- "BDI"
country_cpi$iso[ country_cpi$country == "China, People's Republic of" ] <- "CHN"
country_cpi$iso[ country_cpi$country == "Congo, Dem. Rep. of the" ] <- "COD"
country_cpi$iso[ country_cpi$country == "Congo, Republic of" ] <- "COG"
country_cpi$iso[ country_cpi$country == "Hong Kong SAR" ] <- "HKG"
country_cpi$iso[ country_cpi$country == "Kosovo" ] <- "XKK"
country_cpi$iso[ country_cpi$country == "Lao P.D.R." ] <- "LAO"
country_cpi$iso[ country_cpi$country == "South Sudan, Republic of" ] <- "SSD"
country_cpi$iso[ country_cpi$country == "Türkiye, Republic of" ] <- "TUR"

# select variables and turn format from wide to long
country_cpi <- country_cpi %>% 
               filter( !is.na( iso )) %>% 
               select( iso, starts_with( "2" )) %>% 
               select( -(( ncol(.) - 1 ):ncol(.))) %>% 
               pivot_longer( cols = starts_with( "2" ), 
                             names_to = "year", 
                             values_to = "CPI" )

# turn character to numeric
country_cpi$year <- as.numeric( country_cpi$year ) 

## country_geographics
# select variables and filter rows
country_geographics <- country_geographics %>% 
                       select( Origin, Year, island ) %>%
                       filter( !( Origin %in% c( "Bonaire", "Palestinian", "Saint Pierre et Miquelon", 
                                                 "Tibetan", "Yugoslavia" )))   
# create and correct iso codes 
country_geographics$iso <- iso_codes( country_geographics$Origin, type = "iso3_alpha3" )
country_geographics$iso[ country_geographics$Origin == "Dem. People's Korea Republic of" ] <- "PRK"
country_geographics$iso[ country_geographics$Origin == "Burundi" ] <- "BDI"
country_geographics$iso[ country_geographics$Origin == "Curasao" ] <- "CUW"
country_geographics$iso[ country_geographics$Origin == "Korea Republic of" ] <- "KOR"
country_geographics$iso[ country_geographics$Origin == "Macedonia" ] <- "MKD"
country_geographics$iso[ country_geographics$Origin == "Zaire" ] <- "COD"

# replace missing values 
country_geographics$island[ country_geographics$Origin == "South Sudan" ] <- 0
country_geographics$island[ country_geographics$Origin == "Montenegro" ]  <- 0

## country_conflic
# create iso codes 
country_conflict$iso <- iso_codes( country_conflict$country, type = "iso3_alpha3" )
country_conflict$iso[ country_conflict$country == "Burundi" ]  <- "BDI"

# select variables 
country_conflict <- country_conflict %>% 
                    select( iso, year, type_of_violence, best ) %>% 
                    group_by( iso, year ) %>% 
                    summarise( typeOfViolence = round( mean( type_of_violence ), 0 ), 
                               best_est = sum( best )) %>% 
                    ungroup() 
 
## country_conflict_UCDP
# create iso codes 
country_conflict_UCDP$iso <- iso_codes( country_conflict_UCDP$country, type = "iso3_alpha3" )
country_conflict_UCDP$iso[ country_conflict_UCDP$country == "Burundi" ]    <- "BDI"

# yearly sums 2022
country_conflict_UCDP <- country_conflict_UCDP %>% 
                         select( iso, year, type_of_violence, best ) %>% 
                         group_by( iso, year ) %>% 
                         summarise( typeOfViolence = round( mean( type_of_violence ), 0 ), 
                                    best_est = sum( best )) %>% 
                         ungroup()  

## country_conflict_predict
# create iso codes 
country_conflict_predict$iso <- iso_codes( country_conflict_predict$name, type = "iso3_alpha3" )
country_conflict_predict$iso[ country_conflict_predict$name == "Burundi" ]       <- "BDI"
country_conflict_predict$iso[ country_conflict_predict$name == "Marshall Is." ]  <- "MHL"
country_conflict_predict$iso[ country_conflict_predict$name == "Congo, DRC" ]    <- "COD"
country_conflict_predict$iso[ country_conflict_predict$name == "Timor Leste" ]   <- "TLS"
country_conflict_predict$iso[ country_conflict_predict$name == "Solomon Is." ]   <- "SLB"
country_conflict_predict$iso[ country_conflict_predict$name == "Kosovo" ]        <- "XKX"
country_conflict_predict$iso[ country_conflict_predict$name == "Macedonia" ]     <- "MKD"


# calculate yearly values
country_conflict_predict <- country_conflict_predict %>% 
                            filter( year %in% new_years ) %>% 
                            group_by( iso, year ) %>% 
                            summarise( best_est = round( sum( main_mean ), 0 )) %>%  
                            mutate( typeOfViolence = NA  ) %>% 
                            ungroup() %>% 
                            select( iso, year, typeOfViolence, best_est )


# merge conflict data 
country_conflict <- country_conflict %>% 
                    bind_rows( country_conflict_UCDP ) %>% 
                    bind_rows( country_conflict_predict ) %>% 
                    arrange( iso, year ) 

# create year count 
country_conflict <- country_conflict %>%
                    mutate( idx = if_else( best_est >= 50, 1, 0 )) %>%  
                    group_by( iso, grp = cumsum( idx == 0)) %>%
                    mutate( Nyear_conflict = cumsum( idx )) %>%
                    ungroup() %>%
                    select( -c( grp, idx )) 


#### merge data sets 
# merge two dimensional matrices 
dat <- country_population %>% 
       left_join( country_cpi, by = c( "iso", "year" )) %>%
       full_join( country_gdp_pp, by = c( "iso", "year" )) %>% 
       full_join( country_gdp_ppp, by = c( "iso", "year" )) %>%
       left_join( country_geographics, by = c( "iso", "year" = "Year" )) %>% 
       left_join( country_area, by = c( "iso" )) %>% 
       left_join( country_pol_index, by = c( "iso", "year" )) %>% 
       left_join( country_conflict, by = c( "iso", "year" )) %>% 
       select( -c( Origin, country.x, country.y ))

# remove duplicate rows 
dat <- dat %>% 
       distinct( iso, year, .keep_all = TRUE )

# remove Kosovo 
dat <- dat %>% filter( iso != "XKK" )

#### data cleaning 
# replace missings with zeros 
impute.zero <- function(x) replace( x, is.na(x), 0 )
dat <- dat %>% mutate( best_est =
                        if_else( year <= last_year, impute.zero( best_est ), best_est )) %>% 
               mutate( typeOfViolence =
                        if_else( year <= last_year & best_est < 50, impute.zero( typeOfViolence ), typeOfViolence ),
                       Nyear_conflict =
                        if_else( year <= last_year & best_est < 50, impute.zero( Nyear_conflict ), Nyear_conflict ))

# replace missings with value from last year
impute.2020 <- function(x) replace( x, c( start:end ), x[ c( replace )])
dat <- dat %>% group_by( iso ) %>% 
               mutate( typeOfViolence = impute.2020( typeOfViolence ), 
                       CL = impute.2020( CL ),
                       PR = impute.2020( PR )) %>% 
               ungroup() 

# impute mean function 
impute.mean <- function(x) replace( x, is.na(x), round( mean( x, na.rm = TRUE ), 0 ))
dat <- dat %>% group_by( iso ) %>% 
               mutate( island = impute.mean( island ))

#### imputation with caret KNN
## turn vars into factor
### turn numeric vectors into factors 
num_vars <- c( "year", "island", "landlocked", "typeOfViolence" )
dat <- dat %>% mutate_at( all_of( num_vars ), as.factor )
## use carets knn imputation
impute <- preProcess( as.data.frame( dat ), method = 'knnImpute' )
impu_dat <- predict( impute, dat )

### de-scale and de-center data 
# identify numeric variables 
numeric_vars <- which( sapply( dat, is.numeric ) == TRUE )
# retrieve mean and sd
vector_mean <- impute$mean 
vector_sd   <- impute$std 
# reverse scaling and centering
for( i in 1:length( numeric_vars )){
   impu_dat[ , numeric_vars[i]] <- impu_dat[ , numeric_vars[i]] * vector_sd[i] + vector_mean[i]
}

#### create new variables
impu_dat <- within( impu_dat, {
   # dummy variables
   dead <- as.factor( ifelse( best_est > 25, 1, 0 )) 
   Nyear_conf <- as.factor( ifelse( Nyear_conflict == 0, 0, 1 )) 
   # log transformation
   dead_log <- ifelse( best_est == 0, 0, log( best_est ))
   Nyear_log <- ifelse( Nyear_conflict == 0, 0, log( Nyear_conflict ))
})



# ### impute missing values from other sources
# dat <- as.data.frame( map2_dfc( dat, dat_res, coalesce ))
# 
# #### imputation with Amelia
# # set seed 
# set.seed( 123 )
# 
# # create boundary matrix 
# bounds <- matrix( c( 11, 1, 7, 
#                      12, 1, 7 ), 
#                   ncol = 3, byrow = TRUE )
# 
# # run amelia
# impu <- amelia( dat, idvars = "Country", ts = "year", cs = "ISO", 
#                 sqrts = c( "GDP_PPP", "GDP_PP", "pop", "dead_log", "Nyear_log", 
#                            "area", "best_est", "Nyear_conflict" ),
#                 noms = c(  "dead", "Nyear_conf","typeOfViolence", 
#                            "island", "landlocked" ),
#                 polytime = 1, leads = 2, bounds = bounds, 
#                 parallel = "multicore", p2s = 1 )
# 
# # round categorical variables to nearest value
# impu <- transform( impu, 
#                    CL = round( CL, 0 ), 
#                    PR = round( PR, 0 ))
# 
# ### evaluation 
# # summary
# summary( impu )
# 
# # density plot 
# compare.density( impu, var = "GDP_PPP" )
# compare.density( impu, var = "GDP_PP" )
# compare.density( impu, var = "CL" )
# compare.density( impu, var = "PR" )
# compare.density( impu, var = "CPI" )
# compare.density( impu, var = "Nyear_log" )
# compare.density( impu, var = "dead_log" )


#### proceed merging
# create three dimensional array
est_dat <- expand_grid( iso_o = unique( impu_dat$iso ), 
                        iso_d = unique( impu_dat$iso ), 
                        year = as.factor( 2000:max( new_years ))) %>% 
           filter( iso_o != iso_d ) %>% 
           left_join( impu_dat, by = c( "iso_o" = "iso", "year" )) %>%
           left_join( impu_dat, by = c( "iso_d" = "iso", "year" ), suffix = c( "_o", "_d" )) %>% 
           left_join( country_newarrival, by = c( "iso_o", "iso_d", "year" )) %>% 
           #select( -Id ) %>% 
           mutate( newarrivals = replace_na( newarrivals, 0 ), 
                   Id = paste0( iso_o, iso_d )) %>%
           left_join( country_distances, by = c( "iso_o", "iso_d" )) %>% 
           left_join( dat_index, by = c( "iso_o", "iso_d", "year" ))


### data cleaning and imputation 
## replace missings with mean values
# impute mean function 
impute.mean <- function(x) replace( x, is.na(x), round( mean( x, na.rm = TRUE ), 0 ))
# application of input.mean function 
est_dat <- est_dat %>% group_by( Id ) %>% 
                       mutate( contig = zoo::na.locf( contig, na.rm = FALSE ),
                               comlang_off = zoo::na.locf( comlang_off, na.rm = FALSE ),
                               comlang_ethno = zoo::na.locf( comlang_ethno, na.rm = FALSE ),
                               colony = zoo::na.locf( colony, na.rm = FALSE ),
                               comcol = zoo::na.locf( comcol, na.rm = FALSE ),
                               col45 = zoo::na.locf( col45, na.rm = FALSE ),
                               smctry = zoo::na.locf( smctry, na.rm = FALSE ),
                               dist = zoo::na.locf( dist, na.rm = FALSE ), 
                               index0asylum = zoo::na.locf( index0asylum, na.rm = FALSE )) %>% 
                       ungroup() %>% 
                       mutate( contig = round( impute.mean( contig )),
                               comlang_off = impute.mean( comlang_off ),
                               comlang_ethno = impute.mean( comlang_ethno ),
                               colony = impute.mean( colony ),
                               comcol = impute.mean( comcol ),
                               col45 = impute.mean( col45 ),
                               smctry = impute.mean( smctry ),
                               dist = impute.mean( dist ), 
                               index0asylum = impute.mean( index0asylum ))

# loop through list 
save( est_dat, file = "../Data/WorkData/impuData.Rdata" )


# #stop()
# ################################################################################
# ##                                stock data                                  ##
# ################################################################################
# 
# stock_list <- vector( mode = "list", length = 4 )
# j = 1
# 
# # decision rate country of origin in country of asylum 
# load( "../Data/Workdata/data_deci_2019.Rdata" )
# load( "../Data/Workdata/data_deci_2020.Rdata" )
# load( "../Data/Workdata/data_deci_2021.Rdata" )
# load( "../Data/Workdata/data_deci_2022.Rdata" )
# # decision rate country of asylum 
# data_deci_d_2019 <- read_dta( "../Data/RawData/UNHCR_deci_rate_d.dta" )
# data_deci_d_2020 <- read_dta( "../Data/RawData/UNHCR_deci_rate_d.dta" )
# data_deci_d_2021 <- read_csv( "../Data/RawData/decrate2021.csv" )
# data_deci_d_2022 <- read_csv( "../Data/RawData/decrate2022.csv" )
# 
# data_deci_d_2019$year <- 2019 
# data_deci_d_2020$year <- 2020 
# 
# 
# for( i in c( 2019:last_year )){
#    
# #### read in data 
# data_vda    <- read_csv( paste0( "../Data/WorkData/vdadata_", last_year, ".csv" ))
# data_flow   <- read_csv( paste0( "../Data/WorkData/flowdata_", last_year, ".csv" ))
# data_stock  <- read_csv( paste0( "../Data/WorkData/stockdata_", last_year, ".csv" ))
#    
# #### create stocks 
# data_stock <-    data_stock %>% 
#                  filter( iso_o != iso_d & year == i ) %>% 
#                  select( iso_o, iso_d, year, ref, asy, vda )
# 
# #### create flow 
# data_flow <-     data_flow %>% 
#                  filter( year == i ) %>% 
#                  rename( iso_o = originiso, iso_d = asylumiso )
# 
# #### create percVDA data
# data_vda <- subset( data_vda, year == i )
# data_flow_sum <- data_flow %>% 
#                  filter( iso_d %in% data_vda$iso_d ) %>% 
#                  group_by( iso_d ) %>% 
#                  summarise( total = sum( newarrival, na.rm = TRUE ))
# 
# data_vda <-      data_vda %>% 
#                  left_join( data_flow_sum, by = "iso_d" ) %>% 
#                  replace_na( list( total = 0 )) %>% 
#                  mutate( total = total + vda ) %>% 
#                  mutate( percVDA = vda/total ) %>%
#                  select( -c( year, new_displaced, vda, total )) %>% 
#                  replace_na( list( percVDA = 0 ))
# 
# # if( i == 2021 ){
# # #### correct problems with index0asylum 
# # EU <- c( "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", 
# #          "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", 
# #          "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE" )
# # 
# # data_deci_o$index0asylum[ data_deci_o$iso_o == "VEN" & data_deci_o$iso_d == "COL" ] <- 1 
# # data_deci_o$index0asylum[ data_deci_o$iso_o == "UKR" & data_deci_o$iso_d %in% EU ] <- 1 
# # }
# 
# #### merge data sets 
# # merge 
# dat_stock <- data_stock %>% 
#              full_join( data_flow, by = c( "iso_o", "iso_d", "year" )) %>% 
#              select( -c( index0asylum, newarrival )) %>% 
#              filter( !is.na( iso_o ) & !is.na( iso_d )) %>% 
#              replace( is.na(.), 0 ) %>% 
#              group_by( iso_o, iso_d ) %>% 
#              group_modify( ~ add_row( .x, year = ( i + 1 ):( i + 3 ))) %>% 
#              ungroup() %>% 
#              left_join( data_vda, by = c( "iso_o", "iso_d" ))  %>%
#              mutate( percVDA = replace( percVDA, is.na( percVDA ), 0 )) %>% 
#              left_join( get( paste0( "data_deci_d_", i )), by = c( "iso_d" )) %>% 
#              left_join( get( paste0( "data_deci_o_", i )), by = c( "iso_o", "iso_d" )) %>% 
#              select( -c( year.y, year )) %>% 
#              rename( year = year.x ) %>% 
#              mutate( deci_rate_d = replace_na( deci_rate_d, mean( deci_rate_d, na.rm = TRUE )), 
#                      deci_posi_rate_o = replace_na( deci_posi_rate_o, mean( deci_posi_rate_o, na.rm = TRUE )),
#                      index0asylum = replace_na( index0asylum, 0 )) 
# 
# stock_list[[j]] <- dat_stock
# j <- j + 1 
# 
# }
# 
# save( stock_list, file = "../Data/WorkData/stock_calc.Rdata" )
# 








