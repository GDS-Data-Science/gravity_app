################################################################################
################################################################################
##                                                                            ##
##                         Calculation Stock Figures                          ##
##                                                                            ##
################################################################################
################################################################################

##### load packages 
library( dplyr )
library( fixest )
library( readr )
library( tidyr )

##### read in data
load( "../Data/WorkData/stock_calc.Rdata" )
load( "../Results/estimations_poisson_ind.Rdata" )
load( "../Data/WorkData/impuData22.Rdata" )
load( "../Data/WorkData/impuData17.Rdata" )


##### prediction of future flows
### remove missing clusters
# ind
impu22 <- lapply( impu22,
                  function( x ) filter( x, !( iso_o %in% c( "ABW", "UVK", "MHL", "PLW", "PRI" )) &
                                           !( iso_d %in% c( "ATG", "BTN", "BRN", "CPV", "GNQ", "FSM", "MMR",
                                                            "NRU", "ERI", "KIR", "UVK", "MAC", "MDV", "MHL",
                                                            "PRI", "WSM", "SMR", "STP", "SYC", "SLE", "SGP", 
                                                            "LCA", "TWN", "TLS", "TON", "TKM", "TUV", "UZB" ))))

true_dat <- impu22[[1]] %>% 
            filter( year == 2021 ) %>% 
            select( iso_o, iso_d, year, newarrival )
                              
sum21 <- impu22[[1]] %>% group_by( iso_d, year ) %>% summarise( tot21 = sum( newarrival ))

### add cluster 2019
for( i in 1:5 ){
   impu22[[i]]$year <- 2019  
}

flow_predictions <- mapply( function( x, y ) 
                            predict( x, newdata = y, type = "response" ), 
                            x = est_models_poisson, y = impu22 )

# round( rowMeans( flow_predictions ), 0 )
pre_newarrival <- data.frame( iso_o = impu22[[1]]$iso_o, 
                              iso_d = impu22[[1]]$iso_d, 
                              year = rep( 2021:2024, times = nrow( flow_predictions)/4 ),
                              var = round( rowMeans( flow_predictions ), 0 ))

##### checks
check1 <- pre_newarrival %>% group_by( year ) %>% summarise( tot = sum( var, na.rm = TRUE ))
check2 <- pre_newarrival %>% group_by( iso_d, year ) %>% 
                             summarise( tot = sum( var, na.rm = TRUE )) %>% 
                             left_join( sum21, by = c( "iso_d", "year" ))
check4 <- pre_newarrival %>% filter( year == 2021 ) %>% 
                             left_join( true_dat, by = c( "iso_o", "iso_d", "year" )) %>% 
                             summarise( mse = 1/nrow( pre_newarrival ) * sum(( var - newarrival )^2 ))


##### calculation of stock figures 
dat_stock <- dat_stock %>% 
             left_join( pre_newarrival, by = c( "iso_o", "iso_d", "year" )) %>% 
             filter( year %in% c( 2021:2024 )) %>% 
             rename( predarrival = var ) %>% 
             group_by( iso_o, iso_d ) %>% 
             mutate( deci_rate_d = replace( deci_rate_d, is.na( deci_rate_d ), 0 ),
                     deci_posi_rate_o = replace( deci_posi_rate_o, is.na( deci_posi_rate_o ), deci_rate_d ), 
                     index0asylum = replace( index0asylum, is.na( index0asylum ), 0 ), 
                     predarrival = replace_na( predarrival, 0 )) %>%
             nest()

# function to calculate new refugee, asylum seeker and vda stocks
stock_calc <- function( df ){        
   within( df, {
      for( i in 2:4 ){
         # refugees 
         ref[i] <- round( ref[i-1] + predarrival[i] * index0asylum[i] +
                             ( predarrival[i] * ( 1 - index0asylum[i]) * ( 1 - percVDA[i-1]) +
                                  asy[i-1]) * deci_rate_d[i] * deci_posi_rate_o[i], 0 )
         # asylum seekers 
         asy[i] <- round( ( asy[i-1] + predarrival[i] * ( 1 - index0asylum[i]) * ( 1 - percVDA[i-1] )) *
                             ( 1 - deci_rate_d[i]), 0 )
         # venezuelans
         vda[i] <- round( vda[i-1] + predarrival[i] * percVDA[i-1], 0 )
      }
   })
}

# calculate stocks 
dat_stock$data <- lapply( dat_stock$data, stock_calc ) 
# unlist  
dat_stock <- unnest( dat_stock, cols = c( data ))
# select variables and rename 
pred_stock <- dat_stock %>% 
              select( iso_o, iso_d, year, ref, asy, vda ) %>% 
              filter( iso_o != "UKN" ) %>%
              mutate( vda = replace( vda, iso_o != "VEN", 0 )) %>% 
              rename( country_origin = iso_o, 
                      country_asylum = iso_d, 
                      refugee_stocks = ref, 
                      asylum_stocks = asy, 
                      venezuelans_stocks = vda ) %>% 
              as.data.frame()

save( pred_stock, file = "../results/predictedStocks_Poisson_ind.Rdata" )
save( pre_newarrival, file = "../results/predictedFlows_Poisson_ind.Rdata" )

##### checks 
check3 <- pred_stock %>% group_by( year ) %>% 
                         summarise( tot_ref = sum( refugee_stocks ), tot_asy = sum( asylum_stocks ))


