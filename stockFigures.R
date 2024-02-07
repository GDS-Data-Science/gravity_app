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
library( ggplot2 )
library( hrbrthemes )
library( Metrics )
library( readr )
library( tidyr )
library( viridis )

##### read in data
# load stock data from script 'predictionData.r'
load( "../Data/WorkData/stock_list_2023.Rdata" )
# load estimation results from script 'grav_est.R'
load( "../Results/estimations_2023.Rdata" )
# load testing and training data from script 'grav_est.R'
load( "../Data/Workdata/seq_dat.Rdata" )


##### extract train and test data and rearrange nested list 
years <- c( last_year-3, last_year-2, last_year-1, last_year )
test_dat <- train_dat <- list( assign( paste0( "dat", years[1]), vector( mode = "list", length = 5 )), 
                               assign( paste0( "dat", years[2]), vector( mode = "list", length = 5 )),
                               assign( paste0( "dat", years[3]), vector( mode = "list", length = 5 )),
                               assign( paste0( "dat", years[4]), vector( mode = "list", length = 5 )))

# extract train data 
for( i in 1:4 ){
   for( j in 1:5 ){
      train_dat[[i]][[j]] <- seq_dat[[j]]$train[[i]]
   }
}

# extract test data
for( i in 1:4 ){
   for( j in 1:5 ){
      test_dat[[i]][[j]] <- seq_dat[[j]]$test[[i]]
   }
}

###### calculate future stocks
t. <- c(( last_year - 4 ):( last_year - 1 )) 

pred_stock <- vector( mode = "list", length = 4 )
pred_flow  <- vector( mode = "list", length = 4 )

for( i in 1:4 ){
##### prediction of future flows
### add cluster of respective year
for( j in 1:5 ){
   test_dat[[i]][[j]]$index0asylum[ 
         test_dat[[i]][[j]]$iso_o == "SDN" &
         test_dat[[i]][[j]]$iso_d %in% c( "TCD", "SSD", "EGY" ) &
         test_dat[[i]][[j]]$year == 2024 ] <- 1 
   test_dat[[i]][[j]]$year <- t.[i]  
}

#### flow prediction
flow_predictions <- matrix( NA, nrow = nrow( test_dat[[1]][[1]] ), ncol = 5 ) 
for( j in 1:5 ){
         ddd <<- train_dat[[i]][[j]]
         x <- est_models_poisson[[i]][[j]]
         x$call$data <- str2lang( "ddd" )
         flow_predictions[ , j ] <- predict( x, newdata = test_dat[[i]][[j]], type = "response" )
}
rm( ddd, envir = globalenv())

### round predictions to nearest integer
pred_newarrival <- data.frame( iso_o = test_dat[[i]][[1]]$iso_o, 
                               iso_d = test_dat[[i]][[1]]$iso_d, 
                               year  = rep(( t.[i] + 2 ):( t.[i] + 3 ), 
                                             times = nrow( flow_predictions )/2 ),
                               var   = round( rowMeans( flow_predictions ), 0 ))

pred_flow[[i]] <- pred_newarrival %>% 
                  mutate( var = replace_na( var, 0 ))

##### calculation of stock figures 
dat_stock <- stock_list_2023[[i]] %>% 
             filter( year != 2022 ) %>% 
             left_join( pred_newarrival, by = c( "iso_o", "iso_d", "year" )) %>% 
             filter( year %in% c( t.[[i]]+1:( t.[[i]] + 3 ))) %>% 
             rename( predarrival = var ) %>% 
             group_by( iso_o, iso_d ) %>% 
             mutate( deci_rate_d      = replace( deci_rate_d, is.na( deci_rate_d ), 0 ),
                     deci_posi_rate_o = replace( deci_posi_rate_o, is.na( deci_posi_rate_o ), deci_rate_d ), 
                     index0asylum     = replace( index0asylum, is.na( index0asylum ), 0 ), 
                     predarrival      = replace_na( predarrival, 0 )) %>%
             nest()

# function to calculate new refugee, asylum seeker and vda stocks
stock_calc <- function( df ){        
   within( df, {
      for( j in 2:3 ){
         # refugees 
         ref[j] <- round( ref[j-1] + 
                          predarrival[j] * index0asylum[j] +
                        ( predarrival[j] * ( 1 - index0asylum[j]) * ( 1 - percVDA[j-1]) +
                          asy[j-1] ) *  deci_posi_rate_o[j] * deci_rate_d[j]
                         , 0 )
         # asylum seekers 
         asy[j] <- round(( asy[j-1] + 
                           predarrival[j] * ( 1 - index0asylum[j]) * 
                              ( 1 - percVDA[j-1] )) * ( 1 - deci_rate_d[j])
                         , 0 )
         # venezuelans
         vda[j] <- round( vda[j-1] + predarrival[j] * percVDA[j-1], 0 )
      }
   })
}

# calculate stocks 
dat_stock$data <- lapply( dat_stock$data, stock_calc ) 
# unlist  
dat_stock <- unnest( dat_stock, cols = c( data ))
# select variables and rename 
pred_stock[[i]] <- dat_stock %>% 
                   select( iso_o, iso_d, year, ref, asy, vda ) %>% 
                   filter( iso_o != "UKN" ) %>%
                   mutate( vda = replace( vda, iso_o != "VEN", 0 )) %>% 
                   rename( country_origin = iso_o, 
                           country_asylum = iso_d, 
                           refugee_stocks = ref, 
                           asylum_stocks = asy, 
                           venezuelans_stocks = vda ) %>% 
                   distinct( country_origin, country_asylum, year, .keep_all = TRUE ) %>% 
                   as.data.frame()

}

### write out predicted stocks 
save( pred_stock, file = "../results/predictedStocks_Poisson_ind_2023.Rdata" )
### write out predicted flows
save( pred_flow, file = "../results/predictedFlows_Poisson_ind_2023.Rdata" )

sub_flows_MENA <-  pred_flow[[4]] %>% 
                   filter( iso_o %in% c( "TUN", "MAR", "LBY", "DZA", "EGY", "MRT" ) | 
                           iso_d %in% c( "TUN", "MAR", "LBY", "DZA", "EGY", "MRT" )) %>% 
                   filter( !is.na( var ))

write_csv( sub_flows_MENA, file = "../Results/flows_MENA.csv" )

sub_stocks_MENA <-  pred_stock[[4]] %>% 
   filter( country_asylum %in% c( "TUN", "MAR", "LBY", "DZA", "EGY", "MRT" ) | 
           country_origin %in% c( "TUN", "MAR", "LBY", "DZA", "EGY", "MRT" )) %>% 
   filter( !is.na( refugee_stocks ))

write_csv( sub_stocks_MENA, file = "../Results/stocks_MENA.csv" )


################################################################################
##### checks 
## rmse
rmse( test_dat[[1]][[1]]$newarrival, pred_flow[[1]]$var )
## rrse
rrse( test_dat[[1]][[1]]$newarrival, pred_flow[[1]]$var )
## mae
mae( test_dat[[1]][[1]]$newarrival, pred_flow[[1]]$var )
## smape
smape( test_dat[[1]][[1]]$newarrival, pred_flow[[1]]$var )
## rae
rae( test_dat[[1]][[1]]$newarrival, pred_flow[[1]]$var )
## msle
msle( test_dat[[1]][[1]]$newarrival, pred_flow[[1]]$var )
## bias
bias( test_dat[[1]][[1]]$newarrival, pred_flow[[1]]$var )

# ##### plots 
# ## kernel density plot
# pred <- pred_flow[[1]] 
# pred$orig <- test_dat$dat_2019[[1]]$newarrival 
# 
# p1 <- pred  %>% 
#       pivot_longer( cols = c( "var", "orig"), names_to = "source", values_to = "value" ) %>%
#       ggplot( aes( x = log10( value ), group = source, fill = source )) +
#       geom_density( adjust=1.5, alpha=.4 ) +
#       ggtitle( "Kernel Density Plot True vs. Predicted") +
#       theme_ipsum()
#       
# ## selected countries 
# p2 <- pred %>% 
#       filter( iso_o == "SYR" & iso_d == "TUR" ) %>% 
#       mutate( pair_id = paste0( iso_o, " -> ", iso_d )) %>% 
#       pivot_longer( cols = c( "var", "orig"), names_to = "source", values_to = "value" ) %>% 
#       ggplot( aes( x = year, y = value, color = source )) +
#       geom_line( size = 1.5 ) + 
#       scale_x_continuous( breaks = c( 2019, 2020, 2021 )) +
#       ggtitle( "Predicted vs. original: Syrians in Turkey" ) +
#       theme_ipsum()
# 
# p3 <- pred %>% 
#       filter( iso_o == "VEN" & iso_d == "COL" ) %>% 
#       mutate( pair_id = paste0( iso_o, " -> ", iso_d )) %>% 
#       pivot_longer( cols = c( "var", "orig"), names_to = "source", values_to = "value" ) %>% 
#       ggplot( aes( x = year, y = value, color = source )) +
#       geom_line( size = 1.5 ) + 
#       scale_x_continuous( breaks = c( 2019, 2020, 2021 )) +
#       ggtitle( "Predicted vs. original: Venezuelans in Columbia" ) +
#       theme_ipsum()
# 
# p4 <- pred %>% 
#       filter( iso_o == "ETH" & iso_d == "SUD" ) %>% 
#       mutate( pair_id = paste0( iso_o, " -> ", iso_d )) %>% 
#       pivot_longer( cols = c( "var", "orig"), names_to = "source", values_to = "value" ) %>% 
#       ggplot( aes( x = year, y = value, color = source )) +
#       geom_line( size = 1.5 ) + 
#       scale_x_continuous( breaks = c( 2019, 2020, 2021 )) +
#       ggtitle( "Predicted vs. original: Venezuelans in Columbia" ) +
#       theme_ipsum()
# 
