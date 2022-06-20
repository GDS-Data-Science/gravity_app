################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                           Elastic Net Estimations                          ##
##                                                                            ##
################################################################################
################################################################################


### load packages
library( caret )
library( dplyr )
library( glmnet)
library( purrr )
library( readr )
library( tidyr )

### run elastic net model
el_net <- train( newarrival ~ ., 
                 data = dat, 
                 method = "glmnet",
                 family = "poisson",
                 trControl = time_slice,
                 metric = "RMSE",
                 preProc = c( "center", "scale" ),
                 tuneGrid = expand.grid( .alpha = seq( .005, 1, length = 15 ),
                                         .lambda = c(( 1:5 )/10 )))
el_net 







