################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                           Elastic Net Estimations                          ##
##                                                                            ##
################################################################################
################################################################################

## create traincontrol function
my_control <- trainControl( 
                   classProbs = FALSE, 
                   verboseIter = TRUE, 
                   savePredictions = TRUE, 
                   index = time_slice, 
                   parallel = TRUE )

### run elastic net model
el_net <- train( newarrival ~ ., 
                 data = dat, 
                 method = "glmnet",
                 family = "poisson",
                 trControl = my_control,
                 metric = "RMSE",
                 preProc = c( "center", "scale" ),
                 tuneGrid = expand.grid( .alpha = seq( .005, 1, length = 15 ),
                                         .lambda = c(( 1:5 )/10 )))


el_net 






