################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                           Elastic Net Estimations                          ##
##                                                                            ##
################################################################################
################################################################################

## tuning grid 
t.grid <- expand.grid( .alpha = seq( .005, 1, length = 15 ),
                       .lambda = c(( 1:5 )/10 ))


## create cluster 
cl <- makePSOCKcluster( 5 )
registerDoParallel( cl )


### all data 
el_net <- train( newarrival ~ ., 
                 data      = dat, 
                 method    = "glmnet",
                 family    = "poisson",
                 trControl = timecontrol,
                 metric    = "RMSE",
                 preProc   = c( "center", "scale" ),
                 tuneGrid  = t.grid )

## stop cluster
stopCluster( cl )

el_net 


### by country of origin 
## nesting data 
dat_nest <- dat_iso_o %>% group_by( iso_o ) %>% 
                          nest()

## create cluster 
cl <- makePSOCKcluster( 5 )
registerDoParallel( cl )

## train models
mod_res <- dat_nest %>% 
           mutate( iso_o_model = map( data,
                   ~ train( newarrival ~ ., 
                        data      = .x, 
                        method    = "glmnet",
                        family    = "poisson",
                        trControl = trainControl(
                           method            = "timeslice",
                           initialWindow     = window.length * length( unique( .x$Id )),
                           horizon           = length( unique( .x$Id )),
                           skip              = length( unique( .x$Id )),
                           verboseIter       = TRUE,
                           selectionFunction = "best",
                           fixedWindow       = TRUE,
                           savePredictions   = "final",
                           allowParallel     = TRUE ),
                        metric    = "RMSE",
                        preProc   = c( "center", "scale" ),
                        tuneGrid  = t.grid )))

## stop cluster
stopCluster( cl )

















