################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                               Random Forest                                ##
##                                                                            ##
################################################################################
################################################################################

## tuning grid 
t.grid <- expand.grid(
            mtry          = 2:4,
            splitrule     = "gini",
            min.node.size = c( 10, 20 ))


## create cluster 
cl <- makePSOCKcluster( 5 )
registerDoParallel( cl )


### all data 
ran_fo <- train( newarrival ~ ., 
                 data       = dat, 
                 method     = "ranger",
                 trControl  = timecontrol,
                 metric     = "RMSE",
                 tuneGrid   = t.grid, 
                 num.trees  = 300,
                 importance = "permutation" )

## stop cluster
stopCluster( cl )

ran_fo 


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
                                       data       = .x, 
                                       method     = "ranger",
                                       trControl  = trainControl(
                                          method            = "timeslice",
                                          initialWindow     = window.length * length( unique( .x$Id )),
                                          horizon           = length( unique( .x$Id )),
                                          skip              = length( unique( .x$Id )),
                                          selectionFunction = "best",
                                          fixedWindow       = TRUE,
                                          savePredictions   = "final",
                                          allowParallel     = TRUE ),
                                       metric     = "RMSE",
                                       tuneGrid   = t.grid,
                                       num.trees  = 300,
                                       importance = "permutation" )))

## stop cluster
stopCluster( cl )

















