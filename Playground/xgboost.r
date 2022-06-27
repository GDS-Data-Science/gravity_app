################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                                   XGBoost                                  ##
##                                                                            ##
################################################################################
################################################################################

## tuning grid 
xgbGrid <- expand.grid(
               nrounds          = c(1, 10),
               max_depth        = c(1, 4),
               eta              = c(.1, .4),
               gamma            = 0,
               colsample_bytree = .7,
               min_child_weight = 1,
               subsample        = c( .8, 1 ))


## create cluster 
cl <- makePSOCKcluster( 5 )
registerDoParallel( cl )


### all data 
xgboo  <- train( newarrival ~ ., 
                 data       = dat_train, 
                 method     = "xgbTree",
                 trControl  = timecontrol,
                 metric     = "RMSE",
                 preProc    = c("center", "scale"),
                 tuneGrid   = xgbGrid )

## stop cluster
stopCluster( cl )

xgboo 


















