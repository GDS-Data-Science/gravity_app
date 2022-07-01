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

### classification model
xgboo  <- train( zero ~ ., 
                 data       = dat_train_class, 
                 method     = "xgbTree",
                 trControl  = timecontrol_class,
                 metric     = "ROC",
                 preProc    = c("center", "scale"),
                 tuneGrid   = xgbGrid )

### regression model
xgreg  <- train( newarrival ~ ., 
                 data       = dat_train_reg, 
                 method     = "xgbTree",
                 trControl  = timecontrol_reg,
                 metric     = "RMSE",
                 preProc    = c("center", "scale"),
                 tuneGrid   = xgbGrid )

## stop cluster
stopCluster( cl )

xgboo

xgreg 


















