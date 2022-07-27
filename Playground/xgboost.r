################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                                  XGBoost                                   ##
##                                                                            ##
################################################################################
################################################################################

## tuning grid 
xgbGrid <- expand.grid(
               nrounds          = c( 1, 10 ),
               max_depth        = c( 1, 4 ),
               eta              = c(.1, .4 ),
               gamma            = 0,
               colsample_bytree = .7,
               min_child_weight = 1,
               subsample        = c( .8, 1 ))


## create cluster 
#cl <- makePSOCKcluster( 5 )
#registerDoParallel( cl )

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
#stopCluster( cl )


#### prediction test data 
### classification model 
# prediction
pred_boo_train <- predict( xgboo )
pred_boo_test <- predict( xgboo, newdata = dat_test_class )
# confusion matrix 
confusionMatrix( pred_boo_train, dat_train_class$zero )
confusionMatrix( pred_boo_test, dat_test_class$zero )

### regression model 
pred_reg_train <- round( predict( xgreg ), 0 )
pred_reg_test <- round( predict( xgreg, newdata = dat_test_reg ), 0 )
rmse( dat_train_reg$newarrival, pred_reg_train )
rmse( dat_test_reg$newarrival, pred_reg_test )












