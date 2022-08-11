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
               nrounds          = c( 1 ),           # 10 ),
               max_depth        = c( 6 ),           # 1, 4 ),
               eta              = c( .3 ),          # .1, .4 ),
               gamma            = 0,
               colsample_bytree = 1,                #.7,
               min_child_weight = 1,
               subsample        = c( 1 ))           # .8,


model_weights <- ifelse( dat_train_class$zero == "yes", 1, .125 ) 

ctrl <- trainControl( method = "repeatedcv",
                      number = 4,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE )


## create cluster 
#cl <- makePSOCKcluster( 5 )
#registerDoParallel( cl )

xgboo  <- train( zero ~ ., 
                 data       = dat_train_class, 
                 method     = "xgbTree",
                 trControl  = ctrl,
                 metric     = "ROC",
                 preProc    = c("center", "scale"),
                 tuneLength = 1 )


### classification model
xgboo1  <- train( zero ~ ., 
                 data       = dat_train_class, 
                 method     = "xgbTree",
                 trControl  = ctrl,
                 weights    = model_weights,
                 metric     = "ROC",
                 preProc    = c("center", "scale"),
                 tuneLength = 1 )


 ctrl$sampling <- "down"

xgboo2  <- train( zero ~ ., 
                 data       = dat_train_class, 
                 method     = "xgbTree",
                 trControl  = ctrl,
                 metric     = "ROC",
                 preProc    = c("center", "scale"),
                 tuneLength = 1 )

ctrl$sampling <- "up"

xgboo3  <- train( zero ~ ., 
                  data       = dat_train_class, 
                  method     = "xgbTree",
                  trControl  = ctrl,
                  metric     = "ROC",
                  preProc    = c("center", "scale"),
                  tuneLength = 1 )


ctrl$sampling <- "smote"

xgboo4  <- train( zero ~ ., 
                  data       = dat_train_class, 
                  method     = "xgbTree",
                  trControl  = ctrl,
                  metric     = "ROC",
                  preProc    = c("center", "scale"),
                  tuneLength = 1 )




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
pred_boo_train <- predict( xgboo4 )
pred_boo_test <- predict( xgboo4, newdata = dat_test_class )
# confusion matrix 
confusionMatrix( pred_boo_train, dat_train_class$zero )
confusionMatrix( pred_boo_test, dat_test_class$zero )

### regression model 
pred_reg_train <- round( predict( xgreg ), 0 )
pred_reg_test <- round( predict( xgreg, newdata = dat_test_reg ), 0 )
rmse( dat_train_reg$newarrival, pred_reg_train )
rmse( dat_test_reg$newarrival, pred_reg_test )












