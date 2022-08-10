################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                               Random Forest                                ##
##                                                                            ##
################################################################################
################################################################################

## tuning grid
# classification model
t.grid_class <- expand.grid(
                  mtry          = c( 2, 4, 6, 8, 10 ),
                  splitrule     = c( "gini", "extratrees" ),
                  min.node.size = c( 10, 20 ))

# classification model
t.grid_reg <- expand.grid(
                  mtry          = c( 2, 4, 6, 8, 10 ),
                  splitrule     = c( "variance", "extratrees" ),
                  min.node.size = c( 10, 20 ))


## create cluster 
#cl <- makePSOCKcluster( 5 )
#registerDoParallel( cl )

### classification model
ran_foclass <- train( zero ~ ., 
                      data       = dat_train_class, 
                      method     = "ranger",
                      trControl  = timecontrol_class,
                      metric     = "AUC",
                      tuneGrid   = t.grid_class, 
                      num.trees  = 300,
                      importance = "permutation" )


### regression model
ran_foreg <- train( newarrival ~ ., 
                    data       = dat_train_reg, 
                    method     = "ranger",
                    trControl  = timecontrol_reg,
                    metric     = "RMSE",
                    tuneGrid   = t.grid_reg, 
                    num.trees  = 300,
                    importance = "permutation" )

## stop cluster
#stopCluster( cl )

#### prediction test data 
### classification model 
# prediction
pred_ranclass_train <- predict( ran_foclass )
pred_ranclass_test <- predict( ran_foclass, newdata = dat_test_class )
# confusion matrix 
confusionMatrix( pred_ranclass_train, dat_train_class$zero )
confusionMatrix( pred_ranclass_test, dat_test_class$zero )

### regression model 
pred_ranreg_train <- round( predict( ran_foreg ), 0 )
pred_ranreg_test <- round( predict( ran_foreg, newdata = dat_test_reg ), 0 )
rmse( dat_train_reg$newarrival, pred_ranreg_train )
rmse( dat_test_reg$newarrival, pred_ranreg_test )



# ### by country of origin 
# ## nesting data 
# dat_nest <- dat_iso_o %>% group_by( iso_o ) %>% 
#    nest()
# 
# ## create cluster 
# cl <- makePSOCKcluster( 5 )
# registerDoParallel( cl )
# 
# ## train models
# mod_res <- dat_nest %>% 
#    mutate( iso_o_model = map( data,
#                               ~ train( newarrival ~ ., 
#                                        data       = .x, 
#                                        method     = "ranger",
#                                        trControl  = trainControl(
#                                           method            = "timeslice",
#                                           initialWindow     = window.length * length( unique( .x$Id )),
#                                           horizon           = length( unique( .x$Id )),
#                                           skip              = length( unique( .x$Id )),
#                                           selectionFunction = "best",
#                                           fixedWindow       = TRUE,
#                                           savePredictions   = "final",
#                                           allowParallel     = TRUE ),
#                                        metric     = "RMSE",
#                                        tuneGrid   = t.grid,
#                                        num.trees  = 300,
#                                        importance = "permutation" )))
# 
# ## stop cluster
# stopCluster( cl )
# 
















