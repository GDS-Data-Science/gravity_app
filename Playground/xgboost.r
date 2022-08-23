################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                                  XGBoost                                   ##
##                                                                            ##
################################################################################
################################################################################

#### traincontrol
ctrl_class <- trainControl( method = "adaptive_cv",
                            number = 8,
                            repeats = 2,
                            verboseIter = TRUE,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE, 
                            adaptive = list( min = 2, alpha = 0.05, 
                                             method = "BT", complete = FALSE ),
                            search = "random" )


ctrl_reg   <- trainControl( method = "adaptive_cv",
                            number = 8,
                            repeats = 2,
                            verboseIter = TRUE,
                            adaptive = list( min = 2, alpha = 0.075, 
                                             method = "BT", complete = FALSE ),
                            search = "random" )


## create cluster 
#cl <- makePSOCKcluster( 5 )
#registerDoParallel( cl )

##### classification model
#### with reweighted observations
## model weights
model_weights <- ifelse( dat_train_class$zero == "yes", 1, .125 ) 
## estimation
xgboo_w  <- train( zero ~ ., 
                   data       = dat_train_class, 
                   method     = "xgbTree",
                   trControl  = ctrl_class,
                   weights    = model_weights,
                   metric     = "Sens",
                   preProc    = c( "center", "scale" ),
                   tuneLength = 30 )


## with SMOTE
ctrl$sampling <- "smote"
xgboo_s  <- train( zero ~ ., 
                   data       = dat_train_class, 
                   method     = "xgbTree",
                   trControl  = ctrl_class,
                   metric     = "ROC",
                   preProc    = c( "center", "scale" ),
                   tuneLength = 20 )


##### regression model
xgreg  <- train( newarrival ~ ., 
                 data       = dat_train_reg, 
                 method     = "xgbTree",
                 trControl  = ctrl_reg,
                 metric     = "RMSE",
                 preProc    = c( "center", "scale" ),
                 tuneLength = 20 )

## stop cluster
#stopCluster( cl )


#### predictions  
### classification model 
# prediction
dat_train_class$pred <- predict( xgboo_w )
dat_test_class$pred <- predict( xgboo_w, newdata = dat_test_class )
# confusion matrix 
confusionMatrix( dat_train_class$pred, dat_train_class$zero )
confusionMatrix( dat_test_class$pred, dat_test_class$zero )

### checking false negative observations
# filter false negatives 
idx_train <- dat_train_class %>% 
             filter( pred == "no" & zero == "yes" ) %>% 
             mutate( Id = paste0( iso_o, iso_d, year )) %>% 
             select( Id, year )  
idx_test  <- dat_test_class %>% 
             filter( pred == "no" & zero == "yes" ) %>% 
             mutate( Id = paste0( iso_o, iso_d, year )) %>% 
             select( Id, year )

check_train <- dat %>% 
               mutate( ID = paste0( Id, year )) %>% 
               filter( ID %in% idx_train$Id )

hist( check_train$newarrival, 50, 
      main = "Distribution of 'newarrival' for false negative classifications" )
summary( check_train$newarrival )

check_test <- dat %>% 
   mutate( ID = paste0( Id, year )) %>% 
   filter( ID %in% idx_test$Id )

hist( check_test$newarrival, 50, 
      main = "Distribution of 'newarrival' for false negative classifications" )
summary( check_test$newarrival )


### regression model 
dat_train_reg$pred <- round( predict( xgreg ), 0 )
dat_test_reg$pred <- round( predict( xgreg, newdata = dat_test_reg ), 0 )
rmse( dat_train_reg$newarrival, dat_train_reg$pred )
rmse( dat_test_reg$newarrival, dat_test_reg$pred )












