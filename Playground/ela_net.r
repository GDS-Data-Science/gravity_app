################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                           Elastic Net Estimations                          ##
##                                                                            ##
################################################################################
################################################################################

## tuning grid 
t.grid <- expand.grid( .alpha = 0,                              #seq( 0, 1, by = 0.2 ),
                       .lambda = 0.5 )                          #seq( 0, 1, by = 0.15 ))  



### all data 
el_net_class <- train( zero ~ ., 
                       data      = dat_train_class, 
                       method    = "glmnet",
                       family    = "binomial",
                       trControl = timecontrol_class,
                       metric    = "ROC",
                       preProc   = c( "center", "scale" ),
                       tuneGrid  = t.grid )


trellis.par.set( caretTheme())
plot( el_net_class ) 


### all data 
el_net <- train( newarrival ~ ., 
                 data      = dat_train_reg, 
                 method    = "glmnet",
                 family    = "poisson",
                 trControl = ctrl_reg,
                 metric    = "RMSE",
                 preProc   = c( "center", "scale" ),
                 tuneLength  = 3 )

#### prediction test data 
### classification model 
# prediction
dat_train_class$pred <- predict( el_net_class )
dat_test_class$pred <- predict( el_net_class, newdata = dat_test_class )
# confusion matrix 
confusionMatrix( dat_train_class$pred, dat_train_class$zero )
confusionMatrix( dat_test_class$pred, dat_test_class$zero )

### regression model 
dat_train_reg$pred <- round( predict( el_net ), 0 )
dat_test_reg$pred <- round( predict( el_net, newdata = dat_test_reg ), 0 )
rmse( dat_train_reg$newarrival, dat_train_reg$pred )
rmse( dat_test_reg$newarrival, dat_test_reg$pred )

dat_dis <- dat_train_reg %>% 
           select( newarrival, pred ) %>% 
           pivot_longer( cols = c( "newarrival", "pred" ), 
                         names_to = "origin", 
                         values_to = "val" )

ggplot( dat_dis, aes( x = asinh( val ), fill = origin )) +
   geom_density( alpha = .5 )

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

















