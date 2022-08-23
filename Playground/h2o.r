################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                                    h2o                                     ##
##                                                                            ##
################################################################################
################################################################################

#### initiate h2o cluster
h2o.init( nthreads = -1, max_mem_size = "6g" )
h2o.clusterInfo()

#### turn data into a h2o object
dat.h2o <- as.h2o( dat )
str( dat.h2o )

#### generate training and testing data
dat.split = h2o.splitFrame( data = dat.h2o, ratios = .8 )
dat.train = dat.split[[1]]
dat.test  = dat.split[[2]]

#### create dependent and explanatory variables
Y <- "newarrival"
X <- setdiff( names( dat.train ), "newarrival" )


#### define hyper parameters 
hyper_params <- list( tweedie_power = c( 1.1, 1.2, 1.5, 1.7, 1.9 ), 
                      ntrees = c( 200, 500, 1000 ), 
                      learn_rate = c( 0.001, 0.01, 0.1 ), 
                      max_depth = c( 3, 5, 9 ),
                      sample_rate = c( 0.8, 1.0 ),
                      col_sample_rate = c( 0.2, 0.5, 1.0 ))


#### conduct grid search 
tic()
grid <- h2o.grid( x = X, 
                  y = Y, 
                  training_frame = dat.train,
                  validation_frame = dat.test, 
                  algorithm = "gbm",
                  grid_id = "flee_grid",
                  distribution = "tweedie",
                  hyper_params = hyper_params,
                  search_criteria = list( strategy = "RandomDiscrete", 
                                          max_models = 2, seed = 1 ),
                  parallelism = 7,
                  seed = 42 )
toc()

## evaluate results 
sorted_grid <- h2o.getGrid( "flee_grid", sort_by = "rmse", decreasing = FALSE )

#### choose best model 
best_gbm <- h2o.getModel( grid@model_ids[[1]])


#### predict test data 
pred <- round( h2o.predict( best_gbm, newdata = dat.test ), 0 ) 
pred$predict <- ifelse( pred$predict < 0, 0, pred$predict )
h2o.hist( pred, 50 )
summary( pred )
summary( dat.test$newarrival )

#### plot rootogram
predicted <- table( as.data.frame( pred )$predict )
predicted <- predicted[ 1:600 ]
origin <- table( as.data.frame( dat.test )$newarrival )
origin <- origin[ 1:600 ]
rootogram( origin, predicted )

#### create kernel density plots and confusion matrix 
# generate data frame 
check_dat <- data.frame( original = as.data.frame( dat.test )$newarrival,
                         predicted = as.data.frame( pred )$predict )

# turn it into long formatand plot density plot  
check_dat  %>%
   pivot_longer( cols      = c( "original", "predicted" ), 
                 names_to  = "origin", 
                 values_to = "val" ) %>%
   ggplot( aes( x = log10( val ), fill = origin )) +
   geom_density( alpha = .3 )

# create data frame for confusion matrix 
check_dat2 <- check_dat %>% 
              mutate( orig_class = ifelse( .$original > 0, 1, 0 ), 
                      pred_class = ifelse( .$predicted > 0, 1, 0 ))
# confusion matrix 
confusionMatrix( as.factor( check_dat2$pred_class ), 
                 as.factor( check_dat2$orig_class ))



############################### h2o autoML #####################################

aml <- h2o.automl( x = X, 
                   y = Y,
                   training_frame = dat.train,
                   max_models = 3,
                   seed = 1)

