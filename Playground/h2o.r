################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                                    h2o                                     ##
##                                                                            ##
################################################################################
################################################################################

### remove zero variable from dat
dat <- select( dat, -c( zero ))

#### generate training and testing data
dat.train <- subset( dat, year %in% c( 2000:2020 ))
dat.test <- subset( dat, year %in% c( 2021 ))

#### initiate h2o cluster
h2o.init( nthreads = -1, max_mem_size = "6g" )
h2o.clusterInfo()

#### turn data into a h2o object
dat.h2o.train <- as.h2o( dat.train )
str( dat.h2o.train )
dat.h2o.test <- as.h2o( dat.test )
str( dat.h2o.test )

#### generate training and testing data
# set.seed( 42 )
# dat.split = h2o.splitFrame( data = dat.h2o, ratios = .8 )
# dat.train = dat.split[[1]]
# dat.test  = dat.split[[2]]

#### create dependent and explanatory variables
Y <- "newarrival"
X <- setdiff( names( dat ), "newarrival" )


#### define hyper parameters 
hyper_params <- list( tweedie_power   = c( 1.1, 1.2, 1.5, 1.7, 1.9 ), 
                      ntrees          = c( 200, 500, 1000 ), 
                      learn_rate      = c( 0.001, 0.01, 0.1 ), 
                      max_depth       = c( 3, 5, 9 ),
                      sample_rate     = c( 0.8, 1.0 ),
                      col_sample_rate = c( 0.2, 0.5, 1.0 ))


search_criteria <- list( strategy   = "RandomDiscrete",
                         max_models = 40,
                         seed       = 1 )


#### conduct grid search 
tic()
gbm_grid <- h2o.grid( algorithm       = "gbm",
                      distribution    = "tweedie",
                      grid_id         = "grav_mod",
                      x               = X,
                      y               = Y,
                      training_frame  = dat.h2o.train,
                      seed            = 42,
                      nfolds          = 10,
                      keep_cross_validation_predictions = TRUE,
                      hyper_params    = hyper_params,
                      search_criteria = search_criteria,
                      parallelism     = 8 )
toc()

## evaluate results 
sorted_grid <- h2o.getGrid( "grav_mod", sort_by = "rmse", decreasing = FALSE )

#### choose best model 
best_gbm <- h2o.getModel( gbm_grid@model_ids[[1]])
summary( best_gbm )

tic()
xg_grid <- h2o.grid( algorithm       = "xgboost",
                     distribution    = "tweedie",
                     grid_id         = "grav_mod_xg",
                     x               = X,
                     y               = Y,
                     training_frame  = dat.h2o.train,
                     seed            = 42,
                     nfolds          = 10,
                     keep_cross_validation_predictions = TRUE,
                     hyper_params    = hyper_params,
                     search_criteria = search_criteria,
                     parallelism     = 8 )
toc()

## evaluate results 
sorted_grid_xg <- h2o.getGrid( "grav_mod_xg", sort_by = "rmse", decreasing = FALSE )

#### choose best model 
best_xg <- h2o.getModel( xg_grid@model_ids[[1]])
summary( best_xg )

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble( x              = X,
                                 y              = Y,
                                 training_frame = dat.h2o,
                                 base_models    = list( best_gbm, best_xg ))

stop()
save.image( )
 
# Eval ensemble performance on a test set
perf_ens <- h2o.performance( ensemble, newdata = dat.test )


#### predict test data 
model <- ensemble
#model <- best_rf
#model <- best_gbm

pred <- round( h2o.predict( model, newdata = dat.h2o ), 0 ) 
pred$predict <- ifelse( pred$predict < 0, 0, pred$predict )
h2o.hist( pred, 50 )
summary( pred )
summary( dat.h2o$newarrival )

#### plot rootogram
predicted <- table( as.data.frame( pred )$predict )
predicted <- predicted[ 1:600 ]
origin <- table( as.data.frame( dat.h2o )$newarrival )
origin <- origin[ 1:600 ]
rootogram( origin, predicted )

#### create kernel density plots and confusion matrix 
# generate data frame 
check_dat <- data.frame( original  = as.data.frame( dat.h2o )$newarrival,
                         predicted = as.data.frame( pred )$predict )

# turn it into long format and plot density plot  
check_dat  %>%
   pivot_longer( cols      = c( "original", "predicted" ), 
                 names_to  = "origin", 
                 values_to = "val" ) %>%
   ggplot( aes( x = log10( val ), fill = origin )) +
   geom_density( alpha = .3 )

# scatter plot
check_dat %>% 
   mutate( res = predicted - original ) %>% 
   ggplot( aes( x = asinh( original ), y = asinh( predicted ))) + 
   geom_point( aes( color = res )) +
   geom_abline( intercept = 0, slope = 1 )

# create data frame for confusion matrix 
check_dat2 <- check_dat %>% 
              mutate( orig_class = ifelse( .$original > 0, 1, 0 ), 
                      pred_class = ifelse( .$predicted > 0, 1, 0 ))
# confusion matrix 
confusionMatrix( as.factor( check_dat2$pred_class ), 
                 as.factor( check_dat2$orig_class ))




