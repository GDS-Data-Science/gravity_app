## create lasso data set 
# create dummy variables
cols <- c( "iso_d", "PR_o", "CL_o", "typeOfViolence_o", 
           "PR_d", "CL_d", "typeOfViolence_d" )
dat_lasso <- dummy_cols( dat, select_columns = cols, 
                         remove_first_dummy = TRUE, remove_selected_columns = TRUE )



################################################################################
#                                LASSO model                                   #
################################################################################

mse_round <- rep( NA, 10 )

## run cross validation glm.net
for( i in 1:10 ){
   
   # nest data frame by country of origin
   dat_nest <- dat_lasso %>% 
      group_by( iso_o ) %>% 
      nest()
   
   # generate training and testing data 
   dat_tt <- dat_nest %>% 
      mutate( train = map( data, ~ filter( .x, !( year %in% v[ i, ] ))), 
              test = map( data, ~filter( .x, year %in% v[ i, ]))) %>% 
      mutate( train_x = map( train, ~ select( .x, -c( newarrival ))), 
              train_y = map( train, ~ select( .x, newarrival )), 
              test_x = map( test, ~ select( .x, -c( newarrival ))), 
              test_y = map( test, ~ select( .x, newarrival )))
   
   # estimate LASSO with training data 
   fit <- dat_tt %>% 
      mutate( model = map2( .x = train_x, .y = train_y,
                            ~ cv.glmnet( as.matrix( .x ), as.matrix( .y ),
                                         family = "poisson" )))
   
   # predict values for test data 
   dat_p <- fit %>% 
      mutate( p = map2( .x = model, .y = test_x, 
                        ~ predict( .x, newx = as.matrix( .y ), 
                                   type = "response", s = .x$lambda.min ))) 
   
   # calculate MSE 
   dat_final <- dat_p %>% 
      mutate( test_y = map( test_y, ~ as.numeric( .x$newarrival )),
              mse = map2( .x = test_y, .y = p, 
                          ~ rmse( as.numeric( .x ), .y ))) %>% 
      unnest( mse )
   
   mse_round[i] <- mean( dat_final$mse )
}


mse_final <- mean( mse_round )

################################################################################
#                                Random Forest                                 # 
################################################################################

## create cluster 
cl <- makePSOCKcluster( 5 )
registerDoParallel( cl )

## random forest model
model <- train( 
   newarrival ~ ., 
   tuneLength = 5, 
   data = dat_rf, 
   method = "ranger", 
   trControl = trainControl(
      method = "cv", 
      number = 5,
      allowParallel = TRUE,
      verboseIter = TRUE,
      seeds = NULL
   ),
   metric = "RMSE"
)

## stop cluster
stopCluster( cl )

method = "timeslice", 
initialWindow = 5,
horizon = 1,


#cementry

# ### year wise cross validation 
# ## create timeslice indices for CV with panel data
# # block length
# t <- length( unique( dat$year ))
# # number of cross-sectional observations
# n <- length( unique( dat$Id ))
# # length of window and horizon
# window_length <- 12
# horizon_length <- 1
# # add-on for block
# add_train <- rep( seq( 0, by = t, length.out = n ), each = window_length )
# add_test <- rep( seq( 0, by = t, length.out = n ), each = horizon_length )
# # generate y
# y <- 1:t
# # create timeslice object
# time_slice <- createTimeSlices( y, initialWindow = window_length, horizon = horizon_length )
# # length of list
# list_length <- length( time_slice$train )
# # adjust for rest of the blocks
# for( i in 1:list_length ){
#    time_slice$train[[i]] <- rep( time_slice$train[[i]], n ) + add_train
#    time_slice$test[[i]] <- rep( time_slice$test[[i]], n ) + add_test
# }


