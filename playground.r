################################################################################
################################################################################
##                                                                            ##
##                       Playground for Gravity Model                         ##
##                                                                            ##
################################################################################
################################################################################


#### load packages
library( caret )
library( doParallel )
library( dplyr )
library( fastDummies )
library( ggplot2 )
library( glmnet )
library( hexbin )
library( Metrics )
library( purrr )
library( ranger )
library( readr )
library( tidyr )
library( xgboost )

#### read in data
load( "../Data/WorkData/impuData17.Rdata" )
dat <- impu17[[1]]

################################################################################
#                                    EDA                                       #
################################################################################

# structure of the data 
str( dat )
# summary of the variables
summary( dat )
## structure of the data
# length of time series (balanced panel)
table( table( dat$Id ))
# number of unique country of origin 
n_o <- length( unique( dat$iso_o ))
# number of unique country of asylum
n_d <- length( unique( dat$iso_d ))
# number of unique country pairs 
n_p <- length( unique( dat$Id ))
# equality of countries in 'o' and 'd' category
mean( unique( dat$iso_o ) %in% unique( dat$iso_d ))

## visualization 
# distribution of dependent variable 
hist( dat$newarrival, main = "Distribution of 'newarrival' " )
# share of zeros
mean( dat$newarrival == 0 )
# plot time series 
dat %>% group_by( iso_o, year ) %>% 
        summarise( total = sum( newarrival )) %>% 
        filter( total > 100 ) %>% 
        ggplot( aes( x = year, y = total, color = iso_o )) +
        geom_line( )
# impact of PR and CL on newarrival
ggplot( data = dat, mapping = aes( x = log( newarrival ), 
                                   y = ..density..,
                                   colour = as.factor( PR_o ))) +
   geom_freqpoly(  ) +
   ggtitle( "Impact of political rights on the distribution of newarrival" )
   
ggplot( data = dat, mapping = aes( x = log( newarrival ),
                                   y = ..density..,
                                   colour = as.factor( CL_o ))) +
   geom_freqpoly(  ) +
   ggtitle( "Impact of civil liberty on the distribution of newarrival" )
# impact of violence type on newarrival
ggplot( data = dat, mapping = aes( x = log( newarrival ), 
                                   y = ..density..,
                                   colour = as.factor( typeOfViolence_o ))) +
   geom_freqpoly(  ) +
   ggtitle( "Impact of violence type on the distribution of newarrival" )
# impact of island status 
ggplot( data = dat, mapping = aes( x = as.factor( island_o ), y = log( newarrival ))) +
   geom_boxplot()
ggplot( data = dat, mapping = aes( x = as.factor( island_d ), y = log( newarrival ))) +
   geom_boxplot()
# impact of landlocked status 
ggplot( data = dat, mapping = aes( x = as.factor( landlocked_o ), y = log( newarrival ))) +
   geom_boxplot()
# impact of prima facie 
ggplot( data = dat, mapping = aes( x = as.factor( index0asylum ), y = log( newarrival ))) +
   geom_boxplot()
# correlation between GDP_PP and newarrival
ggplot( data = dat ) +
   geom_hex( mapping = aes( x = GDP_PP_o, y = log( newarrival )))
ggplot( data = dat ) +
   geom_hex( mapping = aes( x = GDP_PP_d, y = log( newarrival )))
# correlation between best_est_o and newarrival
ggplot( data = dat ) +
   geom_hex( mapping = aes( x = best_est_o, y = newarrival ))
# correlation between distance and newarrival
ggplot( data = dat, mapping = aes( x = dist, y = log( newarrival ))) + 
   geom_boxplot( mapping = aes( group = cut_number( dist, 10 )))


################################################################################
################################################################################
#                            Analysis with caret                               #
################################################################################
################################################################################

################################################################################
#                             Data Preparation                                 #
################################################################################

## change categorical variables into factors 
cols_caret <- c( "iso_o", "iso_d", "year", "PR_o", "CL_o", "typeOfViolence_o", 
                 "PR_d", "CL_d", "typeOfViolence_d", "island_o", "island_d", 
                 "landlocked_o", "landlocked_d", "index0asylum", "contig",
                 "comlang_off", "comlang_ethno", "colony", "comcol", "col45", "smctry" )
dat[ cols_caret ] <- lapply( dat[ cols_caret ], factor )
dat_caret <- dat 

## create timeslice indices for CV with panel data 
# block length
t <- length( unique( dat_caret$year ))
# number of cross-sectional observations
n <- length( unique( dat_caret$Id ))
# length of window and horizon
window_length <- 12 
horizon_length <- 1
# add-on for block
add_train <- sort( rep( seq( 0, n, by = t ), window_length ))
add_test <- sort( rep( seq( 0, n, by = t ), horizon_length ))
# generate y 
y <- 1:t
# create timeslice object 
time_slice <- createTimeSlices( y, initialWindow = window_length, horizon = horizon_length )
# length of list 
list_length <- length( time_slice$train )
# adjust for rest of the blocks
for( i in 1:list_length ){
   time_slice$train[[i]] <- rep( time_slice$train[[i]], n ) + add_train
   time_slice$test[[i]] <- rep( time_slice$test[[i]], n ) + add_test
}


################################################################################
#                              Data Preparation                                #
################################################################################

### year wise cross validation 
set.seed( 42 )
# create cv matrix
v <- t( replicate( 10, sample( 2000:2021, 2 )))

# select variables from dat
dat <- select( dat, -c( "Country_o", "Country_d", "Id" ))

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

## trial with caret 

dat_nest <- dat %>% 
            group_by( id ) %>% 
            nest( )

dat_m <- dat_nest %>% 
         mutate( model = map( data, 
                              ~ train( 
                                 y ~ ., 
                                 data = .x,
                                 method = "glmnet",
                                 family = "poisson",
                                 tuneGrid = expand.grid( 
                                                alpha = seq( 0, 1, by = 0.1 ), 
                                                lambda = seq( 0, 25, by = 1 )),
                                 trControl = trainControl( 
                                    method = "cv", 
                                    number = 10 ),
                                 preProcess = c( "zv", "nzv", "scale", "center" ),
                                 savePredictions = TRUE )))


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
                              method = "timeslice", 
                              initialWindow = 5,
                              horizon = 2,
                              allowParallel = TRUE,
                              verboseIter = TRUE, 
                              seeds = NULL
             ),
             metric = "RMSE"
          )

## stop cluster
stopCluster( cl )
             
             
################################################################################
#                                  XG Boost                                    #
################################################################################
             

### to do 
# Adjust createTimeSlices function
# Create a train/test control object for all estimations
# program unnested LASSO
# program unnested RF
# run blog example XGboost
# program unnested XGboost
# program nested versions


             
             