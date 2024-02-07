################################################################################
################################################################################
##                                                                            ##
##                                Function for                                ##
##                      Sequential Test and Training Data                     ##
##                                                                            ##
################################################################################
################################################################################

## usage ##
# function to create training and testing data sets for sequential data. Can be
# applied to both longitudinal and time series data.


seq_fun <- function( df, t, train_window, test_window, fix_start = FALSE, fix_end = FALSE ){
  ## required packages
  require( dplyr )
  ## vector of sequence periods
  year <- sort( unique( df[ , c( t )]))
  year_n <- length( year )
  ## output listd
  n <- year_n - ( train_window + test_window ) + 1
  df_list <- list( train = vector( mode = "list", length = n ),
                   test = vector( mode = "list", length = n ))
  ## create subsets of training and testing data
  for( i in 1:n ){
   if( fix_start == FALSE & fix_end == FALSE ){
      year_train <- year[ i:( i + train_window - 1 )]
      year_test <- year[( i + train_window ):( i + train_window + test_window - 1 )]
   } else if( fix_start == TRUE & fix_end == FALSE ) {
      year_train <- year[ 1:( i + train_window - 1 )]
      year_test <- year[( i + train_window ):( i + train_window + test_window - 1 )]
   } else if( fix_start == FALSE & fix_end == TRUE ) {
      year_train <- year[ i:( i + train_window - 1 )]
      year_test <- year[( i + train_window ):year_n ]
   } else{
      year_train <- year[ 1:( i + train_window - 1 )]
      year_test <- year[( i + train_window ):year_n ]
   }
    df_list$train[[i]] <- df %>% filter( year %in% year_train )
    df_list$test[[i]] <- df %>% filter( year %in% year_test )
  }
  return( df_list )
}

### Documentation ###
# df            a data frame either longitudinal data or (univariate) time series
# t             the variable that describes the sequence, for example year,
#               as a character string
# train_window  length of the (initial) training window
# test_window   length of the (initial) testing window
# fix_start     FALSE (default) keeps window size of training data constant
#               TRUE returns a variable window size for the training data
#               beginning with the first year
# fix_end       FALSE (default) keeps window size of testing data constant
#               TRUE returns a variable window size for the testing data
#               stopping with the last year

### bug fixes
# - does not work with tibbles

