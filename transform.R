################################################################################
################################################################################
##                                                                            ##
##                            Data Transformation                             ##
##                                                                            ##
################################################################################
################################################################################


### load packages 
library( caret )
library( corrplot )
library( dplyr )
library( e1071 )


### load data 
load( "../Data/WorkData/impuData.Rdata" )

### turn numeric vectors into factors 
num_vars <- c( "index0asylum", "contig", "comlang_off", 
               "comlang_ethno", "colony", "comcol", "col45", "smctry" )

est_dat <- est_dat %>% mutate_at( all_of( num_vars ), as.factor )

### test for skewness
numeric_dat <- est_dat %>% select_if( is.numeric )
skews <- apply( numeric_dat, 2, skewness )

### histograms of worst variables 
hist( est_dat$pop_o, 50 )
hist( est_dat$CPI_o, 50 )
hist( est_dat$area_o, 50 )
hist( est_dat$Nyear_conflict_o, 50 )

### check for multicollinearity and near zero variance 
# variance 
idx <- nearZeroVar( est_dat )
est_dat[ , idx ]

# correlation 
correlations <- cor( numeric_dat )
corrplot( correlations, oder = "hclust" )

### generate train predict data 
# turn into data frame as seq-fun does not work with tibbles
est_dat <- as.data.frame( est_dat )
# define length of training window based on number of years minus 5 (3 years prediction times 3)
train_window <- length( unique( est_dat$year )) - 6 
dat <- seq_fun( df = est_dat, t = "year", train_window = train_window, 
                test_window = 3, fix_start = TRUE )

dat_UT <- dat

### transform data sets
## list to save transformation models
trans_model <- vector( mode = "list", length = 4 )

for( i in 1:4 ){
    
   ### transform variables: unskew using box-cox transformation, scale, center
   tmp_train  <- dat$train[[i]]
   tmp_test   <- dat$test[[i]]
   
   tmp_train_response <- tmp_train[ , c( "iso_o", "iso_d", "year", "newarrivals" )]
   tmp_test_response  <- tmp_test[ , c( "iso_o", "iso_d", "year", "newarrivals" )]
   
   trans      <- preProcess( tmp_train[ , -c( 36 )], method = c( "YeoJohnson", "scale", "center" ))
   tmp_train  <- predict( trans, tmp_train[ , -c( 36 )])
   tmp_test   <- predict( trans, tmp_test[ , -c( 36 )])

   ### join data together again 
   tmp_train  <- tmp_train %>%  left_join( tmp_train_response, by = c( "iso_o", "iso_d", "year" ))
   tmp_test   <- tmp_test %>%  left_join( tmp_test_response, by = c( "iso_o", "iso_d", "year" ))
   
   dat$train[[i]] <- tmp_train
   dat$test[[i]]  <- tmp_test 
   trans_model[[i]] <- trans
}


# function to convert factor variable to numeric in a data frame
convert_factor_to_numeric <- function(df) {
   df <- df %>%
         mutate( year = as.numeric( as.character( year )))
   return(df)
}

# turn year into numeric variable again
dat$train    <- lapply( dat$train, convert_factor_to_numeric )
dat$test     <- lapply( dat$test, convert_factor_to_numeric )
dat_UT$train <- lapply( dat_UT$train, convert_factor_to_numeric )
dat_UT$test  <- lapply( dat_UT$test, convert_factor_to_numeric )

# saving data and transformation model
save( trans_model, file = "../Data/WorkData/trans_model.Rdata" )
save( dat, file = "../Data/WorkData/est_dat.Rdata" )
save( dat_UT, file = "../Data/WorkData/dat_UT.Rdata" )





