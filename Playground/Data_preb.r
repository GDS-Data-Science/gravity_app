################################################################################
################################################################################
##                                                                            ##
##                 Playground for Gravity Model - Data                        ##
##                                                                            ##
################################################################################
################################################################################

#### load packages 
library( caret )
library( dplyr )
library( purrr )
library( readr )
library( tidyr )

#### read in data
load( "../Data/WorkData/impuData17.Rdata" )
dat <- impu17[[1]]

################################################################################
#                              Data Preparation                                #
################################################################################

### year wise cross validation 
set.seed( 42 )
## create timeslice indices for CV with panel data
# block length
t <- length( unique( dat$year ))
# number of cross-sectional observations
n <- length( unique( dat$Id ))
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

### select variables from dat
dat <- select( dat, -c( "Country_o", "Country_d", "Id" ))

### create factors 
cols <- c( "iso_o", "iso_d", "year", "PR_o", "CL_o", "typeOfViolence_o", 
           "PR_d", "CL_d", "typeOfViolence_d", "island_o", "island_d", 
           "landlocked_o", "landlocked_d", "index0asylum", "contig",
           "comlang_off", "comlang_ethno", "colony", "comcol", "col45", "smctry" )
dat[ cols ] <- lapply( dat[ cols ], factor )





