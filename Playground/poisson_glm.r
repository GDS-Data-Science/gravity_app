################################################################################
################################################################################
##                                                                            ##
##                           Poisson - GLMMadaptive                           ##
##                                                                            ##
################################################################################
################################################################################

#### load packages
library( GLMMadaptive )
library( dplyr )


#### read in data
load( "../Data/WorkData/impuData17.Rdata" )
dat <- impu17[[1]]


################################################################################
#                               Data preparation                               #
################################################################################

#### select variables from dat
dat <- dat %>% select( -c( "Country_o", "Country_d", "iso_o", "iso_d", 
                           "best_est_o", "Nyear_conflict_o", 
                           "best_est_d", "Nyear_conflict_d" )) %>% 
       arrange( year )

#### create factors 
cols <- c( "PR_o", "CL_o", "typeOfViolence_o", 
           "PR_d", "CL_d", "typeOfViolence_d", 
           "island_o", "island_d", 
           "landlocked_o", "landlocked_d", "index0asylum", "contig",
           "comlang_off", "comlang_ethno", "colony", "comcol", "col45", "smctry" )
dat[ cols ] <- lapply( dat[ cols ], factor )

#### create training and test data set 
set.seed( 42 )
ID <- unique( dat$Id )
idx <- sample( ID, length( ID )*0.6 )
dat_train <- subset( dat, Id %in% idx )
dat_test <- subset( dat, !( Id %in% idx ))

#### create CV time window 
train.list <- test.list <- vector( mode = "list", length = 16 )
t.train <- 2005:2020 
t.test <- 2006:2021
for( i in 1:16 ){
   train.list[[i]] <- c(( t.train[i] - 5 ):t.train[i])
   test.list[[i]] <- c( t.test[i])
}

################################################################################
#                                 Estimations                                  #
################################################################################

#### standard poisson with penalty
for( i in 1:9 ){
tmp <- subset( dat_train, year %in% train.list[[i]] )
gm1 <- mixed_model( fixed = newarrival ~ ., random = ~ 1 | Id, data = tmp,
                    family = poisson(), penalized = TRUE )
}








