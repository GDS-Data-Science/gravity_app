################################################################################
################################################################################
##                                                                            ##
##                 Playground for Gravity Model - Data                        ##
##                                                                            ##
################################################################################
################################################################################

################################################################################
#                              Data Preparation                                #
################################################################################

### select variables from dat
dat <- dat %>% select( -c( "Country_o", "Country_d" )) %>% 
   arrange( year )

### create factors 
cols <- c( "iso_o", "iso_d","PR_o", "CL_o", "typeOfViolence_o", 
           "PR_d", "CL_d", "typeOfViolence_d", "island_o", "island_d", 
           "landlocked_o", "landlocked_d", "index0asylum", "contig",
           "comlang_off", "comlang_ethno", "colony", "comcol", "col45", "smctry" )
dat[ cols ] <- lapply( dat[ cols ], factor )


### create data sets for country based estimations 
dat_iso_o <- dat %>% select( -c( island_o, area_o, landlocked_o ))

### create training and testing data 
set.seed( 42 )
ID <- unique( dat$Id )
idx <- sample( ID, length( ID ) * 0.6 )
dat_train <- subset( dat, Id %in% idx )
dat_test <- subset( dat, !( Id %in% idx ))

### create caret time windows
window.length <- 5

timecontrol   <- trainControl(
   method            = "timeslice",
   initialWindow     = window.length * length( unique( dat_train$Id )),
   horizon           = length( unique( dat_train$Id )),
   skip              = length( unique( dat_train$Id )),
   verboseIter       = TRUE,
   selectionFunction = "best",
   fixedWindow       = TRUE,
   savePredictions   = "final",
   allowParallel     = TRUE 
)















