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
dat <- dat %>% select( -c( "Country_o", "Country_d", 
                           "Nyear_log_o", "dead_log_o",
                           "Nyear_log_d", "dead_log_d", 
                           "Nyear_conf_o", "Nyear_conf_d" )) %>% 
               arrange( year )

### create factors 
cols <- c( "iso_o", "iso_d","PR_o", "CL_o", "typeOfViolence_o", 
           "PR_d", "CL_d", "typeOfViolence_d", "island_o", "island_d", 
           "landlocked_o", "landlocked_d", "index0asylum", "contig",
           "comlang_off", "comlang_ethno", "colony", "comcol", "col45", "smctry" )
dat[ cols ] <- lapply( dat[ cols ], factor )


### create data sets for country based estimations 
dat_iso_o <- dat %>% select( -c( island_o, area_o, landlocked_o ))

### create classification variable for all zero country pairs 
# idx <- dat %>% group_by( Id ) %>% 
#                summarise( tot = sum( newarrival )) %>% 
#                filter( tot == 0 )

#dat$zero <- factor( ifelse( dat$Id %in% idx$Id, 0, 1 ), labels = c( "no", "yes" ))
dat$zero <- factor( ifelse( dat$newarrival > 0, 1, 0 ), labels = c( "no", "yes"))

### create training and testing data 
set.seed( 42 )
ID <- unique( dat$Id )
idx <- sample( ID, length( ID ) * 0.6 )
dat_train <- subset( dat, Id %in% idx )
dat_test <- subset( dat, !( Id %in% idx ))

### create caret time windows
window.length <- 17

timecontrol_class   <- trainControl(
   method            = "timeslice",
   initialWindow     = window.length * length( unique( dat_train$Id )),
   horizon           = length( unique( dat_train$Id )),
   skip              = length( unique( dat_train$Id )),
   verboseIter       = TRUE,
   selectionFunction = "best",
   fixedWindow       = TRUE,
   savePredictions   = "final",
   allowParallel     = TRUE, 
   classProbs        = TRUE 
)

timecontrol_reg     <- trainControl(
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


# dat_train_class <- select( dat_train, zero, year, ends_with( "_o" ))
dat_train_class <- select( dat_train, -c( newarrival, Id ))
# dat_train_reg <- dat_train %>%
#                  filter( zero == "yes" ) %>%
#                  select( -c( zero, Id ))

# dat_test_class <- select( dat_test, zero, year, ends_with( "_o" ))
dat_test_class <- select( dat_test, -c( newarrival, Id ))
# dat_test_reg <-  dat_test %>%
#                  filter( zero == "yes" ) %>%
#                  select( -c( zero, Id ))












