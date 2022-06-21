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

window.length <- 12

timecontrol   <- trainControl(
   method            = "timeslice",
   initialWindow     = window.length * length( unique( dat$Id )),
   horizon           = length( unique( dat$Id )),
   skip              = length( unique( dat$Id )),
   selectionFunction = "best",
   fixedWindow       = TRUE,
   savePredictions   = "final",
   allowParallel     = TRUE 
)


### select variables from dat
dat <- dat %>% select( -c( "Country_o", "Country_d", "Id" )) %>% 
               arrange( year )

### create factors 
cols <- c( "iso_o", "iso_d", "year", "PR_o", "CL_o", "typeOfViolence_o", 
           "PR_d", "CL_d", "typeOfViolence_d", "island_o", "island_d", 
           "landlocked_o", "landlocked_d", "index0asylum", "contig",
           "comlang_off", "comlang_ethno", "colony", "comcol", "col45", "smctry" )
dat[ cols ] <- lapply( dat[ cols ], factor )







