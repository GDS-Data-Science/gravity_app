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

### select variables from dat
dat <- select( dat, -c( "Country_o", "Country_d", "Id" ))

### create factors 
cols <- c( "iso_o", "iso_d", "year", "PR_o", "CL_o", "typeOfViolence_o", 
           "PR_d", "CL_d", "typeOfViolence_d", "island_o", "island_d", 
           "landlocked_o", "landlocked_d", "index0asylum", "contig",
           "comlang_off", "comlang_ethno", "colony", "comcol", "col45", "smctry" )
dat[ cols ] <- lapply( dat[ cols ], factor )


### year wise cross validation 
set.seed( 42 )




