################################################################################
################################################################################
##                                                                            ##
##                      Playground for Gravity Model                          ##
##                                                                            ##
################################################################################
################################################################################

#### load packages
library( caret )
library( countreg )
library( doParallel )
library( dplyr )
library( fastDummies )
library( ggplot2 )
library( glmnet )
library( h2o )
library( hexbin )
library( Metrics )
library( pscl )
library( purrr )
library( ranger )
library( readr )
library( scales )
library( solitude )
library( tictoc )
library( tidyr )
library( xgboost )


#### read in data
load( "../Data/WorkData/impuData17.Rdata" )
dat <- impu17[[1]]

#### explorative data analysis 
# source( "Playground/EDA.r" )

#### data preparation
source( "Playground/Data_preb.r", echo = TRUE )

#### h2o script
source( "Playground/h2o.r", echo = TRUE )

#### xgboost 
#source( "Playground/xgboost.r", echo = TRUE )

#### elastic_net
#source( "Playground/ela_net.r", echo = TRUE )

#### random forest 
#source( "Playground/rand_forrest.r", echo = TRUE )

save.image( "h2o.Rdata" )
