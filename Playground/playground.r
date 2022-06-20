################################################################################
################################################################################
##                                                                            ##
##                      Playground for Gravity Model                          ##
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

#### explorative data analysis 
source( "Playground/EDA.r" )

#### data preparation
source( "Playground/Data_preb.r" )

#### elastic net
source( "Playground/ela_net.r" )





