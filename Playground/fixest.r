################################################################################
################################################################################
##                                                                            ##
##                          Estimation Gravity App                            ##
##                                                                            ##
################################################################################
################################################################################

#### load packages
library( fixest )

#### read in data 
load( "../Data/WorkData/impuData17.Rdata" )
dat <- impu17[[1]] 

#### run estimations
# estimation equation 
form <- formula(  newarrival ~   
                  index0asylum + log( dist ) +  contig + comlang_off +
                  comlang_ethno + colony + comcol + col45 + smctry +
                  log( GDP_PP_d ) + log( GDP_PP_o ) + log( pop_d ) + log( pop_o ) +
                  log( CL_o ) + log( CL_d ) + log( PR_o ) + log( PR_d ) +
                  asinh( CPI_d ) + asinh( CPI_o ) + dead_d + dead_log_d +
                  dead_o + dead_log_o + Nyear_conf_d + Nyear_log_d + Nyear_conf_o +
                  Nyear_log_o |  iso_o + iso_d + year )

## FENmlm
# poisson
fennel1_1 <- femlm( form, data = impu17[[i]], family = "poisson" )







