################################################################################
################################################################################
##                                                                            ##
##                          Estimation Gravity App                            ##
##                                                                            ##
################################################################################
################################################################################

#### load packages
library( alpaca )
library( dplyr )
library( fixest )
library( readr )
library( tidyr )


#### read in data 
dat <- read.csv( "../Data/WorkData/gravity.csv" )

#### run estimations
# estimation equation 
form <- formula(  newarrival ~   
                  index0asylum + log( dist ) +  contig + comlang_off +
                  comlang_ethno + colony + comcol + col45 + smctry +
                  log( GDP_Level_d ) + log( GDP_Level_o ) + 
                  log( CL_o ) + log( CL_d ) + log( PR_o ) + log( PR_d ) +
                  id_dead_d + ln_best_d + id_dead_o + ln_best_o + 
                  id_year_Con_d + id_year_Con_o |  iso_o + iso_d + year )


## FENmlm
# poisson
fennel1 <- femlm( form, data = dat, family = "poisson" )


#### save results 
save(  fennel1, file = "../Results/estimations_poisson_andrea.Rdata" )

stop()
flow_predictions <-  predict( fennel1, newdata = y, type = "response" ), 
   x = est_models_poisson, y = impu22 )

# round( rowMeans( flow_predictions ), 0 )
pre_newarrival <- data.frame( iso_o = impu22[[1]]$iso_o, 
                              iso_d = impu22[[1]]$iso_d, 
                              year = rep( 2021:2024, times = nrow( flow_predictions)/4 ),
                              var = round( rowMeans( flow_predictions ), 0 ))

##### checks
check1 <- pre_newarrival %>% group_by( year ) %>% summarise( tot = sum( var ))






