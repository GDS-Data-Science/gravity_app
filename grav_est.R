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
load( "../Data/WorkData/impuData17.Rdata" )

#### turn CL and PR into factors
for( i in 1:5 ){
   impu17[[i]]$CL_o <- as.factor( impu17[[i]]$CL_o )
   impu17[[i]]$CL_d <- as.factor( impu17[[i]]$CL_d )
   impu17[[i]]$PR_o <- as.factor( impu17[[i]]$PR_o )
   impu17[[i]]$PR_d <- as.factor( impu17[[i]]$PR_d )
}

#### run estimations
# estimation equation 
form <- formula(  newarrival ~   
                  index0asylum + log( dist ) +  contig + comlang_off +
                  comlang_ethno + colony + comcol + col45 + smctry +
                  log( GDP_PP_d ) + log( GDP_PP_o ) + log( pop_d ) + log( pop_o ) +
                  CL_o  +  CL_d  +  PR_o  +  PR_d  +
                  asinh( CPI_d ) + asinh( CPI_o ) + dead_d + dead_log_d +
                  dead_o + dead_log_o + Nyear_conf_d + Nyear_log_d + Nyear_conf_o +
                  Nyear_log_o |  iso_o + iso_d + year )

### loop through five imputed data sets 
for( i in 1:5 ){
   ## alpaca 
   # poisson 
   #assign( paste0( "lama1_", i ), feglm( formula = form, data = impu17[[i]], family = poisson( )))

   # negative binomial
   #assign( paste0( "lama2_", i ), feglm.nb( formula = form, data = impu17[[i]] ))

   ## FENmlm
   # poisson
   assign( paste0( "fennel1_", i ), femlm( form, data = impu17[[i]], family = "poisson" ))     

   # negative binomial
   #assign( paste0( "fennel2_", i ), femlm( form, data = impu17[[i]], family = "negbin" ))
}

#### model average 
mean_coefs <- ( fennel1_1$coefficients + fennel1_2$coefficients + fennel1_3$coefficients +
                fennel1_4$coefficients + fennel1_5$coefficients )/5

#### save results 
est_models_poisson <- list( fennel1_1, fennel1_2, fennel1_3, fennel1_4, fennel1_5 )
save(  est_models_poisson, file = "../Results/estimations_poisson_ind.Rdata" )

# est_models_nb <- list( fennel2_1, fennel2_2, fennel2_3, fennel2_4, fennel2_5 )
# save(  est_models_nb, file = "../Results/estimations_nb_ind.Rdata" )





