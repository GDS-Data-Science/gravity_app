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
load( "../Data/WorkData/est_dat.Rdata" )

#### run estimations
# estimation equation 
form <- formula(  newarrivals ~   
                  index0asylum + dist +  contig + comlang_off +
                  comlang_ethno + colony + comcol + col45 + smctry +
                  GDP_PP_d + GDP_PP_o + pop_d + pop_o + 
                  PR_o + PR_d + CL_o + CL_d +
                  CPI_d + CPI_o + dead_d + dead_log_d +
                  dead_o + dead_log_o |  iso_o + iso_d + year )                 # + Nyear_conf_d + Nyear_log_d + Nyear_conf_o + Nyear_log_o


years <- c( last_year-3, last_year-2, last_year-1, last_year )

est_models_poisson <- vector( mode = "list", length = 4 )

for( i in 1:4 ){
   df <- dat$train[[i]]
   #### FENmlm
   # poisson
   assign( paste0( "est_", years[i]), femlm( form, data = df, family = "poisson" ))
   est_models_poisson[[i]] <- get( paste0( "est_", years[i]) )
}

### save models                       
save( est_models_poisson, file = "../Results/estimations.Rdata" )






