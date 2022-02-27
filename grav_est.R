################################################################################
################################################################################
##                                                                            ##
##                           Estimation Gravity App                           ##
##                                                                            ##
################################################################################
################################################################################

#### load packages
library( alpaca )
library( broom )
library( dplyr )
library( FENmlm )
library( ggplot2 )
library( hrbrthemes )
library( randomForest )
library( readr )
library( tidyr )
library( viridis )

#### read in data 
load( "../Data/WorkData/impuData17.Rdata" )

for( i in 1:5 ){
   impu17[[i]]$PR_o <- as.factor( impu17[[i]]$PR_o )
   impu17[[i]]$PR_d <- as.factor( impu17[[i]]$PR_d )
   impu17[[i]]$CL_o <- as.factor( impu17[[i]]$CL_o )
   impu17[[i]]$CL_d <- as.factor( impu17[[i]]$CL_d )
}

#### run estimations
# estimation equation 
form <- formula( newarrival ~ index0asylum + log( dist ) + contig + comlang_off +
                 comlang_ethno + colony + comcol + col45 + smctry + log( GDP_PPP_d ) +
                 log( GDP_PPP_o ) + log( GDP_PP_d ) + log( GDP_PP_o ) + log( pop_d ) +
                 log( pop_o ) + CL_o + CL_d + PR_o + PR_d + 
                 asinh( CPI_d ) + asinh( CPI_o ) + dead_d + dead_log_d + 
                 dead_o + dead_log_o + Nyear_conf_d + Nyear_log_d + Nyear_conf_o + 
                 Nyear_log_o | iso_o + iso_d )

### loop through five imputed data sets 
for( i in 1:5 ){
   ## alpaca 
   # poisson 
   #assign( paste0( "lama1_", i ), feglm( formula = form, data = impu17[[i]], family = poisson( )))

   # negative binomial
   #assign( paste0( "lama2_", i ), feglm.nb( formula = form, data = impu17[[i]] ))

   ## FENmlm
   # poisson
   assign( paste0( "fennel1_", i ), femlm( form, data = impu17[[i]], family = "poisson", na.rm = TRUE ))

   # negative binomial
   #assign( paste0( "fennel2_", i ), femlm( form, data = impu17[[i]], family = "negbin", na.rm = TRUE ))
}

#### save results 
est_models <- list( fennel1_1, fennel1_2, fennel1_3, fennel1_4, fennel1_5 )
save(  est_models, file = "../Results/estimations.Rdata" )

#### mean results over 5 data sets
coeffs <- ( coef( fennel1_1 ) + coef( fennel1_2 ) + coef( fennel1_3 ) + coef( fennel1_4 ) + coef( fennel1_5 ))/5

# ### evaluation
# ## plot functions
# density_plot <- function( dat, title ){
#       dat %>% pivot_longer( cols = c( "model", "true" )) %>%
#       ggplot( aes( x = asinh( value ), group = name, fill = name )) +
#       geom_density( adjust=1.5, alpha=.4 ) +
#       scale_fill_viridis( discrete=TRUE ) +
#       scale_color_viridis( discrete=TRUE ) +
#       xlab( "asinh( newarrival )") +
#       ggtitle( title ) +
#       theme_classic( )
# }
# 
# scatter_plot <- function( dat, title ){
#       ggplot( dat, aes( x = asinh( true ), y = asinh( model ))) +
#       geom_point( ) +
#       geom_abline( intercept = 0, slope = 1, color = "red", size = 1 ) +
#       ggtitle( title ) +
#       theme_classic( )
# }
# 
# #calculate fitted values for all four models
# for( j in 1:2 ){
#    for( i in 1:5 ){
#       assign( paste( "fitted_lama", j, i, sep = "_" ),
#               predict( get( paste0( "lama", j, "_", i  )), type = "response" ))
#       assign( paste( "fitted_fennel", j, i, sep = "_" ),
#               predict( get( paste0( "fennel", j, "_", i  )), type = "response" ))
#    }
# }
# 
# mean_fitted_lama1 <- round( rowMeans( cbind( fitted_lama_1_1, fitted_lama_1_2,
#                                       fitted_lama_1_3, fitted_lama_1_4, fitted_lama_1_5 )), 0 )
# mean_fitted_lama2 <- round( rowMeans( cbind( fitted_lama_2_1, fitted_lama_2_2,
#                                       fitted_lama_2_3, fitted_lama_2_4, fitted_lama_2_5 )), 0 )
# mean_fitted_fennel1 <- round( rowMeans( cbind( fitted_fennel_1_1, fitted_fennel_1_2,
#                                         fitted_fennel_1_3, fitted_fennel_1_4, fitted_fennel_1_5 )), 0 )
# mean_fitted_fennel2 <- round( rowMeans( cbind( fitted_fennel_2_1, fitted_fennel_2_2,
#                                         fitted_fennel_2_3, fitted_fennel_2_4, fitted_fennel_2_5 )), 0 )
# 
# ## plots fitted and real values
# # lama1
# dat_lama1 <- as.data.frame( cbind( mean_fitted_lama1, impu15[[1]]$newarrival ))
# names( dat_lama1 ) <- c( "model", "true" )
# density_plot( dat_lama1, title = "Model Poisson 'alpaca' package" )
# ggsave( "../Results/fitted_lama1_1.pdf" )
# 
# scatter_plot( dat_lama1, title = "Model Poisson 'alpaca' package" )
# ggsave( "../Results/fitted_lama1_2.pdf" )
# 
# # lama2
# dat_lama2 <- as.data.frame( cbind( mean_fitted_lama2, impu15[[1]]$newarrival ))
# names( dat_lama2 ) <- c( "model", "true" )
# density_plot( dat_lama2, title = "Model Negative Binomial 'alpaca' package" )
# ggsave( "../Results/fitted_lama2_1.pdf" )
# 
# scatter_plot( dat_lama1, title =  "Model Negative Binomial 'alpaca' package" )
# ggsave( "../Results/fitted_lama2_2.pdf" )
# 
# # fennel1
# dat_fennel1 <- as.data.frame( cbind( mean_fitted_fennel1, impu15[[1]]$newarrival ))
# names( dat_fennel1 ) <- c( "model", "true" )
# density_plot( dat_fennel1, title = "Model Poisson 'FENmlm' package" )
# ggsave( "../Results/fitted_fennel1_1.pdf" )
# 
# scatter_plot( dat_fennel1, title =  "Model Poisson 'FENmlm' package" )
# ggsave( "../Results/fennel1_2.pdf" )
# 
# # fennel2
# dat_fennel2 <- as.data.frame( cbind( mean_fitted_fennel2, impu15[[1]]$newarrival ))
# names( dat_fennel2 ) <- c( "model", "true" )
# density_plot( dat_fennel2, title = "Model Negative Binomial 'FENmlm' package" )
# ggsave( "../Results/fitted_fennel2_1.pdf" )
# 
# scatter_plot( dat_fennel2, title =  "Model Negative Binomial 'FENmlm' package" )
# ggsave( "../Results/fennel2_2.pdf" )
# 
# # mean square error
# fitted_mse_lama1 <- ( sum( dat_lama1$true - dat_lama1$model )^2 )/nrow( dat_lama1 )
# fitted_mse_lama2 <- ( sum( dat_lama2$true - dat_lama2$model )^2 )/nrow( dat_lama2 )
# fitted_mse_fennel1 <- ( sum( dat_fennel1$true - dat_fennel1$model )^2 )/nrow( dat_fennel1 )
# fitted_mse_fennel2 <- ( sum( dat_fennel2$true - dat_fennel2$model )^2 )/nrow( dat_fennel2 )
#
# ## predict last two years
# for( j in 1:2 ){
#    for( i in 1:5 ){
#       assign( paste( "predict_lama", j, i, sep = "_" ),
#               predict( get( paste0( "lama", j, "_", i  )), newdata = impu19[[i]], type = "response" ))
#       assign( paste( "predict_fennel", j, i, sep = "_" ),
#               predict( get( paste0( "fennel", j, "_", i  )), newdata = impu19[[i]], type = "response" ))
#    }
# }
#
# mean_predict_lama1 <- round( rowMeans( cbind( predict_lama_1_1, predict_lama_1_2,
#                                               predict_lama_1_3, predict_lama_1_4, predict_lama_1_5 )), 0 )
# mean_predict_lama2 <- round( rowMeans( cbind( predict_lama_2_1, predict_lama_2_2,
#                                               predict_lama_2_3, predict_lama_2_4, predict_lama_2_5 )), 0 )
# mean_predict_fennel1 <- round( rowMeans( cbind( predict_fennel_1_1, predict_fennel_1_2,
#                                                 predict_fennel_1_3, predict_fennel_1_4, predict_fennel_1_5 )), 0 )
# mean_predict_fennel2 <- round( rowMeans( cbind( predict_fennel_2_1, predict_fennel_2_2,
#                                                 predict_fennel_2_3, predict_fennel_2_4, predict_fennel_2_5 )), 0 )
#
# ## plots fitted and real values
# # lama1
# dat_lama1 <- as.data.frame( cbind( mean_predict_lama1, impu19[[1]]$newarrival ))
# names( dat_lama1 ) <- c( "model", "true" )
# density_plot( dat_lama1, title = "Model Poisson 'alpaca' package" )
# ggsave( "../Results/predicted_lama1_1.pdf" )
#
# scatter_plot( dat_lama1, title = "Model Poisson 'alpaca' package" )
# ggsave( "../Results/predicted_lama1_2.pdf" )
#
# # lama2
# dat_lama2 <- as.data.frame( cbind( mean_predict_lama2, impu19[[1]]$newarrival ))
# names( dat_lama2 ) <- c( "model", "true" )
# density_plot( dat_lama2, title = "Model Negative Binomial 'alpaca' package" )
# ggsave( "../Results/predicted_lama2_1.pdf" )
#
# scatter_plot( dat_lama1, title =  "Model Negative Binomial 'alpaca' package" )
# ggsave( "../Results/predicted_lama2_2.pdf" )
#
# # fennel1
# dat_fennel1 <- as.data.frame( cbind( mean_predict_fennel1, impu19[[1]]$newarrival ))
# names( dat_fennel1 ) <- c( "model", "true" )
# density_plot( dat_fennel1, title = "Model Poisson 'FENmlm' package" )
# ggsave( "../Results/predicted_fennel1_1.pdf" )
#
# scatter_plot( dat_fennel1, title =  "Model Poisson 'FENmlm' package" )
# ggsave( "../Results/predicted_fennel1_2.pdf" )
#
# # fennel2
# dat_fennel2 <- as.data.frame( cbind( mean_predict_fennel2, impu19[[1]]$newarrival ))
# names( dat_fennel2 ) <- c( "model", "true" )
# density_plot( dat_fennel2, title = "Model Negative Binomial 'FENmlm' package" )
# ggsave( "../Results/predicted_fennel2_1.pdf" )
#
# scatter_plot( dat_fennel2, title =  "Model Negative Binomial 'FENmlm' package" )
# ggsave( "../Results/predicted_fennel2_2.pdf" )
#
# # mean square error
# predict_mse_lama1 <- ( sum( dat_lama1$true - dat_lama1$model )^2 )/nrow( dat_lama1 )
# predict_mse_lama2 <- ( sum( dat_lama2$true - dat_lama2$model )^2 )/nrow( dat_lama2 )
# predict_mse_fennel1 <- ( sum( dat_fennel1$true - dat_fennel1$model )^2 )/nrow( dat_fennel1 )
# predict_mse_fennel2 <- ( sum( dat_fennel2$true - dat_fennel2$model )^2 )/nrow( dat_fennel2 )
#








