################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                                    GLM                                     ##
##                                                                            ##
################################################################################
################################################################################

ctrl_reg   <- trainControl( method = "repeatedcv",
                            number = 3,
                            repeats = 1,
                            verboseIter = TRUE )


glm_g  <- train( newarrival ~ ., 
                 data       = dat_train_reg, 
                 method     = "glm",
                 family     = "poisson",
                 trControl  = ctrl_reg,
                 metric     = "RMSE",
                 preProc    = c( "center", "scale" ))


glm_b  <- train( newarrival ~ ., 
                 data       = dat_train_reg, 
                 method     = "bayesglm",
                 family     = "Gamma",
                 trControl  = ctrl_reg,
                 metric     = "RMSE",
                 preProc    = c( "center", "scale" ))





