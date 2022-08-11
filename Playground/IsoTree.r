################################################################################
################################################################################
##                                                                            ##
##                       Playground for Gravity Model                         ##
##                                IsoTree                                     ##
##                                                                            ##
################################################################################
################################################################################

#### load packages 
#library( devtools )
#install_github( "david-cortes/isotree" )
library(isotree)
library( pROC )

#### remove dependent variable from dat_train and dat_test
dat_iso_train <- select( dat_train, -c( zero, newarrival ))
dat_iso_test <- select( dat_test, -c( zero, newarrival ))
#### transform binary variable
zero_train <- ifelse( dat_train$zero == "yes", TRUE, FALSE )
zero_test <- ifelse( dat_test$zero == "yes", TRUE, FALSE )

#### hyper parameters
results_df <- vector( mode = "list", length = ( 4 * 3 ))
sample_size <- c( 180, 256, 2500, 5000 )
n_trees <- c( 100, 1000, 5000 )
k = 1

### run loop over hyper parameters
for( i in 1:4 ){
   for( j in 1:3 ){

# Isolation forest 
mod1 <- isolation.forest( 
            data = dat_iso_train, 
            sample_size = sample_size[i], 
            ntrees = n_trees[j], 
            ndim = 1, 
            categ_split_type = "single_categ" )

mod1_pred <- predict( mod1, dat_iso_train )

# Density isolation forest 
mod2 <- isolation.forest( 
            data = dat_iso_train, 
            sample_size = sample_size[i], 
            ntrees = n_trees[j], 
            ndim = 1, 
            categ_split_type = "single_categ", 
            scoring_metric="density" )

mod2_pred <- predict( mod2, dat_iso_train )

# Fair-cut forest 
mod3 <- isolation.forest( 
           data = dat_iso_train, 
           sample_size = sample_size[i], 
           ntrees = n_trees[j], 
           ndim = 1, 
           categ_split_type = "single_categ", 
           prob_pick_pooled_gain = 1 )

mod3_pred <- predict( mod3, dat_iso_train )

# results data frame 
results_df[[k]] <- data.frame(
                  Model = c(
                           "Isolation Forest",
                           "Density Isolation Forest",
                           "Fair-Cut Forest"
               ),
                  AUROC = c(
                           pROC::auc( zero_train, mod1_pred ),
                           pROC::auc( zero_train, mod2_pred ),
                           pROC::auc( zero_train, mod3_pred )
               )
             )

      
      print( Sys.time())
      print(k)
      k = k + 1
   }
}

#### confusion matrices best model
mod2_pred <- factor( ifelse( mod2_pred >= .5, 1, 0 ), labels = c( "no", "yes" ))
mod2_pred_test <- factor( ifelse( mod2_pred_test >= .5, 1, 0 ), labels = c( "no", "yes" ))
confusionMatrix( mod2_pred, dat_train$zero )
confusionMatrix( mod2_pred_test, dat_test$zero )




