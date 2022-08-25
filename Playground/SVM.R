################################################################################
################################################################################
##                                                                            ##
##                       Playground for Gravity Model                         ##
##                                  SVM                                       ##
##                                                                            ##
################################################################################
################################################################################

ctrl <- trainControl( method = "repeatedcv",
                      number = 4,
                      repeats = 2,
                      verboseIter = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE )

svm_class <- train( zero ~., 
                    data = dat_train_class, 
                    method = "svmRadial", 
                    trControl = ctrl, 
                    preProcess = c( "center", "scale" ),
                    tuneLength = 1 )
