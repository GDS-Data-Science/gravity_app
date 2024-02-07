################################################################################
################################################################################
##                                                                            ##
##                        Meta Script Gravity Model                           ##
##                                                                            ##
################################################################################
################################################################################

### link for PRIO API
link <- "https://api.viewsforecasting.org/fatalities002_2023_11_t01/cm"

### setting global variables 
nowcasting <- TRUE                  ## TRUE, FALSE
last_year  <- 2023                  ## latest year available in nowcasting or ASR
new_years  <- c(( last_year + 1 ):( last_year + 3 ))  ## years to predict
end        <- length( c( 2000:max( new_years ))) ## replacement row end
start      <- end - 4               ## replacement row start
replace    <- start - 1             ## row to replace with

### load seq_fun function
source( "seq_fun.R", echo = TRUE )

### variables to keep
global_names <- c( "global_names", "nowcasting", "last_year", "new_years",  
                   "start", "end", "replace", "seq_fun", "link" )

### create flow data set 
## setting nowcasting condition if nowcasting flow data is to be added
source( "flows.R", echo = TRUE )
rm( list = setdiff( ls(), global_names ))

### update fatalities predictions from ViEWS API
source( "ViEWS_API.R", echo = TRUE )
rm( list = setdiff( ls(), global_names ))

### update percentages and index0asylum 
source( "percent_index.R", echo = TRUE )
rm( list = setdiff( ls(), global_names ))

### create modelling and prediction data set
source( "predictionData.R", echo = TRUE )
rm( list = setdiff( ls(), global_names ))

### transformation of covariates and division into train and predictive data 
source( "transform.R", echo = TRUE )
rm( list = setdiff( ls(), global_names ))

### estimation of prediction models 
source( "grav_est.R", echo = TRUE )
rm( list = setdiff( ls(), global_names ))

### creation of stock data list for prediction
source( "stocks.R", echo = TRUE )
rm( list = ls())

