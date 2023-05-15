################################################################################
################################################################################
##                                                                            ##
##                        Playground for Gravity Model                        ##
##                           MLR3 BENCHMARKING                                ##
##                                                                            ##
################################################################################
################################################################################

library(mlr3)
library(mlr3spatiotempcv)
library(mlr3learners)
library(dplyr)
library(mlr3viz)
library(readr)
library(mlr3verse)

# Load data
load("./GravityApp/input_data/impuData17.RData")
dat0 <- impu17[[1]]
#
#   Check distribution of new arrivals
#
plot(table(dat0$newarrival))
sum(dat0$newarrival<1)
sum(dat0$newarrival>1)

# Create arrivals categorical variable
#
dat0_cat <- dat0 %>%
  mutate( arrivals_cat = as.factor(ifelse(newarrival>0, 1, 0)))

# Subset dataset to only keep a few countries
#dat1 <- subset( dat0_cat, iso_o %in% c( "LUX", "SYR", "LBN", "ATG", "CAN", "EGY" ))

#### select variables from dat
dat <- dat0_cat %>% select( -c( "Country_o", "Country_d", 
                                "best_est_o", "Nyear_conf_d",
                                "best_est_d", "Nyear_conf_o",
                                "dead_log_o", "Nyear_log_o", 
                                "dead_log_d", "Nyear_log_d")) %>% 
  arrange( year ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_all(., as.numeric) %>%
  mutate(arrivals_cat = as.factor(arrivals_cat))

dat <- dat %>%
  arrange(year) %>%
  dplyr::select(-newarrival)

#####################################
#####################################
#
#     Benchmarking with mlr3
#
#####################################
#####################################

#################################
#   1 -- Write mlr3 tasks
#################################

tsk_newarrival = as_task_classif(dat, target = "arrivals_cat")
print(tsk_newarrival)

#################################
#   2 -- Write mlr3 learners
#################################

# mlr_learners lists all mlr3 learners present
glmnet = lrn("classif.glmnet")
glmnet$predict_type

xgboost = lrn("classif.xgboost")
xgboost$param_set$ids()

lrn("classif.xgboost")$help()

randomforest = lrn("classif.ranger")
lrn("classif.ranger")$help()

#####################################################################
#   3 -- Write custom resampling taking into account temporal order
#####################################################################

calculate_last_row <- function(year_n){
  
  dat %>%
    filter(year<=year_n) %>%
    nrow(.)
}

calculate_first_row <- function(year_n){
  
  calculate_last_row(year_n-1) +1
}

train_sets1 = list(seq(1,calculate_last_row(2013)),
                   seq(calculate_first_row(2001), calculate_last_row(2014)),
                   seq(calculate_first_row(2002), calculate_last_row(2015)),
                   seq(calculate_first_row(2003), calculate_last_row(2016)),
                   seq(calculate_first_row(2004), calculate_last_row(2017)),
                   seq(calculate_first_row(2005), calculate_last_row(2018)),
                   seq(calculate_first_row(2006), calculate_last_row(2019)),
                   seq(calculate_first_row(2007), calculate_last_row(2020)))

test_sets1 = list(seq(calculate_first_row(2014), calculate_last_row(2014)),
                  seq(calculate_first_row(2015), calculate_last_row(2015)),
                  seq(calculate_first_row(2016), calculate_last_row(2016)),
                  seq(calculate_first_row(2017), calculate_last_row(2017)),
                  seq(calculate_first_row(2018), calculate_last_row(2018)),
                  seq(calculate_first_row(2019), calculate_last_row(2019)),
                  seq(calculate_first_row(2020), calculate_last_row(2020)),
                  seq(calculate_first_row(2021), calculate_last_row(2021)))

custom <- rsmp("custom")
custom$instantiate(tsk_newarrival, train_sets=train_sets1, test_sets=test_sets1)

# custom re-sampling, taking into account temporal order

design = benchmark_grid(
  tasks = tsk_newarrival,
  learners = lrns(c("classif.glmnet", "classif.xgboost", "classif.ranger"),
                  predict_type = "response", predict_sets = c("train", "test")),
  resamplings = custom
)

print(design)
#
#   Run design
#
bmr1 = benchmark(design)

measures = list(
  msr("classif.acc", predict_sets = "train", id = "acc_train"),
  msr("classif.acc", id = "acc_test")
)

tab = bmr1$aggregate(measures)
print(tab[, .(task_id, learner_id, acc_train, acc_test)])

autoplot(bmr1) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

as.data.table(bmr1$resample_results$resample_result)

bmr_small = bmr1$clone(deep = TRUE)
autoplot(bmr_small, type = "roc")

save(bmr1, file="bmr_cat_8resamp.RData")

#################################
#   1 -- Write mlr3 tasks
#################################
# 
# task_newarrival = as_task_regr(dat, target = "newarrival", group="year")
# task_newarrival$
# #################################
# #   2 -- Write mlr3 learners
# #################################
# 
# # mlr_learners lists all mlr3 learners present
# glmnet = lrn("regr.glmnet", family="poisson")
# xgboost = lrn("regr.xgboost")
# randomforest = lrn("regr.ranger")
# 
# #################################
# #   3 -- Write custom resampling
# #################################
# 
