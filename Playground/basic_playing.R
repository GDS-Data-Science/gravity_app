#############################################################
#############################################################
#############################################################
#
#   Basic playing around with data
#
#############################################################
#############################################################
#############################################################
#
library(dplyr)
library(lme4)

load("./GravityApp/input_data/impuData17.RData")

dat0 <- impu17[[1]]

# Design matrix with only numerics
dat1 <- subset( dat0, iso_o %in% c( "LUX", "SYR", "LBN", "ATG", "CAN", "EGY" ))

#### select variables from dat
dat <- dat1 %>% select( -c( "Country_o", "Country_d", 
                           "best_est_o", "Nyear_conflict_o", 
                           "best_est_d", "Nyear_conflict_d", "dead_log_o", "Nyear_log_o", "dead_log_d")) %>% 
  arrange( year ) %>%
  mutate_if(is.character, is.factor)

### Only select numeric factors and create corr matrix
X <- select_if(dat, is.numeric) %>%
  select(., -newarrival)

corr_matrix <- cor(X)
#
#   Apply simple linear model
#
lm_test <- lm( newarrival ~ ., dat)

summary(lm_test)
plot(lm_test)
#
#   Apply simple mixed model where random effects are time
#
mixed_model <- lmer(newarrival ~ . - year + (iso_o|year),
                    data = dat)

isSingular(X)

summary(mixed_model)

