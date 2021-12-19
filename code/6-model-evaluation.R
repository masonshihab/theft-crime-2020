# load libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(usdata)
library(tm)
library(stringr)
library(gbm)
library(glmnetUtils) # boosting
library(randomForest)
library(kableExtra)
library(cowplot)

theft_train = read_csv("../data/clean/theft_train.csv") 
theft_test = read_csv("../data/clean/theft_test.csv") 

set.seed(471)

# ridge prediction error

# load ridge fit object
load("../results/ridge_fit.Rda")

# load lasso fit object
load("../results/lasso_fit.Rda")

# load ridge fit object
load("../results/elnet_fit.Rda")

# load lasso fit object
load("../results/rf_opt.Rda")

# load lasso fit object
load("../results/gbm_fit_optimal.Rda")

set.seed(471)
ridge_predictions = predict(ridge_fit, 
                            newdata = theft_test, 
                            s = "lambda.1se") %>% as.numeric()

ridge_RMSE = sqrt(mean((ridge_predictions-theft_test$theftrate)^2))



# lasso prediction error
set.seed(471)
lasso_predictions = predict(lasso_fit, 
                            newdata = theft_test, 
                            s = "lambda.1se") %>%
  as.numeric()

lasso_RMSE = sqrt(mean((lasso_predictions-theft_test$theftrate)^2))




# elnet prediction error
set.seed(471)
elnet_predictions = predict(elnet_fit,
                            alpha = elnet_fit_best$alpha,
                            newdata = theft_test, 
                            s = "lambda.1se") %>%
  as.numeric()



elnet_RMSE = sqrt(mean((elnet_predictions-theft_test$theftrate)^2))

set.seed(471)

# intercept-only prediction error
training_mean_response = mean(theft_test$theftrate)
constant_RMSE = sqrt(mean((training_mean_response-theft_test$theftrate)^2))



#RF
set.seed(471)
rf_predictions = predict(rf_opt, newdata = theft_test) 

rf_RMSE = sqrt(mean((rf_predictions-theft_test$theftrate)^2))

#Boosting
gbm_predictions = predict(gbm_fit_optimal, n.trees = optimal_num_trees,
                          newdata = theft_test)

gbm_RMSE = sqrt(mean((gbm_predictions-theft_test$theftrate)^2))




# print nice table


tibble(Ridge = ridge_RMSE, Lasso = lasso_RMSE, `Intercept-only` = constant_RMSE, 
       Elastic_Net = elnet_RMSE, Random_Forest = rf_RMSE, Boosting = gbm_RMSE) %>% 
  pivot_longer(everything(), names_to = "Model", values_to = "Test_RMSE") %>% 
  arrange(Test_RMSE) %>%
  write_tsv("../results/model-evaluation.tsv")
  