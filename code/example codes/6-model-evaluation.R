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

# load test data
covid_test = read_tsv("data/clean/covid_test.tsv")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-covid_test$case_fatality_rate)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-covid_test$case_fatality_rate)^2))

# print nice table
tibble(Method = c("Ridge", "Lasso"), `Test RMSE` = c(ridge_RMSE, lasso_RMSE)) %>%
  write_tsv("results/model-evaluation.tsv")