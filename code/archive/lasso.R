## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
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


## ---------------------------------------------------------------------------------------------------------------------------------------------
# install.packages("scales")              # dependency of plot_glmnet
source("functions/plot_glmnet.R")


## ---- message = FALSE-------------------------------------------------------------------------------------------------------------------------
set.seed(471) # set seed before cross-validation for reproducibility

lasso_fit = cv.glmnet(theftrate ~. -state -county -fips , alpha = 1, nfolds = 10, data = theft_train) 


## ----lasso-cv-plot, echo = FALSE, fig.width = 6, fig.height = 3, out.width = "100%", fig.align='center', fig.cap = "This is the CV plot for the 10-fold cross-validated lasso regression model on the training data.", fig.pos = "H"----
plot(lasso_fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------
lambda_lasso = lasso_fit$lambda.1se
sprintf("The value of lambda based on the one-standard-error rule: %f",
        lambda_lasso)


## ---------------------------------------------------------------------------------------------------------------------------------------------
num_features = lasso_fit$nzero[lasso_fit$lambda == lasso_fit$lambda.1se]
sprintf("The number of features (excluding intercept) selected (1se): %i", 
        num_features)


## ----lasso-nonzero-std-coefficients-table-----------------------------------------------------------------------------------------------------
extract_std_coefs(lasso_fit, theft_train) %>% 
  filter(coefficient != 0) %>% arrange(desc(coefficient))

## ---------------------------------------------------------------------------------------------------------------------------------------------
plot_glmnet(lasso_fit, theft_train)

