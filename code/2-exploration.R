# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(readxl)
library(usdata)
library(tm)
library(stringr)
library(gbm)
library(glmnetUtils)

# read in the cleaned data
theft_train = read_csv("../data/clean/theft_train.csv")



# Regression-Based Models 

## Lasso Regression

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


## Elastic Net


## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit = cva.glmnet(theftrate ~ . -fips -county -state, # formula notation, as usual
                       nfolds = 10,               # number of folds
                       data = theft_train)   # data to run on


## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit$alpha


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot_cva_glmnet(elnet_fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit_best = extract_best_elnet(elnet_fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit_best$alpha


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(elnet_fit_best)


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot_glmnet(elnet_fit_best, theft_train)


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot_glmnet(elnet_fit_best, theft_train, features_to_plot = 10)


## ----nonzero-std-coefficients-table-----------------------------------------------------------------------------------------------------
extract_std_coefs(elnet_fit_best, theft_train) %>% 
  filter(coefficient != 0) %>% arrange(desc(abs(coefficient)))



# Tree-Based Models

## Boosting


gbm_1 = gbm(theftrate ~ . -fips -state -county,
            distribution = "gaussian",
            n.trees = 1000,
            interaction.depth = 1,
            shrinkage = 0.1,
            cv.folds = 5,
            data = theft_train)

set.seed(471) # for reproducibility (DO NOT CHANGE)
# TODO: Fit random forest with interaction depth 2

gbm_2 = gbm(theftrate ~ . -fips -state -county,
            distribution = "gaussian",
            n.trees = 1000,
            interaction.depth = 2,
            shrinkage = 0.1,
            cv.folds = 5,
            data = theft_train)

set.seed(471) # for reproducibility (DO NOT CHANGE)
# TODO: Fit random forest with interaction depth 3


gbm_3 = gbm(theftrate ~ . -fips -state -county,
            distribution = "gaussian",
            n.trees = 1000,
            interaction.depth = 3,
            shrinkage = 0.1,
            cv.folds = 5,
            data = theft_train)


## ---------------------------------------------------------------------------------------------------------------------------------------------
ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_1$cv.error, Depth = 1),
  tibble(ntree = 1:ntrees, cv_err = gbm_2$cv.error, Depth = 2),
  tibble(ntree = 1:ntrees, cv_err = gbm_3$cv.error, Depth = 3)
) %>% mutate(Depth = factor(Depth))

# plot CV errors

mins = cv_errors %>% group_by(Depth) %>% summarise(min_err = min(cv_err))

gbm.perf(gbm_3, plot.it = FALSE)


## ----deptherr, echo = TRUE, fig.width = 5, fig.height = 3, out.width = "100%", fig.align='center', fig.cap = "CV Error by Trees and Interaction Depth (with min error for each depth dashed)", fig.pos = "H"----
cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = Depth)) +
  geom_line() + theme_bw() + 
  geom_hline(aes(yintercept = min_err, color = Depth), 
             data = mins, linetype = "dashed") + 
  labs(y = "CV Error", x = "Trees") + scale_y_log10()


## ----relinf-----------------------------------------------------------------------------------------------------------------------------------
gbm_fit_optimal = gbm_3
optimal_num_trees = gbm.perf(gbm_1, plot.it = FALSE) 
summary(gbm_3, n.trees = optimal_num_trees, plotit = FALSE) %>% tibble() %>%
  head(10) 


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(gbm_3, i.var = "housing_density", 
     n.trees = optimal_num_trees)


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(gbm_3, i.var = "poor_fair_health", 
     n.trees = optimal_num_trees)



## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(gbm_3, i.var = "pertrump", n.trees = 
       optimal_num_trees) 


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(gbm_3, i.var = "PctEmpFIRE", n.trees = 
       optimal_num_trees) 


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(gbm_3, i.var = "saversperpop", n.trees = 
       optimal_num_trees) 


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(gbm_3, i.var = "pop_density", n.trees = 
       optimal_num_trees) 


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(gbm_3, i.var = "unemp_bens_possible", n.trees = 
       optimal_num_trees) 



## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit = cva.glmnet(theftrate ~ . -fips -county -state, # formula notation, as usual
                       nfolds = 10,               # number of folds
                       data = theft_train)   # data to run on


## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit$alpha


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot_cva_glmnet(elnet_fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit_best = extract_best_elnet(elnet_fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------
elnet_fit_best$alpha


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(elnet_fit_best)


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot_glmnet(elnet_fit_best, theft_train)


## ---------------------------------------------------------------------------------------------------------------------------------------------
plot_glmnet(elnet_fit_best, theft_train, features_to_plot = 10)



