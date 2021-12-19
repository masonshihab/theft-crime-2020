## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
options(scipen = 0, digits = 3)  # controls number of significant digits printed


## ---- message = FALSE------------------------------------------------------------------------------------------------------
library(rpart)         # to train decision trees
library(rpart.plot)    # to plot decision trees
library(randomForest)  # random forests
library(gbm)           # boosting
library(tidyverse)     # tidyverse
library(kableExtra)    # for printing tables
library(cowplot)       # for side by side plots


## --------------------------------------------------------------------------------------------------------------------------
theft_train=read_csv("../data/clean/theft_train.csv")


## ---- cache=TRUE-----------------------------------------------------------------------------------------------------------
set.seed(471) # set seed for reproducibility

mvalues = seq(3,15, by = 1)
oob_errors = numeric(length(mvalues))
ntree = 500
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(theftrate ~. -fips, mtry = m, data = theft_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}
tibble(m = mvalues, oob_err = oob_errors) %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  theme_bw()


## --------------------------------------------------------------------------------------------------------------------------
set.seed(471) # set seed for reproducibility
rf_4 = randomForest(theftrate ~ .-fips, mtry = 4, data = theft_train)


## --------------------------------------------------------------------------------------------------------------------------
rf_4$importance 


## --------------------------------------------------------------------------------------------------------------------------
varImpPlot(rf_4,n.var = 10)

