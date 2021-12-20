## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(glmnetUtils) # boosting
library(randomForest)
library(cowplot)
set.seed(471)


# install.packages("scales")              # dependency of plot_glmnet
source("functions/plot_glmnet.R")



theft_train = read_csv("../data/clean/theft_train.csv") 
theft_test = read_csv("../data/clean/theft_test.csv") 


#Ridge

set.seed(471) # set seed for reproducibility

ridge_fit = cv.glmnet(theftrate ~ .-fips -state -county,  # formula notation, as usual
                      alpha = 0,                 # alpha = 0 for ridge
                      nfolds = 10,               # number of folds
                      data = theft_train)   # data to run ridge on

# save the ridge fit object
save(ridge_fit, file = "../results/ridge_fit.Rda")



png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()


ridgeplot = plot_glmnet(ridge_fit, theft_train, features_to_plot = 8)
ggsave(filename = "../results/ridge-trace-plot.png", 
       plot = ridgeplot, 
       device = "png", 
       width = 6, 
       height = 4)

set.seed(471)


ridge_coef = extract_std_coefs(ridge_fit, theft_train) %>% 
  filter(coefficient != 0) %>% arrange(desc(abs(coefficient))) %>% head(10) %>% as.data.frame()

ridge_coef %>% write_tsv("../results/ridge-features-table.tsv")


#Lasso

set.seed(471) # set seed before cross-validation for reproducibility
lasso_fit = cv.glmnet(theftrate ~. -state -county -fips, alpha = 1, nfolds = 10, data = theft_train) 


png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()


# save the lasso fit object
save(lasso_fit, file = "../results/lasso_fit.Rda")



extract_std_coefs(lasso_fit, theft_train) %>% 
  filter(coefficient != 0) %>% arrange(desc(coefficient)) %>% 
  write_tsv("../results/lasso-features-table.tsv")


lassoplot = plot_glmnet(lasso_fit, theft_train)
ggsave(filename = "../results/lasso-trace-plot.png", 
       plot = lassoplot, 
       device = "png", 
       width = 6, 
       height = 4)


#Elastic_Net


set.seed(471)
elnet_fit = cva.glmnet(theftrate ~ .-fips -county -state, # formula notation, as usual
                       nfolds = 10,               # number of folds
                       data = theft_train)   # data to run on

save(elnet_fit, file = "../results/elnet_fit.Rda")

alphas = plot_cva_glmnet(elnet_fit)

ggsave(filename = "../results/alphas.png", 
       plot = alphas, 
       device = "png", 
       width = 10, 
       height = 7)




elnet_fit_best = extract_best_elnet(elnet_fit)



elnet_fit_best$alpha



png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/elnet-best-cv-plot.png")
plot(elnet_fit_best)
dev.off()

elnetplot = plot_glmnet(elnet_fit_best, theft_train, features_to_plot = 8)
ggsave(filename = "../results/elnet-trace-plot.png", 
       plot = elnetplot, 
       device = "png", 
       width = 6, 
       height = 4)

elnet_coef = extract_std_coefs(elnet_fit_best, theft_train) %>% 
  filter(coefficient != 0) %>% arrange(desc(coefficient)) %>% head(10) %>% as.data.frame()
  
elnet_coef %>% write_tsv("../results/elnet-features-table.tsv")
