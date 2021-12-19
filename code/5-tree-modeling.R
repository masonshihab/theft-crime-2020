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
library(glmnetUtils)
library(randomForest)
library(cowplot)
set.seed(471)

#RandomForest

## ---- cache=TRUE---------------------------------------------------------------------------------------------------------------------------
set.seed(471) # set seed for reproducibility

mvalues = seq(5,20, by = 1)
oob_errors = numeric(length(mvalues))
ntree = 500
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(theftrate ~. -fips -state - county, mtry = m, data = theft_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}

mtries = tibble(m = mvalues, oob_err = oob_errors)
mplot = mtries %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) + labs(y = "Out of Bag Error", x = "Features Available per Split (m)") +
  theme_bw()

lowest_m_row = mtries[mtries$oob_err == min(mtries$oob_err), ]
lowest_m = lowest_m_row$m


ggsave(filename = "../results/mplot.png", 
       plot = mplot, 
       device = "png", 
       width = 6, 
       height = 4)

## ------------------------------------------------------------------------------------------------------------------------------------------
set.seed(471)
rf_lowest_m = randomForest(theftrate ~. -fips -state - county, mtry = lowest_m, data = theft_train)

## ------------------------------------------------------------------------------------------------------------------------------------------
oob_lowest_m = tibble(ntree = 1:500, oob_err = rf_lowest_m$mse)

treeplot = oob_lowest_m %>%
  ggplot(aes(x = ntree, y = oob_err)) +
  geom_line() + labs(y = "Out of Bag Error", x = "Number of Trees Used") + theme_bw()

ggsave(filename = "../results/treeplot.png", 
       plot = treeplot, 
       device = "png", 
       width = 6, 
       height = 4)

## ----cache=TRUE----------------------------------------------------------------------------------------------------------------------------
set.seed(471) # set seed for reproducibility
rf_opt = randomForest(theftrate ~ .-fips -state -county, mtry = lowest_m, ntree = 500, data = theft_train, importance = TRUE)

save(rf_opt, file = "../results/rf_opt.Rda")

## ----varimp, echo = TRUE, fig.width = 11, fig.height = 6, out.width = "100%", fig.align='center', fig.cap = "Variable Importance Plot for the optimal random forest model.", fig.pos = "H"----
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/varImpPlot.png")
varImpPlot(rf_opt, n.var = 10)
dev.off()


#Boosting

## ----cache=TRUE----------------------------------------------------------------------------------------------------------------------------
set.seed(471) # for reproducibility (DO NOT CHANGE)
# TODO: Fit random forest with interaction depth 1

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




## ------------------------------------------------------------------------------------------------------------------------------------------
ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_1$cv.error, Depth = 1),
  tibble(ntree = 1:ntrees, cv_err = gbm_2$cv.error, Depth = 2),
  tibble(ntree = 1:ntrees, cv_err = gbm_3$cv.error, Depth = 3)
) %>% mutate(Depth = factor(Depth))

# plot CV errors

mins = cv_errors %>% group_by(Depth) %>% summarise(min_err = min(cv_err))


## ----deptherr, echo = TRUE, fig.width = 5, fig.height = 3, out.width = "100%", fig.align='center', fig.cap = "CV Error by Trees and Interaction Depth (with min error for each depth dashed)", fig.pos = "H"----
cverrplot = cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = Depth)) +
  geom_line() + theme_bw() + 
  geom_hline(aes(yintercept = min_err, color = Depth), 
             data = mins, linetype = "dashed") + 
  labs(y = "CV Error", x = "Trees") + scale_y_log10()

ggsave(filename = "../results/cverrplot.png", 
       plot = cverrplot, 
       device = "png", 
       width = 6, 
       height = 4)


## ----relinf--------------------------------------------------------------------------------------------------------------------------------
gbm_fit_optimal = gbm_3
optimal_num_trees = gbm.perf(gbm_fit_optimal, plot.it = FALSE) 
save(gbm_fit_optimal, file = "../results/gbm_fit_optimal.Rda")
summary(gbm_3, n.trees = optimal_num_trees, plotit = FALSE) %>% tibble() %>%
  head(12)  %>% 
  write_tsv("../results/gbm_opt_rel_imp.tsv")


## ------------------------------------------------------------------------------------------------------------------------------------------
housing = plot(gbm_3, i.var = "housing_density", 
               n.trees = optimal_num_trees)


## ------------------------------------------------------------------------------------------------------------------------------------------
FIRE = plot(gbm_3, i.var = "PctEmpFIRE", 
            n.trees = optimal_num_trees)



## ------------------------------------------------------------------------------------------------------------------------------------------
trump = plot(gbm_3, i.var = "pertrump", n.trees = 
               optimal_num_trees) 


## ------------------------------------------------------------------------------------------------------------------------------------------
bens = plot(gbm_3, i.var = "unemp_bens_possible", n.trees = 
              optimal_num_trees) 


## ------------------------------------------------------------------------------------------------------------------------------------------
police = plot(gbm_3, i.var = "police_funding_score", n.trees = 
                optimal_num_trees) 


## ------------------------------------------------------------------------------------------------------------------------------------------
healthins = plot(gbm_3, i.var = "no_health_ins", n.trees = 
                   optimal_num_trees) 


## ------------------------------------------------------------------------------------------------------------------------------------------
rel_imp_plots = plot_grid(nrow = 2, housing, bens, healthins, police, trump, FIRE)

ggsave(filename = "../results/rel_imp_plots.png", 
       plot = rel_imp_plots, 
       device = "png", 
       width = 7, 
       height = 4)