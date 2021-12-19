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
library(gbm)           # boosting


## ----cache=TRUE-------------------------------------------------------------------------------------------------------------------------------
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

theftdata = theftdata %>% mutate(weightedtrump = pertrump*pop_density/sum(pop_density))
  
 theftdata  %>%  ggplot(aes(x = pertrump, y = med_income)) + geom_smooth() + geom_point()

