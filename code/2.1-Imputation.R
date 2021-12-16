## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(usdata)
library(tm)
library(stringr)
library(randomForest)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(readr)

getwd()

## ---------------------------------------------------------------------------------------------------------------------------------------------
merge11 = read_csv("../data/clean/merge11fix.csv")


## ---------------------------------------------------------------------------------------------------------------------------------------------
url = "https://raw.githubusercontent.com/themarshallproject/incarceration-census/main/census_incarceration.csv"
incar_raw = read_csv(url) %>% select(FIPS, state, county, total_population_20, incarcerated_20)%>%
  mutate(incar_rate = incarcerated_20/total_population_20) %>%
  select(incar_rate, FIPS) %>% rename(fips = FIPS) %>% ungroup()


## ---------------------------------------------------------------------------------------------------------------------------------------------
incarvars = merge11 %>% inner_join(incar_raw, by = "fips") %>% select(-totalpop) %>% drop_na() %>% as.data.frame()


## ---------------------------------------------------------------------------------------------------------------------------------------------
incar_0 = incarvars %>% filter(incar_rate == 0)


## ---------------------------------------------------------------------------------------------------------------------------------------------
incar_complete = incarvars %>% filter(!(incar_rate == 0)) %>% drop_na()


## ---------------------------------------------------------------------------------------------------------------------------------------------
incar_0rf = incar_0 %>% select(-fips, -county, -state)


## ---------------------------------------------------------------------------------------------------------------------------------------------
incar_completerf = incar_complete %>% as.data.frame() %>% select(-fips, -county, -state)


## ---------------------------------------------------------------------------------------------------------------------------------------------
train_samples = sample(1:nrow(incar_completerf), round(0.8*nrow(incar_completerf)))
incar_train = incar_completerf %>% filter(row_number() %in% train_samples)
incar_test = incar_completerf %>% filter(!(row_number() %in% train_samples))


## ---------------------------------------------------------------------------------------------------------------------------------------------
set.seed(471)
rf_fit = randomForest(incar_rate ~ ., data = incar_train)


## ---------------------------------------------------------------------------------------------------------------------------------------------
rf_predictions = predict(rf_fit, newdata = incar_test)


## ---------------------------------------------------------------------------------------------------------------------------------------------
mean((rf_predictions - incar_test$incar_rate)^2)


## ---------------------------------------------------------------------------------------------------------------------------------------------
set.seed(471)
rf_fit2 = randomForest(incar_rate ~ ., data = incar_completerf)


## ---------------------------------------------------------------------------------------------------------------------------------------------
rf_predictions2 = predict(rf_fit2, newdata = incar_0rf)


## ---------------------------------------------------------------------------------------------------------------------------------------------
incar_predicted = incar_0
incar_predicted$incar_rate = rf_predictions2


## ---------------------------------------------------------------------------------------------------------------------------------------------
incar_final = rbind(incar_predicted, incar_complete) %>% drop_na() %>% select(incar_rate, fips)


## ---------------------------------------------------------------------------------------------------------------------------------------------
merge12 = merge11 %>% inner_join(incar_final, by = "fips")


## ---------------------------------------------------------------------------------------------------------------------------------------------
write.csv(merge12, file = "../data/clean/merge12incar.csv", row.names = FALSE)

