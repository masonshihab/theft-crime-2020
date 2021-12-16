# run all steps of the analysis pipeline


#setwd("C:/Users/Mason Shihab/Documents/MBDS/471/final-project-template/code")
setwd("/Users/diyangchu/Documents/2-grad@Penn/STAT471/theft-crime-2020/code")
source("1-cleaning.R")
source("2.1-imputation.R")
source("2.2-cleaning2.R")
source("2.3-train-test-split.R")
source("2-exploration.R")
#source("4-regression-modeling.R")
#source("5-tree-modeling.R")
#source("6-model-evaluation.R")

