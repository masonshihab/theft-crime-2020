# run all steps of the analysis pipeline

set.seed(471)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("1-cleaning.R")
source("2.1-imputation.R")
source("2.2-cleaning2.R")
source("2.3-train-test-split.R")
source("3-exploration.R")
source("4-regression-modeling.R")
source("5-tree-modeling.R")
source("6-model-evaluation.R")
