setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("utils/utils_functions.R")

# Dataset generate artifically
dataset <- read.csv("./datasets/dataset_for_variance_etc.csv")
dataset

dataset_size <- nrow(dataset)
dataset_size

income_ordered <- dataset[order(dataset$Income),]$Income
income_ordered

arith_mean_income <- arithmetic_mean(income_ordered)
arith_mean_income

income_median <- median(income_ordered)
income_median

# Mean: 48983.5 - Median: 48902.25 - No outliers into the dataset

# QUANTILE --------------------------------------------
income_ordered_size <- length(income_ordered)
second_quantile <- income_median
first_quantile <- income_ordered[(income_ordered_size+1)*0.25]
third_quantile <- income_ordered[(income_ordered_size+1)*0.75]

sprintf('First quantile: %0.2f - second quantile: %0.2f - third quantile: %0.2f', first_quantile, second_quantile, third_quantile)

# --------------------------------------------------------------
# BOX PLOT --------------------------------------------

boxplot(income_ordered)

# --------------------------------------------------------------
# VARIANCE - STD (standard deviation)  --------------------------------------------
income_variance <- variance(income_ordered, arith_mean_income)

standard_deviation <- sqrt(income_variance)
sprintf("Variance of income: %0.2f - Standard deviantion: %0.2f", income_variance, standard_deviation)
# --------------------------------------------------------------