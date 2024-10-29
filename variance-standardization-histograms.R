# --------------------------------------------------------------
# Functions

median <- function(numerical_list){
  list_size <- length(numerical_list)
  
  median_index <- 0
  
  if((list_size && 2) == 0){
    median_index <- floor((numerical_list[list_size/2] + numerical_list[(list_size/2)+1])/2)
  } else {
    median_index <- floor((list_size+1)/2)
  }
  
  return(numerical_list[median_index])
}

arithmetic_mean <- function(numerical_list){
  sum <- 0
  list_size <- length(numerical_list)
  for(number in numerical_list){
    sum <- sum + number
  }
  
  return(sum/list_size)
}

variance <- function (numerical_list, average){
  numerical_list_size <- length(numerical_list)
  numerical_sum <- 0
  for (number in numerical_list){
    numerical_sum <- numerical_sum + (number - average)^2
  }
  variance <- (1/numerical_list_size) * numerical_sum
  return(variance)
}


# --------------------------------------------------------------
# --------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
# VARIANCE - STD  --------------------------------------------
income_variance <- variance(income_ordered, arith_mean_income)
# TOOD: implement STD
# --------------------------------------------------------------
