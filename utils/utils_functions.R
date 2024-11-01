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
  list_size <- length(numerical_list)
  list_sum <- sum(numerical_list)
  mean <- list_sum/list_size
  
  return(mean)
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

standard_deviation <- function (variance){
  return(sqrt(variance))
}

get_intervals_sturges <- function (list_size) {
  size_logarithm <- log(list_size, 2)
  
  sturges_intervals <- floor(1 + size_logarithm)
  return(sturges_intervals)
}