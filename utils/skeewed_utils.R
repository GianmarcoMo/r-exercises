first_pearson_skeewnes_index <- function (mean, median, std){
  index <- (3 * (mean -  median)) / std
  return(index)
}

kurtosis <- function (list, mean, std){
  list_size <- length(list)
  kurtosis_sum <- sum(((list - mean) / std) ^ 4)
  kurtosis <- kurtosis_sum / list_size
  return(kurtosis)
}