source("utils/utils_functions.R")
source("utils/skeewed_utils.R")

# Dataset generate artifically
dataset <- read.csv("./datasets/dataset_for_variance_etc.csv")
dataset

income_ordered <- dataset[order(dataset$Income),]$Income
income_ordered

income_ordered_size <- length(income_ordered)

# HISTOGRAM --------------------------------------------
# How many intervals create?

# Sturges formula
intervals_sturges <- get_intervals_sturges(income_ordered_size)

hist(income_ordered,
     breaks=intervals_sturges,
     main="Distribution of income",
     xlab="Incomes",
     ylab="Frequency",
)
# Normal distribution - OK!

# SKEEWED DATA EXAMPLE -----------------------------------
skeewed_data <- read.csv("datasets/income_skeewed_data.csv")
skeewed_data_list <- skeewed_data$Income_Data
skeewed_data_size <- length(skeewed_data_list)
intervals_skeewed <- get_intervals_sturges(skeewed_data_size)

hist(skeewed_data_list,
     breaks = intervals_skeewed,
     main = "Distribution of Income - Skeewed",
     xlab="Incomes",
     ylab="Frequency"
)

# Pearson Skeewnes index
skeewed_data_mean <- arithmetic_mean(skeewed_data_list)
skeewed_data_median <- median(skeewed_data_list)
skeewed_data_variance <- variance(skeewed_data_list, skeewed_data_mean)
skeewed_data_std <-  standard_deviation(skeewed_data_variance)

index <- first_pearson_skeewnes_index(skeewed_data_mean, skeewed_data_median, skeewed_data_std)
index
# Pearson index is negative: -0.5483676 -> means that the distribution has a left tail

kurtosis_skeewed <- kurtosis(skeewed_data_list, skeewed_data_mean, skeewed_data_std)
kurtosis_skeewed
kurtosis_skeewed_excess <- kurtosis_skeewed - 3
kurtosis_skeewed_excess
# Kurtosis excess is greater than 0, this mean a lot of outliers and higher tail


# Normale distribution test
income_mean <- arithmetic_mean(income_ordered)
income_variance <- variance(income_ordered, income_mean)
income_std <- standard_deviation(income_variance)
income_median <- median(income_ordered)

index_pearson_normal <- first_pearson_skeewnes_index(income_mean, income_median, income_std)
index_pearson_normal
# The pearson index is lower 0.016.. 

kurtosis_normal <- kurtosis(income_ordered, income_mean, income_std)
kurtosis_normal
kurtosis_excess <- kurtosis_normal - 3
kurtosis_excess
# This time, the kurtsosi for this distribution is lower than the respect one computed before
# -0.12 -> means that there are few outliers and the tails are lighter.
# --------------------------------------------------------------
