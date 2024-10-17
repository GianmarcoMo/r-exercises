x <- c(0:3)
n_trials <- 3
probability_for_trial <- 0.5

dist_binom_x <- dbinom(x, size = n_trials, prob = probability_for_trial); dist_binom_x
barplot(dist_binom_x, names.arg = 0:3)

prob_binom_x <- pbinom(x, size = n_trials, prob = probability_for_trial)

x <- c(1,1,1,2,2,3,5,5)
ecdf_x <- ecdf(x); ecdf_x
plot(ecdf_x, main= "Empirical cumulative distribution function")
