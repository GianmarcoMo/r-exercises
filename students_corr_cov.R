students <- read.csv("datasets/students_cov_corr.csv")
students

covariance_hours_test <- cov(students$Hours_Studied, students$Test_Score)
covariance_hours_test

correlation_hours_test <- cor(students$Hours_Studied, students$Test_Score)
correlation_hours_test

covariance_sleep_test <- cov(students$Hours_Slept, students$Test_Score)
covariance_sleep_test

correlation_sleep_test <- cor(students$Hours_Slept, students$Test_Score)
correlation_sleep_test

plot(students$Test_Score, students$Hours_Studied, main="Hours studied - Test score",
     xlab="Test score ", ylab="Hours studied ", pch=19)

plot(students$Test_Score, students$Hours_Slept, main="Hours slept - Test score",
     xlab="Test score ", ylab="Hours slept ", pch=19)

# apply functions to dataframe
margin_column <- 2
apply(students, margin_column, mean)

margin_row <- 1
students$mean <- apply(students, margin_row, mean)
students
