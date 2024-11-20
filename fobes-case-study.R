forbes <- read.csv("datasets/forbes.csv", header = TRUE, sep = ";")

colnames(forbes) <- c("TempF", "Pressure")
forbes$TempC <- round((forbes$TempF - 32) * 5 / 9, 2)

summary(forbes)

# ---------
# 1. Draw histogram of the temperature variable.
ds_size <- length(forbes$TempC)

bins_size <- round(1 + log(ds_size, 2), 0) # Sturges formula
bins_size

hist(forbes$TempC,
     main="Celsius temperature",
     xlab="Temperature in Celsius",
     breaks=bins_size
)

hist(forbes$TempF,
     main="Fahrenheit temperature",
     xlab="Temperature in Fahrenheit",
     breaks=bins_size
)

# 2. Compute average of temperatureC, temperaturF and pressure
avg_tempC <- sum(forbes$TempC) / ds_size
avg_tempF <- sum(forbes$TempF) / ds_size
avg_pressure <- sum(forbes$Pressure) / ds_size

# 3. Compute variance fo tempC and Pressure
variance <- function(observations, mean){
  var_sum <- sum((observations-mean)**2)
  variance <- var_sum / length(observations)
  return(variance)
}

variance(forbes$TempC, avg_tempC)
variance(forbes$Pressure, avg_pressure)

# 4. Draw scatter plot to see relationship between 2 variables
plot(forbes$TempC, forbes$Pressure,
     main = "Temperature vs Pressure",
     xlab = "C° temperature",
     ylab = "Pressure"
)

# 5. Compute covariance and correlation
covariance <- function(x, y){
  x_mean <- mean(x)
  y_mean <- mean(y)
  cov_sum <- sum((x-x_mean) * (y-y_mean))
  cov <- cov_sum / length(x)
  
  return(cov)
}

correlation <- function(x, y){
  x_mean <- mean(x)
  y_mean <- mean(y)
  x_std <- sd(x)
  y_std <- sd(y)
  
  corr_sum <- sum(((x - x_mean)/x_std) * ((y - y_mean)/y_std))
  corr <- corr_sum / length(x)
  return(corr)
}

cov_temp_press <- covariance(forbes$TempC, forbes$Pressure)
corr_temp_press <- correlation(forbes$TempC, forbes$Pressure)a
cov_temp_press # 9.06 positive relationship
corr_temp_press # 0.938 strong positive relationship

# 6. Compute Least square regression line between tempC and preassure (plot it)
model <- lm(Pressure ~ TempC, data = forbes)
summary_model <- summary(model)

plot(forbes$TempC, forbes$Pressure,
     main = "Least Squares Regression", 
     xlab = "Temperature (F)", 
     ylab = "Pressure",
     pch = 16)
abline(model, col = "blue")

# 7. Given the model, what is the pressure for water that boilling at 97° C?
tempC_input <- data.frame(TempC = 97)
predicted_pressure <- predict(model, newdata = tempC_input)
predicted_pressure # 26.96674

# 8. Compute the goodness of the model
r_squared <- summary_model$r.squared
r_squared
