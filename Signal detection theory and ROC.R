# This R script shows how signal detection theory can easily be implemented into ROC,
# and how these models can undergo hypothesis testing.
# models are better explained/visualised (and code provided) in the 'signal detection theory,
# equations' R script. Model codes are still provided here though

library(pROC)
library(ggplot2)


# example ROC -------------------------------------------------------------


set.seed(123)
signal_data <- rnorm(n = 100, mean = 2, sd = 1)  # Adjust mean and sd based on your experiment
noise_data <- rnorm(n = 100, mean = 1, sd = 1)   # Adjust mean and sd based on your experiment

# Combine data and labels
labels <- c(rep(1, length(signal_data)), rep(0, length(noise_data)))
scores <- c(signal_data, noise_data)

# Create ROC curve
# Explicitly specify levels and direction
roc_curve <- roc(labels, scores, levels = c(1, 0), direction = ">")


# Plot ROC curve (think this one is redundant)
plot(roc_curve, col = "blue", main = "ROC Curve",
     col.main = "black", col.axis = "black", col.lab = "black")

# Calculate AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Open a new plotting device with adjusted parameters
# You can adjust width and height based on your preference
png("roc_plot.png", width = 800, height = 600)

# Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve",
     col.main = "black", col.axis = "black", col.lab = "black")

# Close the plotting device
dev.off()

#plotting this with bias

# Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve",
     col.main = "black", col.axis = "black", col.lab = "black")

# Specify a value of c for a bias point
c_value <- 5  # Adjust the value based on your preference

# Calculate sensitivity and specificity at the specified c value
sensitivity_at_c <- roc_curve_radar$sensitivities[which.min(abs(roc_curve_radar$thresholds - c_value))]
specificity_at_c <- 1 - roc_curve_radar$specificities[which.min(abs(roc_curve_radar$thresholds - c_value))]

# Plot the bias point
points(1 - specificity_at_c, sensitivity_at_c, col = "red", pch = 16)

# Add label for the bias point
text(1 - specificity_at_c, sensitivity_at_c,
     labels = paste("Custom Bias Point (c =", round(c_value, 2), ")", sep = ""), pos = 4, col = "purple")


#another example ----


signal_data_radar <- rnorm(n = 100, mean = 20, sd = 3)  # Adjust mean and sd based on your experiment
noise_data_radar <- rnorm(n = 100, mean = 5, sd = 2)    # Adjust mean and sd based on your experiment

# Combine data and labels
labels_radar <- c(rep(1, length(signal_data_radar)), rep(0, length(noise_data_radar)))
scores_radar <- c(signal_data_radar, noise_data_radar)

# Create ROC curve
roc_curve_radar <- roc(labels_radar, scores_radar)

# Plot ROC curve
plot(roc_curve_radar, col = "blue", main = "ROC Curve for Radar Systems",
     col.main = "black", col.axis = "black", col.lab = "black")

# Calculate AUC
auc_value_radar <- auc(roc_curve_radar)
cat("AUC:", auc_value_radar, "\n")

#adding bias to plots----
#so adding criterion (c)

# Plot ROC curve
plot(roc_curve_radar, col = "blue", main = "ROC Curve for Radar Systems",
     col.main = "black", col.axis = "black", col.lab = "black")

# Calculate and plot a few points along the ROC curve with different bias levels
points(0.2, 0.8, col = "red", pch = 16)  # Example bias point 1
points(0.5, 0.5, col = "green", pch = 16)  # Example bias point 2
points(0.8, 0.2, col = "orange", pch = 16)  # Example bias point 3

# Add labels to the points
text(0.2, 0.8, "Bias Point 1", pos = 4, col = "red")
text(0.5, 0.5, "Bias Point 2", pos = 4, col = "green")
text(0.8, 0.2, "Bias Point 3", pos = 4, col = "orange")


#another example of bias plotting----

# Plot ROC curve
plot(roc_curve_radar, col = "blue", main = "ROC Curve for Radar Systems",
     col.main = "black", col.axis = "black", col.lab = "black")

# Specify a value of c for a bias point
c_value <- 0.3  # Adjust the value based on your preference

# Calculate sensitivity and specificity at the specified c value
sensitivity_at_c <- roc_curve_radar$sensitivities[which.min(abs(roc_curve_radar$thresholds - c_value))]
specificity_at_c <- 1 - roc_curve_radar$specificities[which.min(abs(roc_curve_radar$thresholds - c_value))]

# Plot the bias point
points(1 - specificity_at_c, sensitivity_at_c, col = "red", pch = 16)

# Add label for the bias point
text(1 - specificity_at_c, sensitivity_at_c,
     labels = paste("Custom Bias Point (c =", round(c_value, 2), ")", sep = ""), pos = 4, col = "purple")

#AUC model comparison----
roc.test(roc_curve, roc_curve_radar)

# visualising ROC comparison----

# Plot ROC curves
plot(roc_curve, col = "blue", main = "ROC Curves Comparison")
plot(roc_curve_radar, col = "red", add = TRUE)
# Add legend with colors
legend("bottomright", legend = c("roc_curve", "roc_curve_radar"), fill = c("blue", "red"))


#hypothesis testing using delongs test----

# Generate synthetic data
n <- 1000
true_labels <- sample(0:1, n, replace = TRUE)

# Model 1: Generate predicted probabilities with ROC curve AUC = 0.7
predicted_probs_model1 <- rnorm(n, ifelse(true_labels == 1, 0.8, 0.2), 0.2)
roc_model1 <- roc(true_labels, predicted_probs_model1)

# Model 2: Generate predicted probabilities with ROC curve AUC = 0.8
predicted_probs_model2 <- rnorm(n, ifelse(true_labels == 1, 0.9, 0.3), 0.15)
roc_model2 <- roc(true_labels, predicted_probs_model2)

# Perform DeLong test
delong_test_result <- roc.test(roc_model1, roc_model2, method = "delong")

# Print the result
print(delong_test_result)


# 'normal' SDT equation and ROC -------------------------------------------

# Function to compute the hit rate (H) and false alarm rate (FA) for an ROC curve
Prob_from_dp <- function(dpValue, m) {
  # Set a small step size for the discrete approximation
  dz <- 1e-4
  
  # Generate a sequence of z values from -15 to 15, with increments of dz
  zvals <- seq(-15, 15, by = dz)
  
  # Calculate the probability using the sum of the function's values
  pc <- dz * sum(dnorm(zvals - dpValue) * pnorm(zvals)^(m - 1))
  
  # Return the calculated probability
  return(pc)
}

# Function to calculate hit rate (H) and false alarm rate (FA) for different thresholds
roc_curve_points <- function(dpValue, m) {
  thresholds <- seq(-5, 5, by = 0.1)  # Thresholds for decision criteria
  hit_rates <- numeric(length(thresholds))
  false_alarm_rates <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    # For the current threshold, calculate the hit rate and false alarm rate
    threshold <- thresholds[i]
    
    # Hit rate (H): Probability of correctly detecting a signal given d_prime
    hit_rates[i] <- 1 - pnorm(threshold - dpValue)
    
    # False alarm rate (FA): Probability of incorrectly detecting a signal (no signal present)
    false_alarm_rates[i] <- 1 - pnorm(threshold)
  }
  
  # Return data frame with thresholds, hit rates, and false alarm rates
  return(data.frame(thresholds = thresholds, hit_rate = hit_rates, false_alarm_rate = false_alarm_rates))
}

# Function to calculate AUC using the trapezoidal rule, ensuring non-negative AUC values
calculate_auc <- function(hit_rates, false_alarm_rates) {
  # Sort the false_alarm_rates and hit_rates to ensure proper order for AUC calculation
  sorted_indices <- order(false_alarm_rates)
  false_alarm_rates <- false_alarm_rates[sorted_indices]
  hit_rates <- hit_rates[sorted_indices]
  
  # Use trapezoidal rule: AUC = sum of areas of small trapezoids
  auc <- sum(diff(false_alarm_rates) * (hit_rates[-1] + hit_rates[-length(hit_rates)]) / 2)
  
  return(auc)
}

# Set parameters
d_prime_values <- c(0.5, 1.0, 1.5, 2.0)  # Different d' values to plot ROC curves for
m_value <- 2  # Number of alternatives

# Create an empty data frame to store ROC data for different d' values
roc_data <- data.frame()
auc_values <- data.frame(d_prime = numeric(), AUC = numeric())  # To store AUC for each d'

# Loop over each d' value and generate ROC data
for (dpValue in d_prime_values) {
  roc_points <- roc_curve_points(dpValue, m_value)
  roc_points$d_prime <- dpValue  # Add d_prime as a column
  
  # Calculate AUC for the current ROC curve
  auc <- calculate_auc(roc_points$hit_rate, roc_points$false_alarm_rate)
  auc_values <- rbind(auc_values, data.frame(d_prime = dpValue, AUC = auc))  # Store AUC
  
  # Append ROC points to the overall data frame
  roc_data <- rbind(roc_data, roc_points)
}

# Plot the ROC curves
ggplot(roc_data, aes(x = false_alarm_rate, y = hit_rate, color = as.factor(d_prime), group = d_prime)) +
  geom_line(size = 1.2) +
  labs(title = "ROC Curves for Different d' Values",
       x = "False Alarm Rate (FA)",
       y = "Hit Rate (H)",
       color = "d' Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_equal() +  # Ensures the plot is square
  geom_text(data = auc_values, aes(x = 0.7, y = 0.3, label = paste("AUC:", round(AUC, 2))), 
            inherit.aes = FALSE, hjust = 0)

# Print the AUC values
print(auc_values)



# independent model and ROC -----------------------------------------------


# Function to compute the probability for the independent model
P_independent <- function(da_prime, dv_prime, m) {
  # Set a small step size for the discrete approximation
  dz <- 1e-4
  
  # Generate a sequence of z values from -15 to 15, with increments of dz
  zvals <- seq(-15, 15, by = dz)
  
  # Calculate the square root of (da_prime^2 + dv_prime^2)
  delta_prime <- sqrt(da_prime^2 + dv_prime^2)
  
  # Calculate the probability using the sum of the function's values
  pc <- dz * sum(dnorm(zvals - delta_prime) * pnorm(zvals)^(m - 1))
  
  # Return the calculated probability
  return(pc)
}

# Function to calculate hit rate (H) and false alarm rate (FA) for different thresholds
roc_curve_points_independent <- function(da_prime, dv_prime, m) {
  thresholds <- seq(-5, 5, by = 0.1)  # Thresholds for decision criteria
  hit_rates <- numeric(length(thresholds))
  false_alarm_rates <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    # For the current threshold, calculate the hit rate and false alarm rate
    threshold <- thresholds[i]
    
    # Calculate the delta_prime
    delta_prime <- sqrt(da_prime^2 + dv_prime^2)
    
    # Hit rate (H): Probability of correctly detecting a signal given delta_prime
    hit_rates[i] <- 1 - pnorm(threshold - delta_prime)
    
    # False alarm rate (FA): Probability of incorrectly detecting a signal (no signal present)
    false_alarm_rates[i] <- 1 - pnorm(threshold)
  }
  
  # Return data frame with thresholds, hit rates, and false alarm rates
  return(data.frame(thresholds = thresholds, hit_rate = hit_rates, false_alarm_rate = false_alarm_rates))
}

# Function to calculate AUC using the trapezoidal rule, ensuring non-negative AUC values
calculate_auc <- function(hit_rates, false_alarm_rates) {
  # Sort the false_alarm_rates and hit_rates to ensure proper order for AUC calculation
  sorted_indices <- order(false_alarm_rates)
  false_alarm_rates <- false_alarm_rates[sorted_indices]
  hit_rates <- hit_rates[sorted_indices]
  
  # Use trapezoidal rule: AUC = sum of areas of small trapezoids
  auc <- sum(diff(false_alarm_rates) * (hit_rates[-1] + hit_rates[-length(hit_rates)]) / 2)
  
  return(auc)
}

# Set parameters
da_prime_values <- c(0.5, 1.0, 1.5, 2.0)  # Different da_prime values to plot ROC curves for
dv_prime_values <- c(0.5, 1.0, 1.5, 2.0)  # Different dv_prime values to vary
m_value <- 2  # Number of alternatives

# Create an empty data frame to store ROC data for different da_prime and dv_prime values
roc_data <- data.frame()
auc_values <- data.frame(da_prime = numeric(), dv_prime = numeric(), AUC = numeric())  # To store AUC for each combination

# Loop over each combination of da_prime and dv_prime values and generate ROC data
for (da_prime in da_prime_values) {
  for (dv_prime in dv_prime_values) {
    roc_points <- roc_curve_points_independent(da_prime, dv_prime, m_value)
    roc_points$da_prime <- da_prime  # Add da_prime as a column
    roc_points$dv_prime <- dv_prime  # Add dv_prime as a column
    
    # Calculate AUC for the current ROC curve
    auc <- calculate_auc(roc_points$hit_rate, roc_points$false_alarm_rate)
    auc_values <- rbind(auc_values, data.frame(da_prime = da_prime, dv_prime = dv_prime, AUC = auc))  # Store AUC
    
    # Append ROC points to the overall data frame
    roc_data <- rbind(roc_data, roc_points)
  }
}

# Plot the ROC curves
ggplot(roc_data, aes(x = false_alarm_rate, y = hit_rate, color = interaction(da_prime, dv_prime), group = interaction(da_prime, dv_prime))) +
  geom_line(size = 1.2) +
  labs(title = "ROC Curves for Different da' and dv' Values",
       x = "False Alarm Rate (FA)",
       y = "Hit Rate (H)",
       color = "da', dv' Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_equal() +  # Ensures the plot is square
  geom_text(data = auc_values, aes(x = 0.7, y = 0.3, label = paste("AUC:", round(AUC, 2))), 
            inherit.aes = FALSE, hjust = 0)

# Print the AUC values
print(auc_values)




# late model and ROC ------------------------------------------------------


# Function to compute the probability for the late model
P_late <- function(da_prime, dv_prime, m) {
  # Set a small step size for the discrete approximation
  dz <- 1e-4
  
  # Generate a sequence of z values from -15 to 15, with increments of dz
  zvals <- seq(-15, 15, by = dz)
  
  # Calculate the sum (da_prime + dv_prime)
  delta_prime <- da_prime + dv_prime
  
  # Calculate the probability using the sum of the function's values
  pc <- dz * sum(dnorm(zvals - delta_prime) * pnorm(zvals)^(m - 1))
  
  # Return the calculated probability
  return(pc)
}

# Function to calculate hit rate (H) and false alarm rate (FA) for different thresholds
roc_curve_points_late <- function(da_prime, dv_prime, m) {
  thresholds <- seq(-5, 5, by = 0.1)  # Thresholds for decision criteria
  hit_rates <- numeric(length(thresholds))
  false_alarm_rates <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    # For the current threshold, calculate the hit rate and false alarm rate
    threshold <- thresholds[i]
    
    # Calculate delta_prime as the sum of da_prime and dv_prime
    delta_prime <- da_prime + dv_prime
    
    # Hit rate (H): Probability of correctly detecting a signal given delta_prime
    hit_rates[i] <- 1 - pnorm(threshold - delta_prime)
    
    # False alarm rate (FA): Probability of incorrectly detecting a signal (no signal present)
    false_alarm_rates[i] <- 1 - pnorm(threshold)
  }
  
  # Return data frame with thresholds, hit rates, and false alarm rates
  return(data.frame(thresholds = thresholds, hit_rate = hit_rates, false_alarm_rate = false_alarm_rates))
}

# Function to calculate AUC using the trapezoidal rule, ensuring non-negative AUC values
calculate_auc <- function(hit_rates, false_alarm_rates) {
  # Sort the false_alarm_rates and hit_rates to ensure proper order for AUC calculation
  sorted_indices <- order(false_alarm_rates)
  false_alarm_rates <- false_alarm_rates[sorted_indices]
  hit_rates <- hit_rates[sorted_indices]
  
  # Use trapezoidal rule: AUC = sum of areas of small trapezoids
  auc <- sum(diff(false_alarm_rates) * (hit_rates[-1] + hit_rates[-length(hit_rates)]) / 2)
  
  return(auc)
}

# Set parameters
da_prime_values <- c(0.5, 1.0, 1.5, 2.0)  # Different da_prime values to plot ROC curves for
dv_prime_values <- c(0.5, 1.0, 1.5, 2.0)  # Different dv_prime values to vary
m_value <- 2  # Number of alternatives

# Create an empty data frame to store ROC data for different da_prime and dv_prime values
roc_data <- data.frame()
auc_values <- data.frame(da_prime = numeric(), dv_prime = numeric(), AUC = numeric())  # To store AUC for each combination

# Loop over each combination of da_prime and dv_prime values and generate ROC data
for (da_prime in da_prime_values) {
  for (dv_prime in dv_prime_values) {
    roc_points <- roc_curve_points_late(da_prime, dv_prime, m_value)
    roc_points$da_prime <- da_prime  # Add da_prime as a column
    roc_points$dv_prime <- dv_prime  # Add dv_prime as a column
    
    # Calculate AUC for the current ROC curve
    auc <- calculate_auc(roc_points$hit_rate, roc_points$false_alarm_rate)
    auc_values <- rbind(auc_values, data.frame(da_prime = da_prime, dv_prime = dv_prime, AUC = auc))  # Store AUC
    
    # Append ROC points to the overall data frame
    roc_data <- rbind(roc_data, roc_points)
  }
}

# Plot the ROC curves
ggplot(roc_data, aes(x = false_alarm_rate, y = hit_rate, color = interaction(da_prime, dv_prime), group = interaction(da_prime, dv_prime))) +
  geom_line(size = 1.2) +
  labs(title = "ROC Curves for Different da' and dv' Values (Late Model)",
       x = "False Alarm Rate (FA)",
       y = "Hit Rate (H)",
       color = "da', dv' Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_equal() +  # Ensures the plot is square
  geom_text(data = auc_values, aes(x = 0.7, y = 0.3, label = paste("AUC:", round(AUC, 2))), 
            inherit.aes = FALSE, hjust = 0)

# Print the AUC values
print(auc_values)



