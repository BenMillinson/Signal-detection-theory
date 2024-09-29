# This R script covers how to calculate d prime values from false alarms and hit rates
# This covers a 'normal/simplistic' way of calculating dprimes,
# other ways of calculating audio-visual dprimes from audio and visual measures,
# which were taken from Micheyl and Oxenham (2012):
# https://doi.org/10.1121/1.3699231
# These equations are also visualised

# where m is the number of independent measurements or responses

#load packages
library(lattice)
library(RColorBrewer)
library(ggplot2)
library(dplyr)



# calculating dprime ------------------------------------------------------

# Function to calculate hit rate and false alarm rate based on counts
calculate_rates <- function(hits, total_signal_present, false_alarms, total_signal_absent) {
  # Calculate hit rate: proportion of hits over total signal-present trials
  hit_rate <- hits / total_signal_present
  
  # Calculate false alarm rate: proportion of false alarms over total signal-absent trials
  false_alarm_rate <- false_alarms / total_signal_absent
  
  # Ensure hit_rate and false_alarm_rate are within valid range (0,1)
  hit_rate <- pmin(pmax(hit_rate, 1e-5), 1 - 1e-5)  # To avoid Z-scores at infinity
  false_alarm_rate <- pmin(pmax(false_alarm_rate, 1e-5), 1 - 1e-5)
  
  # Return hit rate and false alarm rate as a list
  return(list(hit_rate = hit_rate, false_alarm_rate = false_alarm_rate))
}

# Function to calculate d' (d-prime) using the hit rate and false alarm rate
calculate_d_prime <- function(hits, total_signal_present, false_alarms, total_signal_absent) {
  # Calculate hit rate and false alarm rate
  rates <- calculate_rates(hits, total_signal_present, false_alarms, total_signal_absent)
  
  # Extract hit rate and false alarm rate
  hit_rate <- rates$hit_rate
  false_alarm_rate <- rates$false_alarm_rate
  
  # Calculate Z-scores for hit rate and false alarm rate using the inverse CDF (qnorm)
  z_hit_rate <- qnorm(hit_rate)
  z_false_alarm_rate <- qnorm(false_alarm_rate)
  
  # Calculate d' as the difference between Z(hit rate) and Z(false alarm rate)
  d_prime <- z_hit_rate - z_false_alarm_rate
  
  # Return d' and the rates as a list
  return(list(d_prime = d_prime, hit_rate = hit_rate, false_alarm_rate = false_alarm_rate))
}

# Example parameters
hits <- 85  # Number of hits (signal present, correct response)
total_signal_present <- 100  # Total signal-present trials
false_alarms <- 10  # Number of false alarms (signal absent, incorrect response)
total_signal_absent <- 100  # Total signal-absent trials

# Calculate d' and rates
result <- calculate_d_prime(hits, total_signal_present, false_alarms, total_signal_absent)

# Extract hit rate, false alarm rate, and d'
hit_rate <- result$hit_rate
false_alarm_rate <- result$false_alarm_rate
d_prime <- result$d_prime
#the d_prime value
d_prime

# Create the distributions for visualization
z_values <- seq(-3, 3, length.out = 1000)
signal_distribution <- dnorm(z_values, mean = qnorm(hit_rate), sd = 1)  # Signal distribution
noise_distribution <- dnorm(z_values, mean = qnorm(false_alarm_rate), sd = 1)  # Noise distribution

# Combine the data into a data frame for ggplot
df <- data.frame(z = z_values, 
                 signal = signal_distribution, 
                 noise = noise_distribution)

# Create the plot
ggplot(df, aes(x = z)) +
  geom_line(aes(y = signal), color = "blue", size = 1, alpha = 0.7) +
  geom_line(aes(y = noise), color = "red", size = 1, alpha = 0.7) +
  geom_area(data = subset(df, z > (qnorm(false_alarm_rate) + 0.01) & z < qnorm(hit_rate)),
            aes(y = signal), fill = "lightblue", alpha = 0.5) +
  geom_area(data = subset(df, z > (qnorm(false_alarm_rate) - 0.01) & z < qnorm(false_alarm_rate)),
            aes(y = noise), fill = "lightcoral", alpha = 0.5) +
  geom_vline(xintercept = qnorm(hit_rate), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = qnorm(false_alarm_rate), linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = qnorm(hit_rate), y = 0.01, label = "Hit Rate (dashed line)", color = "blue", vjust = -1) +
  annotate("text", x = qnorm(false_alarm_rate), y = 0.01, label = "False Alarm Rate (dashed line)", color = "red", vjust = -1) +
  annotate("text", x = (qnorm(hit_rate) + qnorm(false_alarm_rate)) / 2, 
           y = 0.15, 
           label = paste("d' =", round(d_prime, 3)), 
           size = 5, 
           color = "black") +  # Adding d' value to the plot
  labs(title = "Signal Detection Theory: Hit Rate and False Alarm Rate",
       x = "Z-score",
       y = "Density") +
  theme_minimal()


# adding the criterion to dprime ------------------------------------------


# Function to calculate hit rate and false alarm rate based on counts
calculate_rates <- function(hits, total_signal_present, false_alarms, total_signal_absent) {
  # Calculate hit rate: proportion of hits over total signal-present trials
  hit_rate <- hits / total_signal_present
  
  # Calculate false alarm rate: proportion of false alarms over total signal-absent trials
  false_alarm_rate <- false_alarms / total_signal_absent
  
  # Ensure hit_rate and false_alarm_rate are within valid range (0,1)
  hit_rate <- pmin(pmax(hit_rate, 1e-5), 1 - 1e-5)  # To avoid Z-scores at infinity
  false_alarm_rate <- pmin(pmax(false_alarm_rate, 1e-5), 1 - 1e-5)
  
  # Return hit rate and false alarm rate as a list
  return(list(hit_rate = hit_rate, false_alarm_rate = false_alarm_rate))
}

# Function to calculate d' (d-prime) and criterion (c) using the hit rate and false alarm rate
calculate_d_prime_and_c <- function(hits, total_signal_present, false_alarms, total_signal_absent) {
  # Calculate hit rate and false alarm rate
  rates <- calculate_rates(hits, total_signal_present, false_alarms, total_signal_absent)
  
  # Extract hit rate and false alarm rate
  hit_rate <- rates$hit_rate
  false_alarm_rate <- rates$false_alarm_rate
  
  # Calculate Z-scores for hit rate and false alarm rate using the inverse CDF (qnorm)
  z_hit_rate <- qnorm(hit_rate)
  z_false_alarm_rate <- qnorm(false_alarm_rate)
  
  # Calculate d' as the difference between Z(hit rate) and Z(false alarm rate)
  d_prime <- z_hit_rate - z_false_alarm_rate
  
  # Calculate the criterion (c)
  c <- - (z_hit_rate + z_false_alarm_rate) / 2
  
  # Return d', c, and the rates as a list
  return(list(d_prime = d_prime, c = c, hit_rate = hit_rate, false_alarm_rate = false_alarm_rate))
}

# Example parameters
hits <- 85  # Number of hits (signal present, correct response)
total_signal_present <- 100  # Total signal-present trials
false_alarms <- 10  # Number of false alarms (signal absent, incorrect response)
total_signal_absent <- 100  # Total signal-absent trials

# Calculate d', c, and rates
result <- calculate_d_prime_and_c(hits, total_signal_present, false_alarms, total_signal_absent)

# Extract hit rate, false alarm rate, d', and c
hit_rate <- result$hit_rate
false_alarm_rate <- result$false_alarm_rate
d_prime <- result$d_prime
c <- result$c

# Create the distributions for visualization
z_values <- seq(-3, 3, length.out = 1000)
signal_distribution <- dnorm(z_values, mean = qnorm(hit_rate), sd = 1)  # Signal distribution
noise_distribution <- dnorm(z_values, mean = qnorm(false_alarm_rate), sd = 1)  # Noise distribution

# Combine the data into a data frame for ggplot
df <- data.frame(z = z_values, 
                 signal = signal_distribution, 
                 noise = noise_distribution)

# Create the plot
ggplot(df, aes(x = z)) +
  geom_line(aes(y = signal), color = "blue", size = 1, alpha = 0.7) +
  geom_line(aes(y = noise), color = "red", size = 1, alpha = 0.7) +
  geom_area(data = subset(df, z > (qnorm(false_alarm_rate) + 0.01) & z < qnorm(hit_rate)),
            aes(y = signal), fill = "lightblue", alpha = 0.5) +
  geom_area(data = subset(df, z > (qnorm(false_alarm_rate) - 0.01) & z < qnorm(false_alarm_rate)),
            aes(y = noise), fill = "lightcoral", alpha = 0.5) +
  geom_vline(xintercept = qnorm(hit_rate), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = qnorm(false_alarm_rate), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = c, linetype = "solid", color = "purple", size = 1) +  # Criterion line
  annotate("text", x = qnorm(hit_rate), y = 0.01, label = "Hit Rate (dashed line)", color = "blue", vjust = -1) +
  annotate("text", x = qnorm(false_alarm_rate), y = 0.01, label = "False Alarm Rate (dashed line)", color = "red", vjust = -1) +
  annotate("text", x = c, y = 0.05, label = paste("Criterion (c) =", round(c, 3)), color = "purple", vjust = -1) +  # Adding c value to the plot
  annotate("text", x = (qnorm(hit_rate) + qnorm(false_alarm_rate)) / 2, 
           y = 0.15, 
           label = paste("d' =", round(d_prime, 3)), 
           size = 5, 
           color = "black") +  # Adding d' value to the plot
  labs(title = "Signal Detection Theory: Hit Rate, False Alarm Rate, d', and Criterion (c)",
       x = "Z-score",
       y = "Density") +
  theme_minimal()




# proportion correct from dprime ------------------------------------------


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

# Example usage of the function
d_prime <- 1  # Example d' value
M <- 2        # Example M value

# Call the function with the desired d' and M
dPrime2PropCorrect(d_prime, M)








# proportion correct from d_a/v (independent model) ----------------------


#visualising the early noise equation:


# Create data for visualization
# Example data
d_a_prime <- seq(1, 10, by = 0.4) # Values for d_a'
d_v_prime <- seq(1, 10, by = 0.4) # Values for d_v'
data <- expand.grid(d_a_prime = d_a_prime, d_v_prime = d_v_prime)
data$d_av_prime <- with(data, sqrt(d_a_prime + d_v_prime)) # Calculate d_av'

# Add random noise to d_av_prime
set.seed(123) # For reproducibility
noise <- rnorm(nrow(data), mean = 0, sd = 1) # Generate random noise
data$d_av_prime_noisy <- data$d_av_prime + noise # Add noise to d_av_prime

# Define a custom color gradient from white to blue
color_palette <- colorRampPalette(c("white", "blue")) # Custom white-to-blue gradient

# Create the 3D plot using lattice with noisy d_av_prime values
cloud(d_av_prime_noisy ~ d_a_prime * d_v_prime,
      data = data,
      xlab = "d'a",
      ylab = "d'v",
      zlab = "d'av",
      main = "",
      col = color_palette(100)[cut(data$d_av_prime_noisy, breaks = 100)], # Apply color gradient to noisy values
      pch = 16)



# Define the function to calculate P(independent) using a sum approximation
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

#example usage of the equation: 
da_prime <- 2 # Example da' value
dv_prime <- 1 # Example dv' value
M <- 5        # Example M value
P_independent(da_prime,dv_prime, M)



# proportion correct from d_a/v (late model) -----------------------


#visualising late noise:

# Create data for visualization
# Example data
d_a_prime <- seq(1, 10, by = 0.4) # Values for d_a'
d_v_prime <- seq(1, 10, by = 0.4) # Values for d_v'
data <- expand.grid(d_a_prime = d_a_prime, d_v_prime = d_v_prime)
data$d_av_prime <- with(data, d_a_prime + d_v_prime) # Calculate d_av'

# Add random noise to d_av_prime
set.seed(123) # For reproducibility
noise <- rnorm(nrow(data), mean = 0, sd = 1) # Generate random noise
data$d_av_prime_noisy <- data$d_av_prime + noise # Add noise to d_av_prime

# Define a custom color gradient from white to blue
color_palette <- colorRampPalette(c("white", "blue")) # Custom white-to-blue gradient

# Create the 3D plot using lattice with noisy d_av_prime values
cloud(d_av_prime_noisy ~ d_a_prime * d_v_prime,
      data = data,
      xlab = "d'a",
      ylab = "d'v",
      zlab = "d'av",
      main = "",
      col = color_palette(100)[cut(data$d_av_prime_noisy, breaks = 100)], # Apply color gradient to noisy values
      pch = 16)




# Define the function to calculate P(late) using a sum approximation
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

#example usage of the equation: 
da_prime <- 2 # Example da' value
dv_prime <- 1 # Example dv' value
M <- 5        # Example M value
P_late(da_prime,dv_prime, M)
