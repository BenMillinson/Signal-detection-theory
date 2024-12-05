# SDT and Generalized Linear Models: Sensitivity, Probability of Correct Response, and Regression -----

library(lme4)

# Simulate binary data for Hits (1's) and False Alarms (0's) ------------------------

set.seed(123)  # For reproducibility

# Simulate binary data for trials
# 1 = Hit, 0 = False Alarm
# This represents the outcome of trials for both signal and noise conditions
hit_data <- c(rep(1, 40), rep(0, 10))  # 50 trials (40 Hits, 10 False Alarms)
false_alarm_data <- c(rep(1, 10), rep(0, 40))  # 50 trials (10 False Alarms, 40 Correct Rejections)

# Calculate Hit Rate (p_hit) and False Alarm Rate (p_FA)
p_hit <- mean(hit_data)  # Proportion of Hits
p_FA <- mean(false_alarm_data)  # Proportion of False Alarms

# Perform the log-log transformation for the Hit probability
log_log_hit <- -log(-log(p_hit))  # Transformation for Hits

# Perform the log-log transformation for the False Alarm probability
log_log_false_alarm <- -log(-log(p_FA))  # Transformation for False Alarms

# Calculate sensitivity (d) using the log-log transformation
d <- log_log_hit - log_log_false_alarm  # Sensitivity calculation

# Display the results
cat("Log-log transformation of Hit probability:", log_log_hit, "\n")
cat("Log-log transformation of False Alarm probability:", log_log_false_alarm, "\n")
cat("Calculated Sensitivity (d):", d, "\n")

# Calculate the decision criterion (c) dynamically based on Hit and False Alarm Rates
# In SDT, the decision criterion c can be calculated as:
# c = -1/2 * (Phi^(-1)(Hit Rate) + Phi^(-1)(False Alarm Rate))
c <- -0.5 * (qnorm(p_hit) + qnorm(p_FA))  # qnorm() gives the inverse of the normal CDF

# Display the calculated criterion (c)
cat("Calculated Decision Criterion (c):", c, "\n")

# Calculate the probability of a correct response (p_correct)
# Formula: p_correct = pnorm(d / 2 - c), where c is the decision criterion
p_correct <- pnorm(d / 2 - c)  # Probability of correct response

# Display the probability of correct response
cat("Probability of a Correct Response (p_correct):", p_correct, "\n")

# Adding Regression to Predict Sensitivity (d) -------------------------------

# Simulate predictors (e.g., cognitive load, stimulus contrast, etc.)
set.seed(123)  # For reproducibility
n <- 100  # Number of observations

# Predictor 1: Cognitive Load (e.g., low to high)
cognitive_load <- runif(n, 1, 10)

# Predictor 2: Stimulus Contrast (e.g., weak to strong)
stimulus_contrast <- runif(n, 1, 5)

# Simulate decision criterion (c) for each observation
criterion <- runif(n, -1, 1)  # Random criterion values (could be adjusted for your data)

# Simulate sensitivity (d) based on predictors
# Using a simple linear model: d = 0.4 * cognitive_load + 0.7 * stimulus_contrast + noise
sensitivity <- 0.4 * cognitive_load + 0.7 * stimulus_contrast + rnorm(n, mean = 0, sd = 0.5)

# Combine data into a data frame
data <- data.frame(sensitivity, cognitive_load, stimulus_contrast, criterion)

# Fit a regression model: Predict sensitivity (d) using the predictors
model_sensitivity <- lm(sensitivity ~ cognitive_load + stimulus_contrast + criterion, data = data)

# Summarize the regression results
cat("\nRegression Results for d' (Sensitivity):\n")
summary(model_sensitivity)

#for criterion: 
#This is the estimated change in sensitivity (d') for each 1-unit increase in the decision criterion (c)

# multilevel models -------------------------------------------------------


# Simulate a 'subject' factor to account for subjects in the dataset
data$subject <- factor(rep(1:20, each = 5))  # 20 subjects, 5 observations each

# Fit a multilevel model to predict sensitivity ('d') using the predictors
# including random intercepts for subjects
multilevel_model <- lmer(sensitivity ~ cognitive_load + stimulus_contrast + criterion + (1 | subject), data = data)

# Summarize the multilevel model
cat("\nMultilevel Model Results:\n")
summary(multilevel_model)

