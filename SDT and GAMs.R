#Code for general additive model predictions of d'

# Load necessary library for GAMs
library(mgcv)
library(Metrics)  # For RMSE and MAE

#Simulate a raw dataset of perceptual trials (same as original)
set.seed(123)  # For reproducibility

n_trials <- 100
audio_outcome <- sample(c(0, 1), size = n_trials, replace = TRUE, prob = c(0.4, 0.6))  # 60% correct in audio
visual_outcome <- sample(c(0, 1), size = n_trials, replace = TRUE, prob = c(0.8, 0.2))  # 20% correct in visual

audio_pitch <- rnorm(n_trials, mean = 440, sd = 50)  # Audio pitch in Hz
audio_rhythm <- rnorm(n_trials, mean = 1, sd = 0.5)  # Random rhythm feature
audio_amplitude <- rnorm(n_trials, mean = 0, sd = 1)  # Random amplitude feature
visual_intensity <- runif(n_trials, min = 0.1, max = 1)  # Light intensity
visual_duration <- runif(n_trials, min = 0.5, max = 2)  # Duration of visual light stimuli

# Combine into a dataframe
df <- data.frame(
  trial = 1:n_trials,
  audio = audio_outcome,
  visual = visual_outcome,
  audio_pitch = audio_pitch,
  audio_rhythm = audio_rhythm,
  audio_amplitude = audio_amplitude,
  visual_intensity = visual_intensity,
  visual_duration = visual_duration
)

df$d_prime <- rnorm(n_trials, mean = 1, sd = 0.5)  # Simulated d' values

# Fit a GAM model
# Use smooth terms (s()) for numerical predictors
gam_model <- gam(
  d_prime ~ s(audio_pitch) + s(audio_rhythm) + s(audio_amplitude) +
    s(visual_intensity) + s(visual_duration) + visual_color,  # visual_color treated as a categorical variable
  data = df,
  method = "REML"  # Restricted maximum likelihood for smoother estimation
)

# Summarize the model
summary(gam_model)

# Step 3: Visualize the smooth effects of predictors
# This will create individual plots for smooth terms
par(mfrow = c(2, 3))  # Arrange plots in a grid
plot(gam_model, shade = TRUE, seWithMean = TRUE, scale = 0)

# Step 4: Evaluate model performance using predictions
# Generate predictions on the training dataset
gam_predictions <- predict(gam_model, df)

# Evaluate model performance
rmse_value <- rmse(df$d_prime, gam_predictions)
mae_value <- mae(df$d_prime, gam_predictions)
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")

# Step 5: Visualize predictions vs actual values
plot(df$d_prime, gam_predictions,
     main = "GAM Predictions vs Actual d' Values",
     xlab = "Actual d' Values",
     ylab = "Predicted d' Values",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Add a line of perfect prediction
