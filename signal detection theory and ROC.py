import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
from sklearn.metrics import roc_curve, auc

# Example ROC -------------------------------------------------------------

np.random.seed(123)

# Signal and noise data generation
signal_data = np.random.normal(loc=2, scale=1, size=100)
noise_data = np.random.normal(loc=1, scale=1, size=100)

# Combine data and labels
labels = np.concatenate([np.ones(len(signal_data)), np.zeros(len(noise_data))])
scores = np.concatenate([signal_data, noise_data])

# Create ROC curve
fpr, tpr, thresholds = roc_curve(labels, scores, pos_label=1)

# Plot ROC curve
plt.figure()
plt.plot(fpr, tpr, color='blue', lw=2)
plt.plot([0, 1], [0, 1], color='gray', linestyle='--')
plt.title('ROC Curve')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.show()

# Calculate AUC
roc_auc = auc(fpr, tpr)
print(f"AUC: {roc_auc}")

# Save the plot to a file
plt.figure()
plt.plot(fpr, tpr, color='blue', lw=2)
plt.plot([0, 1], [0, 1], color='gray', linestyle='--')
plt.title('ROC Curve')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.savefig('roc_plot.png')

# Adding bias to ROC curve
c_value = 5  # Bias point value
sensitivity_at_c = tpr[np.argmin(np.abs(thresholds - c_value))]
specificity_at_c = 1 - fpr[np.argmin(np.abs(thresholds - c_value))]

# Plot bias point
plt.figure()
plt.plot(fpr, tpr, color='blue', lw=2)
plt.scatter(1 - specificity_at_c, sensitivity_at_c, color='red', label=f"Custom Bias Point (c={c_value})")
plt.text(1 - specificity_at_c, sensitivity_at_c, f"Custom Bias Point (c={round(c_value, 2)})", color='purple', ha='left')
plt.title('ROC Curve with Bias Point')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.legend()
plt.show()

# Independent and Late Noise Models -------------------------------------

# Function to compute the probability for the independent model
def P_independent(da_prime, dv_prime, m):
    dz = 1e-4  # Small step size
    zvals = np.arange(-15, 15, dz)  # Generate z-values from -15 to 15
    delta_prime = np.sqrt(da_prime**2 + dv_prime**2)  # Compute delta_prime
    pc = dz * np.sum(norm.pdf(zvals - delta_prime) * norm.cdf(zvals)**(m - 1))  # Calculate probability
    return pc

# Function to calculate hit rate (H) and false alarm rate (FA) for different thresholds (Independent Model)
def roc_curve_points_independent(da_prime, dv_prime, m):
    thresholds = np.arange(-5, 5, 0.1)  # Thresholds for decision criteria
    hit_rates = np.zeros(len(thresholds))
    false_alarm_rates = np.zeros(len(thresholds))
    
    for i, threshold in enumerate(thresholds):
        delta_prime = np.sqrt(da_prime**2 + dv_prime**2)
        hit_rates[i] = 1 - norm.cdf(threshold - delta_prime)  # Hit rate
        false_alarm_rates[i] = 1 - norm.cdf(threshold)  # False alarm rate
    
    return thresholds, hit_rates, false_alarm_rates

# Function to calculate AUC using the trapezoidal rule
def calculate_auc(hit_rates, false_alarm_rates):
    sorted_indices = np.argsort(false_alarm_rates)  # Sort based on false alarm rate
    false_alarm_rates = false_alarm_rates[sorted_indices]
    hit_rates = hit_rates[sorted_indices]
    auc = np.sum(np.diff(false_alarm_rates) * (hit_rates[:-1] + hit_rates[1:]) / 2)  # Trapezoidal rule
    return auc

# Function to compute the probability for the late model
def P_late(da_prime, dv_prime, m):
    dz = 1e-4  # Small step size
    zvals = np.arange(-15, 15, dz)  # Generate z-values from -15 to 15
    delta_prime = da_prime + dv_prime  # Compute delta_prime as the sum of da_prime and dv_prime
    pc = dz * np.sum(norm.pdf(zvals - delta_prime) * norm.cdf(zvals)**(m - 1))  # Calculate probability
    return pc

# Function to calculate hit rate (H) and false alarm rate (FA) for different thresholds (Late Model)
def roc_curve_points_late(da_prime, dv_prime, m):
    thresholds = np.arange(-5, 5, 0.1)  # Thresholds for decision criteria
    hit_rates = np.zeros(len(thresholds))
    false_alarm_rates = np.zeros(len(thresholds))
    
    for i, threshold in enumerate(thresholds):
        delta_prime = da_prime + dv_prime  # Sum of da_prime and dv_prime
        hit_rates[i] = 1 - norm.cdf(threshold - delta_prime)  # Hit rate
        false_alarm_rates[i] = 1 - norm.cdf(threshold)  # False alarm rate
    
    return thresholds, hit_rates, false_alarm_rates

# Set parameters for both models
da_prime_values = [0.5, 1.0, 1.5, 2.0]  # Different da' values
dv_prime_values = [0.5, 1.0, 1.5, 2.0]  # Different dv' values
m_value = 2  # Number of alternatives

# Create empty lists to store ROC data and AUC values for both models
roc_data_independent = []
auc_values_independent = []

roc_data_late = []
auc_values_late = []

# Loop over each combination of da_prime and dv_prime for the independent model
for da_prime in da_prime_values:
    for dv_prime in dv_prime_values:
        # Independent Model ROC and AUC
        thresholds, hit_rates, false_alarm_rates = roc_curve_points_independent(da_prime, dv_prime, m_value)
        auc_value_independent = calculate_auc(hit_rates, false_alarm_rates)
        auc_values_independent.append((da_prime, dv_prime, auc_value_independent))  # Store AUC for each combination
        
        # Store ROC points for Independent Model
        for threshold, hit_rate, false_alarm_rate in zip(thresholds, hit_rates, false_alarm_rates):
            roc_data_independent.append((da_prime, dv_prime, threshold, hit_rate, false_alarm_rate))

        # Late Model ROC and AUC
        thresholds, hit_rates, false_alarm_rates = roc_curve_points_late(da_prime, dv_prime, m_value)
        auc_value_late = calculate_auc(hit_rates, false_alarm_rates)
        auc_values_late.append((da_prime, dv_prime, auc_value_late))  # Store AUC for each combination
        
        # Store ROC points for Late Model
        for threshold, hit_rate, false_alarm_rate in zip(thresholds, hit_rates, false_alarm_rates):
            roc_data_late.append((da_prime, dv_prime, threshold, hit_rate, false_alarm_rate))

# Convert lists to numpy arrays for easier plotting
roc_data_independent = np.array(roc_data_independent)
roc_data_late = np.array(roc_data_late)

# Plot ROC curves for both models
plt.figure(figsize=(12, 6))

# Plot ROC for Independent Model
plt.subplot(1, 2, 1)
for da_prime in da_prime_values:
    for dv_prime in dv_prime_values:
        data = roc_data_independent[(roc_data_independent[:, 0] == da_prime) & (roc_data_independent[:, 1] == dv_prime)]
        plt.plot(data[:, 4], data[:, 3], label=f"da'={da_prime}, dv'={dv_prime}")
plt.title("ROC Curves for Independent Model")
plt.xlabel("False Alarm Rate (FA)")
plt.ylabel("Hit Rate (H)")
plt.legend(title="da', dv' Values")
plt.grid(True)

# Plot ROC for Late Model
plt.subplot(1, 2, 2)
for da_prime in da_prime_values:
    for dv_prime in dv_prime_values:
        data = roc_data_late[(roc_data_late[:, 0] == da_prime) & (roc_data_late[:, 1] == dv_prime)]
        plt.plot(data[:, 4], data[:, 3], label=f"da'={da_prime}, dv'={dv_prime}")
plt.title("ROC Curves for Late Model")
plt.xlabel("False Alarm Rate (FA)")
plt.ylabel("Hit Rate (H)")
plt.legend(title="da', dv' Values")
plt.grid(True)

plt.tight_layout()
plt.show()

# Print AUC values for both models
print("AUC values for Independent Model:")
for da_prime, dv_prime, auc_value in auc_values_independent:
    print(f"AUC for Independent Model (da'={da_prime}, dv'={dv_prime}): {auc_value:.2f}")

print("\nAUC values for Late Model:")
for da_prime, dv_prime, auc_value in auc_values_late:
    print(f"AUC for Late Model (da'={da_prime}, dv'={dv_prime}): {auc_value:.2f}")
