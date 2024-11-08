import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
import seaborn as sns

# Function to calculate hit rate and false alarm rate based on counts
def calculate_rates(hits, total_signal_present, false_alarms, total_signal_absent):
    # Calculate hit rate: proportion of hits over total signal-present trials
    hit_rate = hits / total_signal_present
    
    # Calculate false alarm rate: proportion of false alarms over total signal-absent trials
    false_alarm_rate = false_alarms / total_signal_absent
    
    # Ensure hit_rate and false_alarm_rate are within valid range (0,1)
    hit_rate = np.clip(hit_rate, 1e-5, 1 - 1e-5)  # To avoid Z-scores at infinity
    false_alarm_rate = np.clip(false_alarm_rate, 1e-5, 1 - 1e-5)
    
    return hit_rate, false_alarm_rate

# Function to calculate d' (d-prime) using the hit rate and false alarm rate
def calculate_d_prime(hits, total_signal_present, false_alarms, total_signal_absent):
    hit_rate, false_alarm_rate = calculate_rates(hits, total_signal_present, false_alarms, total_signal_absent)
    
    # Calculate Z-scores for hit rate and false alarm rate using the inverse CDF (ppf)
    z_hit_rate = norm.ppf(hit_rate)
    z_false_alarm_rate = norm.ppf(false_alarm_rate)
    
    # Calculate d' as the difference between Z(hit rate) and Z(false alarm rate)
    d_prime = z_hit_rate - z_false_alarm_rate
    
    return d_prime, hit_rate, false_alarm_rate

# Example parameters
hits = 85  # Number of hits (signal present, correct response)
total_signal_present = 100  # Total signal-present trials
false_alarms = 10  # Number of false alarms (signal absent, incorrect response)
total_signal_absent = 100  # Total signal-absent trials

# Calculate d' and rates
d_prime, hit_rate, false_alarm_rate = calculate_d_prime(hits, total_signal_present, false_alarms, total_signal_absent)

# Print d' value
print(f"d' = {d_prime}")

# Create the distributions for visualization
z_values = np.linspace(-3, 3, 1000)
signal_distribution = norm.pdf(z_values, loc=norm.ppf(hit_rate), scale=1)  # Signal distribution
noise_distribution = norm.pdf(z_values, loc=norm.ppf(false_alarm_rate), scale=1)  # Noise distribution

# Plotting the distributions
plt.figure(figsize=(10, 6))
plt.plot(z_values, signal_distribution, color="blue", label="Signal Distribution")
plt.plot(z_values, noise_distribution, color="red", label="Noise Distribution")
plt.fill_between(z_values, signal_distribution, where=(z_values > norm.ppf(false_alarm_rate) + 0.01) & (z_values < norm.ppf(hit_rate)), color="lightblue", alpha=0.5, label="Hits Area")
plt.fill_between(z_values, noise_distribution, where=(z_values > norm.ppf(false_alarm_rate) - 0.01) & (z_values < norm.ppf(false_alarm_rate)), color="lightcoral", alpha=0.5, label="False Alarms Area")
plt.axvline(norm.ppf(hit_rate), linestyle="--", color="blue", label="Hit Rate (dashed line)")
plt.axvline(norm.ppf(false_alarm_rate), linestyle="--", color="red", label="False Alarm Rate (dashed line)")
plt.text(norm.ppf(hit_rate), 0.01, "Hit Rate (dashed line)", color="blue", verticalalignment='bottom', horizontalalignment='center')
plt.text(norm.ppf(false_alarm_rate), 0.01, "False Alarm Rate (dashed line)", color="red", verticalalignment='bottom', horizontalalignment='center')
plt.text((norm.ppf(hit_rate) + norm.ppf(false_alarm_rate)) / 2, 0.15, f"d' = {d_prime:.3f}", fontsize=12, color="black")
plt.title("Signal Detection Theory: Hit Rate and False Alarm Rate")
plt.xlabel("Z-score")
plt.ylabel("Density")
plt.legend()
plt.grid(True)
plt.show()

# Criterion and d' calculation function
def calculate_d_prime_and_c(hits, total_signal_present, false_alarms, total_signal_absent):
    hit_rate, false_alarm_rate = calculate_rates(hits, total_signal_present, false_alarms, total_signal_absent)
    
    # Calculate Z-scores for hit rate and false alarm rate using the inverse CDF (ppf)
    z_hit_rate = norm.ppf(hit_rate)
    z_false_alarm_rate = norm.ppf(false_alarm_rate)
    
    # Calculate d' as the difference between Z(hit rate) and Z(false alarm rate)
    d_prime = z_hit_rate - z_false_alarm_rate
    
    # Calculate the criterion (c)
    c = - (z_hit_rate + z_false_alarm_rate) / 2
    
    return d_prime, c, hit_rate, false_alarm_rate

# Calculate d', c, and rates
d_prime, c, hit_rate, false_alarm_rate = calculate_d_prime_and_c(hits, total_signal_present, false_alarms, total_signal_absent)

# Plotting the distributions with Criterion
plt.figure(figsize=(10, 6))
plt.plot(z_values, signal_distribution, color="blue", label="Signal Distribution")
plt.plot(z_values, noise_distribution, color="red", label="Noise Distribution")
plt.fill_between(z_values, signal_distribution, where=(z_values > norm.ppf(false_alarm_rate) + 0.01) & (z_values < norm.ppf(hit_rate)), color="lightblue", alpha=0.5, label="Hits Area")
plt.fill_between(z_values, noise_distribution, where=(z_values > norm.ppf(false_alarm_rate) - 0.01) & (z_values < norm.ppf(false_alarm_rate)), color="lightcoral", alpha=0.5, label="False Alarms Area")
plt.axvline(norm.ppf(hit_rate), linestyle="--", color="blue", label="Hit Rate (dashed line)")
plt.axvline(norm.ppf(false_alarm_rate), linestyle="--", color="red", label="False Alarm Rate (dashed line)")
plt.axvline(c, linestyle="-", color="purple", label="Criterion (c) Line")
plt.text(norm.ppf(hit_rate), 0.01, "Hit Rate (dashed line)", color="blue", verticalalignment='bottom', horizontalalignment='center')
plt.text(norm.ppf(false_alarm_rate), 0.01, "False Alarm Rate (dashed line)", color="red", verticalalignment='bottom', horizontalalignment='center')
plt.text(c, 0.05, f"Criterion (c) = {c:.3f}", color="purple", verticalalignment='bottom', horizontalalignment='center')
plt.text((norm.ppf(hit_rate) + norm.ppf(false_alarm_rate)) / 2, 0.15, f"d' = {d_prime:.3f}", fontsize=12, color="black")
plt.title("Signal Detection Theory: Hit Rate, False Alarm Rate, d', and Criterion (c)")
plt.xlabel("Z-score")
plt.ylabel("Density")
plt.legend()
plt.grid(True)
plt.show()

# Function to calculate proportion correct from dprime
def prob_from_dp(dp_value, m):
    dz = 1e-4  # Step size for approximation
    zvals = np.arange(-15, 15, dz)
    pc = dz * np.sum(norm.pdf(zvals - dp_value) * norm.cdf(zvals)**(m - 1))
    return pc

# Example d' value and M
d_prime_example = 1
M_example = 2
pc_result = prob_from_dp(d_prime_example, M_example)
print(f"Proportion Correct (from d') = {pc_result}")

# Function to calculate proportion correct from independent model
def P_independent(da_prime, dv_prime, m):
    dz = 1e-4
    zvals = np.arange(-15, 15, dz)
    delta_prime = np.sqrt(da_prime**2 + dv_prime**2)
    pc = dz * np.sum(norm.pdf(zvals - delta_prime) * norm.cdf(zvals)**(m - 1))
    return pc

# Example usage of the equation
da_prime_example = 2
dv_prime_example = 1
M_example = 5
pc_independent = P_independent(da_prime_example, dv_prime_example, M_example)
print(f"Proportion Correct (Independent Model) = {pc_independent}")

# Function to calculate proportion correct from late model
def P_late(da_prime, dv_prime, m):
    dz = 1e-4
    zvals = np.arange(-15, 15, dz)
    delta_prime = da_prime + dv_prime
    pc = dz * np.sum(norm.pdf(zvals - delta_prime) * norm.cdf(zvals)**(m - 1))
    return pc

# Example usage of the equation
pc_late = P_late(da_prime_example, dv_prime_example, M_example)
print(f"Proportion Correct (Late Model) = {pc_late}")
