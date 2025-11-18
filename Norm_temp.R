# Load necessary library
library(dplyr)

# Provided Temperature values
temperature_values <- c(22.19, 25.03, 21.74, 18.55, 21.06, 24.54, 20.68, 21.63, 21.77, 22.02, 20.95, 20.4,
                                21.07, 20.91, 22.48, 20.36, 20.94, 22.3, 21.83, 19.9)

# Create a DataFrame
temperature_data <- data.frame(Temperature = temperature_values)

# Normalization function for Temperature
normalize_temperature <- function(temp) {
  if (temp >= 15 & temp <= 26) {
    # Apply a parabolic score that peaks at 28.5 and decreases towards 25 and 32
    return(1 - ((temp - 20.5) ^ 2) / (2.5 ^ 2))  # 3.5 is half the range width
  } else {
    return(0)  # Suboptimal for values outside 25-32
  }
}

# Normalize the temperature scores
temperature_data$Normalized_Score <- sapply(temperature_data$Temperature, normalize_temperature)

# Weight for Temperature
weight_temperature <- 0.09

# Function to apply weight to the normalized score
apply_weight_temperature <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
temperature_data$Weighted_Score <- apply_weight_temperature(temperature_data$Normalized_Score, weight_temperature)

# Categorize based on normalized score
temperature_data$Category <- cut(temperature_data$Normalized_Score,
                                 breaks=c(-Inf, 0.25, 0.5, 0.75, Inf),
                                 labels=c("Suboptimal", "Poor", "Fair", "Good", "Excellent"),
                                 right=FALSE)

# Print the DataFrame
print(temperature_data)
