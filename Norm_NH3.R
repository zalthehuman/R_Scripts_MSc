# Load necessary library
library(dplyr)

# Provided NH3 values
NH3_values <- c(0.001, 0.16, 0.009, 0.111, 0.143, 0.23, 0.058, 0.099, 0.001, 0.054, 0.131, 0.022,
                0.221, 0.016, 0.654, 0.031, 0.022, 0.067, 0.043, 0.088, 0.011, 0.05, 0.033, 0.053,
                0.04, 0.02, 0.29, 0.066, 0.207, 0.112, 0.271, 0.36, 0.051, 0.143, 0.18, 0.012, 0.118,
                0.158, 0.222, 0.15, 0.0773, 0.059, 0.059, 0.059, 0.059, 0.005, 0.005, 0.005, 0.034,
                0.079, 0.059, 0.1, 0.09, 0.09, 0.11, 0.03, 0.07, 0.35, 0.07, 0.209, 0.11, 0.546, 0.07,
                0.03, 0.01, 0.02, 0.09, 0.16, 0.07, 0.03, 0.04, 0.005)

# Create a DataFrame
NH3_data <- data.frame(Ammonia = NH3_values)

# Normalization function for NH3 based on categories
normalize_NH3 <- function(NH3) {
  if (NH3 <= 0.02) {
    return(1.0)  # Excellent
  } else if (NH3 > 0.02 & NH3 <= 0.04) {
    return(0.75)  # Good
  } else if (NH3 > 0.04 & NH3 <= 0.05) {
    return(0.5)  # Fair
  } else {
    return(0)  # Poor
  }
}

# Normalize the NH3 scores
NH3_data$Normalized_Score <- sapply(NH3_data$Ammonia, normalize_NH3)

# Weight for NH3 (Example: 0.08)
weight_NH3 <- 0.08

# Function to apply weight to the normalized score
apply_weight_NH3 <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
NH3_data$Weighted_Score <- apply_weight_NH3(NH3_data$Normalized_Score, weight_NH3)

# Categorize based on normalized score
NH3_data$Category <- cut(NH3_data$Normalized_Score,
                         breaks=c(-Inf, 0.01, 0.51, 0.76, Inf),
                         labels=c("Poor", "Fair", "Good", "Excellent"),
                         right=FALSE)

# Print the DataFrame
print(NH3_data)
