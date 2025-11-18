# Load necessary library
library(dplyr)

# Provided NO3 values
NO3_values <- c(0.223, 0.405, 0.267, 0.413, 0.001, 0.224, 0.031, 0.018, 0.206, 0.046, 0.361, 0.263,
                0.666, 0.459, 0.188, 0.138, 0.109, 0.05, 0.05, 0.594, 0.05, 0.05, 0.02, 0.123, 0,
                0.05, 0.05, 0.05, 0.05, 0.05, 0.072, 0.223, 0.618, 0.05, 0.05, 0.185, 0.274, 0.153,
                0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.21, 0.21, 0.13, 0.2,
                0.05, 0.05, 0.05, 1.13, 0.3, 0.05, 0.05, 0.38, 0.22, 0.631, 0.02, 0.02, 0.03, 0.01,
                0.005, 0.05, 0.61, 1, 0.48, 0.76)

# Create a DataFrame
NO3_data <- data.frame(Nitrate = NO3_values)

# Normalization function for Nitrate based on categories
normalize_NO3 <- function(NO3) {
  if (NO3 <= 1) {
    return(1.0)  # Excellent
  } else if (NO3 > 1 & NO3 <= 3) {
    return(0.75)  # Good
  } else if (NO3 > 3 & NO3 <= 5) {
    return(0.5)  # Fair
  } else {
    return(0)  # Poor
  }
}

# Normalize the NO3 scores
NO3_data$Normalized_Score <- sapply(NO3_data$Nitrate, normalize_NO3)

# Weight for Nitrate (Example: 0.08)
weight_NO3 <- 0.08

# Function to apply weight to the normalized score
apply_weight_NO3 <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
NO3_data$Weighted_Score <- apply_weight_NO3(NO3_data$Normalized_Score, weight_NO3)

# Categorize based on normalized score
NO3_data$Category <- cut(NO3_data$Normalized_Score,
                         breaks=c(-Inf, 0.01, 0.51, 0.76, Inf),
                         labels=c("Poor", "Fair", "Good", "Excellent"),
                         right=FALSE)

# Print the DataFrame
print(NO3_data)
