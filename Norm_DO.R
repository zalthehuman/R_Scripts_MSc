# Load necessary library
library(dplyr)

# Provided DO values
DO_values <- c(12.77, 12.79, 10, 12.85, 8.76, 8.96, 12.94, 9.88, 9.6, 9.15, 9.83, 10.68, 8.9, 7.22, 7.19, 
               7.35, 7.87, 11.17, 9.52, 9.28)
# Create a DataFrame
DO_data <- data.frame(Dissolved_Oxygen = DO_values)

# Normalization function for dissolved oxygen
normalize_DO <- function(DO) {
  if (DO > 10) {
    return(1)  # Excellent
  } else if (DO >= 6.0 & DO <= 8.0) {
    return(0.7 + (DO - 6.0) * (0.85 - 0.7) / (8.0 - 6.0))  # Good
  } else if (DO >= 4.0 & DO < 6.0) {
    return(0.4 + (DO - 4.0) * (0.7 - 0.4) / (6.0 - 4.0))  # Fair
  } else {
    return(0.2 + (DO - 2.0) * (0.4 - 0.2) / (4.0 - 2.0))  # Poor
  }
}

# Apply normalization to the data
DO_data$Normalized_Score <- sapply(DO_data$Dissolved_Oxygen, normalize_DO)

# Categorize based on normalized score
DO_data$Category <- cut(DO_data$Normalized_Score, 
                        breaks=c(-Inf, 0.4, 0.7, 0.85, Inf), 
                        labels=c("Poor", "Fair", "Good", "Excellent"),
                        right=FALSE)

# Display the DataFrame
print(DO_data)



######################################################################################## with weights
# Load necessary library
library(dplyr)

# Provided DO values
DO_values <- c(12.77, 12.79, 10, 12.85, 8.76, 8.96, 12.94, 9.88, 9.6, 9.15, 9.83, 10.68, 8.9, 7.22, 7.19, 
               7.35, 7.87, 11.17, 9.52, 9.28)
# Create a DataFrame
DO_data <- data.frame(Dissolved_Oxygen = DO_values)

# Normalization function for dissolved oxygen
normalize_DO <- function(DO) {
  if (DO > 10) {
    return(1)  # Excellent
  } else if (DO >= 6.0 & DO <= 8.0) {
    return(0.7 + (DO - 6.0) * (0.85 - 0.7) / (8.0 - 6.0))  # Good
  } else if (DO >= 4.0 & DO < 6.0) {
    return(0.4 + (DO - 4.0) * (0.7 - 0.4) / (6.0 - 4.0))  # Fair
  } else {
    return(0.2 + (DO - 2.0) * (0.4 - 0.2) / (4.0 - 2.0))  # Poor
  }
}

# Apply normalization to the data
DO_data$Normalized_Score <- sapply(DO_data$Dissolved_Oxygen, normalize_DO)

# Weight for dissolved oxygen
weight_DO <- 0.09

# Function to apply weight to the normalized score
apply_weight <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
DO_data$Weighted_Score <- apply_weight(DO_data$Normalized_Score, weight_DO)

# Categorize based on normalized score
DO_data$Category <- cut(DO_data$Normalized_Score, 
                        breaks=c(-Inf, 0.4, 0.7, 0.85, Inf), 
                        labels=c("Poor", "Fair", "Good", "Excellent"),
                        right=FALSE)

# Display the DataFrame
print(DO_data)

