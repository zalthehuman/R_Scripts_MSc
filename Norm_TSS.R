# Load necessary library
library(dplyr)

# Provided TSS values
TSS_values <- c(1.37, 2.5, 8.73, 2.5, 3, 2, 4.5, 5.79, 2.55, 19.5, 16.75, 2.67, 6.75, 9, 1.5, 15, 2, 4, 10.33)

# Create a DataFrame
TSS_data <- data.frame(Total_Suspended_Solids = TSS_values)

# Normalization function for TSS
normalize_TSS <- function(TSS) {
  if (TSS < 20) {
    return(1)  # Excellent
  } else if (TSS >= 20 & TSS <= 50) {
    return(1 - (TSS - 20) / (50 - 20) * (1 - 0.85))  # Good
  } else if (TSS > 50 & TSS <= 100) {
    return(0.85 - (TSS - 50) / (100 - 50) * (0.85 - 0.4))  # Fair
  } else if (TSS > 100 & TSS <= 200) {
    return(0.4 - (TSS - 100) / (200 - 100) * (0.4 - 0))  # Poor
  } else {
    return(0)  # Extremely poor, beyond the categorization
  }
}

# Apply normalization to the data
TSS_data$Normalized_Score <- sapply(TSS_data$Total_Suspended_Solids, normalize_TSS)


# Weight for TSS (example)
weight_TSS <- 0.12

# Function to apply weight to the normalized score
apply_weight <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
TSS_data$Weighted_Score <- apply_weight(TSS_data$Normalized_Score, weight_TSS)

# Categorize based on normalized score
TSS_data$Category <- cut(TSS_data$Normalized_Score,
                         breaks=c(-Inf, 0.4, 0.85, 1, Inf),
                         labels=c("Poor", "Fair", "Good", "Excellent"),
                         right=FALSE)

# Display the DataFrame
print(TSS_data)
