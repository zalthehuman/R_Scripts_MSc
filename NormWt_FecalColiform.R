# Load necessary library
library(dplyr)

# Provided FC values
FC_values <- c(23, 33, 7.8, 18, 350, 1.8, 4.5, 13, 11, 25, 7.8, 4.5, 13, 2, 4, 2, 20, 23, 7.8, 23, 2, 2,
               23, 1.8, 78, 1.8, 1.8, 1.8, 20, 18, 18, 1300, 18, 45, 40, 110, 20, 130, 18, 75, 47.5, 20,
               20, 20, 20, 20, 60, 100, 30, 26, 30, 37, 35, 106, 59, 268, 85, 85, 46, 46, 77.72, 19, 35,
               28, 28, 18, 19, 44, 44, 282, 220, 220)

# Create a DataFrame
FC_data <- data.frame(Fecal_Coliform = FC_values)

# Normalization function for Fecal Coliform based on categories
normalize_FC <- function(FC) {
  if (FC <= 50) {
    return(1.0)  # Excellent
  } else if (FC > 50 & FC <= 100) {
    return(0.75)  # Good
  } else if (FC > 100 & FC <= 200) {
    return(0.5)  # Fair
  } else {
    return(0)  # Poor
  }
}

# Normalize the FC scores
FC_data$Normalized_Score <- sapply(FC_data$Fecal_Coliform, normalize_FC)

# Weight for Fecal Coliform (0.13)
weight_FC <- 0.13

# Function to apply weight to the normalized score
apply_weight_FC <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
FC_data$Weighted_Score <- apply_weight_FC(FC_data$Normalized_Score, weight_FC)

# Categorize based on normalized score
FC_data$Category <- cut(FC_data$Normalized_Score,
                        breaks=c(-Inf, 0.01, 0.51, 0.76, Inf),
                        labels=c("Poor", "Fair", "Good", "Excellent"),
                        right=FALSE)

# Print the DataFrame
print(FC_data)
