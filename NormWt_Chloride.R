# Load necessary library
library(dplyr)

# Provided Chloride values
chloride_values <- c(193, 179, 175, 175, 175, 268, 169, 130, 83, 130, 122, 106, 77, 81, 182, 74, 75, 103,
                     261, 75, 55, 83, 79, 79, 91, 59, 64, 68, 2987, 1294, 498, 123, 93, 124, 132, 124,
                     116, 109, 116, 210, 305, 430, 430, 430, 430, 399, 310, 198, 102.14, 106.52, 184.31,
                     226.08, 194.13, 253.57, 326.4, 21.74, 204.22, 325.02, 64.36, 264.72, 223.87, 223.21,
                     150.08, 197.32, 97.52, 420.57, 481.52, 536.39, 205.89, 152.19, 5.7, 122.34)

# Create a DataFrame
chloride_data <- data.frame(Chloride = chloride_values)

# Normalization function for Chloride based on categories
normalize_chloride <- function(Cl) {
  if (Cl <= 145) {
    # Linear interpolation within the excellent range
    return(1 - (Cl / 145))
  } else if (Cl > 145 & Cl <= 230) {
    # Linear interpolation within the good range
    return(0.75 - ((Cl - 145) / (230 - 145) * 0.25))
  } else if (Cl > 230 & Cl <= 345) {
    # Linear interpolation within the fair range
    return(0.5 - ((Cl - 230) / (345 - 230) * 0.25))
  } else {
    # Poor range
    return(0)  # Cl > 345
  }
}

# Normalize the chloride scores
chloride_data$Normalized_Score <- sapply(chloride_data$Chloride, normalize_chloride)

# Weight for Chloride (0.09)
weight_chloride <- 0.09

# Function to apply weight to the normalized score
apply_weight_chloride <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
chloride_data$Weighted_Score <- apply_weight_chloride(chloride_data$Normalized_Score, weight_chloride)

# Categorize based on normalized score
chloride_data$Category <- cut(chloride_data$Normalized_Score,
                              breaks=c(-Inf, 0.01, 0.26, 0.51, Inf),
                              labels=c("Poor", "Fair", "Good", "Excellent"),
                              right=FALSE)

# Print the DataFrame
print(chloride_data)
