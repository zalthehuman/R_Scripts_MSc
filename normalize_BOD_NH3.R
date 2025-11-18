
# Load necessary library
library(dplyr)

# Provided BOD values
BOD_values <- c(1.08, 1.04, 1.03, 1.04, 1.04, 1, 1.05, 1.05, 1.25, 1.01, 1, 1.17, 1.17, 1, 1, 1, 1.05, 1, 1, 1)


# Create a DataFrame
BOD_data <- data.frame(BOD = BOD_values)

# Normalization function for BOD based on categories
normalize_BOD <- function(BOD) {
  if (BOD <= 1) {
    return(1.0)  # Excellent
  } else if (BOD > 1.0 & BOD <= 3) {
    return(0.75)  # Good
  } else if (BOD >3  & BOD <= 5) {
    return(0.5)  # Fair
  } else {
    return(0)  # Poor
  }
}


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

# Normalize the BOD scores
BOD_data$Normalized_Score <- sapply(BOD_data$BOD, normalize_BOD)

# Weight for BOD
weight_BOD <- 0.09

# Function to apply weight to the normalized score
apply_weight_BOD <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
BOD_data$Weighted_Score <- apply_weight_BOD(BOD_data$Normalized_Score, weight_BOD)

# Categorize based on normalized score
BOD_data$Category <- cut(BOD_data$Normalized_Score,
                         breaks=c(-Inf, 0.01, 0.51, 0.76, Inf),
                         labels=c("Poor", "Fair", "Good", "Excellent"),
                         right=FALSE)

# Print the DataFrame
print(BOD_data)

