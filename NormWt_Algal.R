# Load necessary library
library(dplyr)

# Provided Algal Count values
algal_values <- c(9813, 1010, 26791, 3106, 13930, 35392, 18021, 2884, 53924, 57616, 336456, 33098,
                  2630, 320, 930, 8767, 4074, 3292, 24672, 40838, 412059, 17427, 3354, 58151, 16674,
                  29659, 1322, 2458, 94992, 186554, 14010, 3991, 5447, 2298, 8006, 1029, 3263, 38280,
                  46938.70313, 46938.70313, 46938.70313, 46938.70313, 46938.70313, 46938.70313, 61989,
                  160241, 30681, 1419, 6604, 28090, 96652, 46938.70313, 19910, 20125, 26788, 7795,
                  27896, 28648, 14504, 9471, 46938.70313, 472033, 6304, 12276, 15618, 127298, 122370,
                  119172, 8613, 535, 1931, 576)

# Create a DataFrame
algal_data <- data.frame(Algal_Count = algal_values)

# Normalization function for Algal Count based on categories
normalize_algal <- function(algal) {
  if (algal <= 1000) {
    return(1.0)  # Excellent
  } else if (algal > 1001 & algal <= 25000) {
    return(0.75)  # Good
  } else if (algal > 25001 & algal <= 75251) {
    return(0.5)  # Fair
  } else {
    return(0)  # Poor
  }
}

algal_data$Normalized_Score

# Normalize the algal count scores
algal_data$Normalized_Score <- sapply(algal_data$Algal_Count, normalize_algal)

# Weight for Algal Count (0.15)
weight_algal <- 0.15

# Function to apply weight to the normalized score
apply_weight_algal <- function(score, weight) {
  return(score * weight)
}

# Apply weight to normalized scores
algal_data$Weighted_Score <- apply_weight_algal(algal_data$Normalized_Score, weight_algal)

# Categorize based on normalized score
algal_data$Category <- cut(algal_data$Normalized_Score,
                           breaks=c(-Inf, 0.01, 0.51, 0.76, Inf),
                           labels=c("Poor", "Fair", "Good", "Excellent"),
                           right=FALSE)

# Print the DataFrame
print(algal_data)




##############################################################################################

# Find the minimum and maximum values
min_AlgalCount <- min(AlgalCount)
max_AlgalCount <- max(AlgalCount)

# Normalize the scores using a different formula
normalized_scores <- (max_AlgalCount - AlgalCount) / (max_AlgalCount - min_AlgalCount) 

# Adjust the normalization so that lower values are closer to 1
normalized_scores <- 1 - normalized_scores

# Print the normalized scores
print(normalized_scores)
