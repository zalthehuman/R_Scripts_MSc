# pH values provided
pH_values <- c(8.01, 7.94, 7.02, 8.28, 6.52, 6.65, 12.04, 7.5, 7.01, 7.17, 5.88, 5.94, 8.19, 7.8, 6.9, 7.77, 7.56, 7.15, 7.11, 7.21)

# Normalization function for pH values with parabolic scoring centered at 7.5
normalize_pH_parabolic <- function(pH) {
  if (pH >= 6.5 & pH <= 8.5) {
    # Score peaks at 7.5 and decreases towards 6.5 and 8.5
    return(1 - (pH - 7.5)^2 / (8.5 - 7.5)^2)
  } else if ((pH >= 6.0 & pH < 6.5) || (pH > 8.5 & pH <= 9.0)) {
    # Good range: set to a fixed lower score, for example 0.8
    return(0.8)
  } else if ((pH >= 5.5 & pH < 6.0) || (pH > 9.0 & pH <= 9.5)) {
    # Fair range: set to a fixed even lower score, for example 0.6
    return(0.6)
  } else {
    # Poor range: set to a fixed low score, for example 0.4
    return(0.4)
  }
}

# Apply the normalization function to each pH value
normalized_scores_parabolic <- sapply(pH_values, normalize_pH_parabolic)

# Display normalized scores
normalized_scores_parabolic

################################################################################################################################################


# Normalization function for pH values with nuanced scoring
normalize_pH_nuanced <- function(pH) {
  if (pH >= 6.5 & pH <= 8.5) {
    # Applying a parabolic function that peaks at 7.5
    return(max(0, 1 - ((pH - 7.5) / 1.5)^2))
  } else {
    # Out of standard range
    return(0.1)  # Very low score for out-of-range values
  }
}

# Apply the normalization function to each pH value
normalized_scores_nuanced <- sapply(pH_values, normalize_pH_nuanced)

# Display normalized scores
normalized_scores_nuanced


# Weight for the pH score
weight_pH <- 0.08

# Normalization function for pH values with nuanced scoring and weighting
normalize_and_weight_pH <- function(pH) {
  # Calculate normalized score
  if (pH >= 6.5 & pH <= 8.5) {
    score <- max(0, 1 - ((pH - 7.5) / 1.5)^2)
  } else {
    score <- 0.1  # Very low score for out-of-range values
  }
  
  # Apply weight
  weighted_score <- score * weight_pH
  return(weighted_score)
}

# Apply the normalization and weighting function to each pH value
pH_values <- c(8.01, 7.94, 7.02, 8.28, 6.52, 6.65, 12.04, 7.5, 7.01, 7.17, 5.88, 5.94, 8.19, 7.8, 6.9, 7.77, 7.56, 7.15, 7.11, 7.21)  # Example pH values
weighted_scores <- sapply(pH_values, normalize_and_weight_pH)

# Display weighted scores
weighted_scores

