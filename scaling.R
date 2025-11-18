library(dplyr)

# Hypothetical normalized scores for alternatives
alternatives_scores <- data.frame(
  Alternative = c('A', 'B', 'C', 'D'),
  Score = c(0.7, 0.8, 0.5, 0.9)  # Assume these are the result of AHP calculations
)

# Define the rating scale based on score
rating_scale <- function(score) {
  if (score > 0.8) {
    return('Excellent')
  } else if (score > 0.6) {
    return('Good')
  } else if (score > 0.4) {
    return('Average')
  } else if (score > 0.2) {
    return('Below Average')
  } else {
    return('Poor')
  }
}

# Apply the rating scale to each alternative
alternatives_scores$Rating <- sapply(alternatives_scores$Score, rating_scale)

# Display the dataframe with the ratings
print(alternatives_scores)
View(alternatives_scores)


