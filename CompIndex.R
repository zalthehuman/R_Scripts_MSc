# Load the necessary libraries
library(dplyr)

# Create data frame
alternatives <- c('UWTP-BAU', 'UWTP-25%', 'UWTP-50%', 'UWTP-75%',
                  'GDM-BAU', 'GDM-25%', 'GDM-50%', 'GDM-75%',
                  'CWTP-BAU', 'CWTP-25%', 'CWTP-50%', 'CWTP-75%')
rank <- c(0.361, 0.472, 0.538, 0.586,
          0.351, 0.456, 0.619, 0.706,
          0.489, 0.657, 0.776, 0.888)

data <- data.frame(Alternatives = alternatives, Rank = rank)

# Normalize the ranks within each treatment group
data <- data %>%
  mutate(Treatment = gsub("-.*", "", Alternatives), # Extract Treatment
         NormalizedRank = ave(Rank, Treatment, FUN = function(x) (x - min(x)) / (max(x) - min(x)))) %>%
  arrange(Treatment, Alternatives) # Arrange for clarity

# Calculate the composite index
composite_index <- data %>%
  group_by(Treatment) %>%
  summarize(CompositeIndex = mean(NormalizedRank)) %>%
  ungroup() # Ungroup to remove the grouping structure

# Print the composite index
print(composite_index)


# Calculate quantiles for the composite index
quantile_breakpoints <- quantile(composite_index$CompositeIndex, probs = c(0, 0.25, 0.5, 0.75, 1))

# Print the quantile breakpoints
print(quantile_breakpoints)
