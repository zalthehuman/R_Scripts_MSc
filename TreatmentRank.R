# Load necessary libraries
library(ggplot2)
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
         Scenario = gsub(".*-", "", Alternatives), # Extract Scenario
         NormalizedRank = ave(Rank, Treatment, FUN = function(x) (x - min(x)) / (max(x) - min(x)))) %>%
  arrange(Treatment, Scenario) # Arrange for clarity

# Define rating scale based on normalized rank
data$Rating <- cut(data$NormalizedRank,
                   breaks=c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf),
                   labels=c("Very Poor", "Poor", "Moderate", "Good", "Very Good"))

# Plotting
ggplot(data, aes(x = Alternatives, y = NormalizedRank, fill = Rating)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "salmon", "pink", "lightblue", "blue")) +
  theme_minimal() +
  labs(title = "Clustered Bar Chart of Normalized Rank by Treatment Group",
       x = "Alternatives",
       y = "Normalized Rank") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for readability

# Print the dataframe and plot
print(data)
ggsave("treatment_ratings.png", width = 10, height = 6) # Save the plot as an image
