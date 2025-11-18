# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the data
data <- read.csv("ActualD_CWB.csv")

# Calculate mean and standard deviation 
stats <- data %>% summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))))

# Reshape the stats dataframe 
stats_long <- pivot_longer(stats, cols = everything(), names_to = c(".value", "parameter"), names_sep = "_")

# Join the stats back to the original data and categorize
data <- data %>%
  pivot_longer(cols = everything()) %>%
  left_join(stats_long, by = c("name" = "parameter")) %>%
  mutate(Category = case_when(
    value < (mean - sd) ~ "Poor",
    value >= (mean - 0.5 * sd) & value <= (mean + 0.5 * sd) ~ "Fair",
    value > (mean + 0.5 * sd) & value <= (mean + sd) ~ "Good",
    value > (mean + sd) ~ "Excellent",
    TRUE ~ "Uncategorized"  # For any data points that don't fit the above categories
  )) %>%
  pivot_wider(names_from = name, values_from = c(value, Category))

# Check the first few rows to confirm the categorization
head(data)
print(data)

# Load necessary libraries
library(dplyr)

# Calculate mean, standard deviation, and breakpoints for each parameter
breakpoints_summary <- data %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    poor = ~mean(., na.rm = TRUE) - sd(., na.rm = TRUE),
    fair_lower = ~mean(., na.rm = TRUE) - 0.5 * sd(., na.rm = TRUE),
    fair_upper = ~mean(., na.rm = TRUE) + 0.5 * sd(., na.rm = TRUE),
    good_lower = ~mean(., na.rm = TRUE) + 0.5 * sd(., na.rm = TRUE),
    good_upper = ~mean(., na.rm = TRUE) + sd(., na.rm = TRUE),
    excellent = ~mean(., na.rm = TRUE) + sd(., na.rm = TRUE)
  )))

# View the breakpoints summary
print(breakpoints_summary)

# Write the breakpoints summary to a CSV file
write.csv(breakpoints_summary, "breakpoints_summary.csv", row.names = FALSE)

