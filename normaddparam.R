# Create the Algal Count data vector
algal_count <- c(100001, 10001, 999, 999, NA, NA, NA, NA, NA, NA, NA, NA)

# Remove missing values from the Algal Count data
algal_count <- algal_count[!is.na(algal_count)]

# Modified min-max normalization for Algal Count (lower is better)
algal_count_normalized <- 1 - ((algal_count - min(algal_count)) / (max(algal_count) - min(algal_count)))

# Create a dataframe with the original and normalized Algal Count data
algal_count_data <- data.frame(
  Algal_Count = algal_count,
  Algal_Count_Normalized = algal_count_normalized
)

# View the Algal Count data with normalization
print(algal_count_data)

# Save the Algal Count data with normalization to a new CSV file
write.csv(algal_count_data, "algal_count_normalized.csv", row.names = FALSE)


# Create the Dissolved Oxygen data vector
dissolved_oxygen <- c(6.89, 8.37, 9.04, 10.04, 7.17, 8.62, 9.76, 10.89, 9.20, 10.35, 11.25, 10.84)

# Min-max normalization for Dissolved Oxygen (higher is better)
dissolved_oxygen_normalized <- (dissolved_oxygen - min(dissolved_oxygen)) / (max(dissolved_oxygen) - min(dissolved_oxygen))

# Create a dataframe with the original and normalized Dissolved Oxygen data
dissolved_oxygen_data <- data.frame(
  Dissolved_Oxygen = dissolved_oxygen,
  Dissolved_Oxygen_Normalized = dissolved_oxygen_normalized
)

# View the Dissolved Oxygen data with normalization
print(dissolved_oxygen_data)

# Save the Dissolved Oxygen data with normalization to a new CSV file
write.csv(dissolved_oxygen_data, "dissolved_oxygen_normalized.csv", row.names = FALSE)