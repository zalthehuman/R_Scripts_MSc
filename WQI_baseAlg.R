# Load necessary library
setwd('/Users/zal/Desktop/grad school shit/Thesis Data/Laguna Lake/data/R_Scripts_DA/')

l
# Load necessary library
library(dplyr)

# Assuming your data is in a CSV file named 'data.csv'
data <- read.csv("res2.csv")

# Calculate base water quality index (without algal count)
base_wqi_params <- c("DO", "Ph", "Temp", "TSS", "BOD", "NH3", "FC", "Chloride", "NO3")
data$Base_WQI <- rowSums(data[, base_wqi_params]) / length(base_wqi_params)

# Ensure Algal Count is normalized (assuming it is based on your description)
# Calculate algal count penalty factor
algal_count_penalty <- 1 - 0.15 * data$AlgalCount

# Calculate final WQI by incorporating algal count penalty
data$WQI <- data$Base_WQI * algal_count_penalty

# Print the final data to check the WQI calculation
print(data[, c("AlgalCount", "Base_WQI", "WQI")])

algal_count_penalty



library(dplyr)

# Load your data from a CSV file named 'data.csv'
data <- read.csv("res2.csv")

# Define the parameters to be included in the base WQI calculation
base_wqi_params <- c("DO", "Ph", "Temp", "TSS", "BOD", "NH3", "FC", "Chloride", "NO3")

# Calculate the base water quality index (Base_WQI) as the mean of the selected parameters
data$Base_WQI <- rowSums(data[, base_wqi_params]) / length(base_wqi_params)

# Since Algal Count is already normalized, directly invert it
data$Inverted_AlgalCount <- 1 - data$AlgalCount

# Calculate the algal count penalty factor (higher inverted values decrease WQI more)
data$Algal_Count_Penalty <- 1 - 0.15 * data$Inverted_AlgalCount

# Calculate the final WQI by incorporating the algal count penalty
data$WQI <- data$Base_WQI * data$Algal_Count_Penalty

# Print the final data to check the WQI calculation
print(data[, c("AlgalCount", "Inverted_AlgalCount", "Base_WQI", "WQI")])
