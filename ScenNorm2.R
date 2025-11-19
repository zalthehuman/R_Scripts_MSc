# Load the required library
library(dplyr)

# Create the dataframe
data <- data.frame(
  Alternatives = c("UWTP- BAU", "UWTP-25%", "UWTP-50%", "UWTP-75%", "GDM-BAU", "GDM-25%", "GDM-50%", "GDM-75%", "CWTP-BAU", "CWTP-25%", "CWTP-50%", "CWTP-75%"),
  pH = c(8.25, 8.06, 7.73, 7.32, 6.80, 6.74, 6.64, 6.36, 7.09, 6.98, 6.82, 6.45),
  `Dissolved Oxygen` = c(6.89, 8.37, 9.04, 10.04, 7.17, 8.62, 9.76, 10.89, 9.20, 10.35, 11.25, 10.84),
  Temperature = c(28.31, 28.18, 27.89, 27.62, 26.35, 25.84, 25.64, 25.58, 21.85, 21.29, 21.04, 21.82),
  TSS = c(71.40, 60.20, 53.62, 47.47, 28.78, 23.93, 18.29, 11.44, 8.74, 6.89, 5.82, 8.02),
  BOD = c(3.96, 2.91, 2.67, 2.24, 2.33, 2.95, 1.90, 1.85, 1.06, 0.89, 0.82, 1.07),
  PO4 = c(0.27, 0.13, 0.11, 0.09, 7.62, 5.27, 3.93, 2.91, 0.05, 0.15, 0.03, 0.08),
  NH3 = c(0.22, 0.14, 0.09, 0.07, 13.01, 8.93, 6.38, 4.43, 4.34, 2.99, 2.41, 1.60),
  NO3 = c(0.37, 0.25, 0.16, 0.12, 25.02, 19.63, 13.75, 9.53, 10.72, 7.23, 5.03, 2.88),
  Chloride = c(263.91, 222.21, 194.26, 174.20, 120.00, 59.50, 39.00, 18.50, 10.45, 7.98, 6.89, 26.38),
  FC = c(145.57, 76.42, 62.95, 48.18, 10050.00, 7695.00, 2690.00, 435.00, 5346.48, 4245.79, 1523.57, 806.91),
  `Total Coliforms` = c(616.07, 512.01, 399.19, 311.25, 24200.00, 11195.00, 4190.00, 735.00, 26500.00, 16400.00, 6300.00, 1310.00),
  `Algal Count` = c(100001, 10001, 999, 999, NA, NA, NA, NA, NA, NA, NA, NA),
  EC = c(NA, NA, NA, NA, NA, NA, NA, NA, 863.27, 682.62, 534.00, 402.94),
  TDS = c(NA, NA, NA, NA, NA, NA, NA, NA, 433.28, 335.73, 278.98, 219.36),
  Salinity = c(NA, NA, NA, NA, NA, NA, NA, NA, 388.76, 296.86, 237.42, 213.84),
  Alkalinity = c(NA, NA, NA, NA, NA, NA, NA, NA, 196.42, 166.88, 138.71, 119.68)
)

# Handle missing values
for (param in names(data)[-1]) {
  if (any(is.na(data[[param]]))) {
    data[[param]][is.na(data[[param]])] <- mean(data[[param]], na.rm = TRUE)
  }
}

# Min-max normalization for Dissolved Oxygen (higher is better)
data$`Dissolved Oxygen_normalized` <- (data$`Dissolved Oxygen` - min(data$`Dissolved Oxygen`)) / (max(data$`Dissolved Oxygen`) - min(data$`Dissolved Oxygen`))

# Modified min-max normalization for other parameters (lower is better)
parameters <- c("pH", "Temperature", "TSS", "BOD", "PO4", "NH3", "NO3", "Chloride", "FC", "Total Coliforms", "Algal Count")

for (param in parameters) {
  data[[paste0(param, "_normalized")]] <- 1 - ((data[[param]] - min(data[[param]])) / (max(data[[param]]) - min(data[[param]])))
}

# Modified min-max normalization for EC, TDS, Salinity, Alkalinity (lower is better)
additional_parameters <- c("EC", "TDS", "Salinity", "Alkalinity")

for (param in additional_parameters) {
  if (all(is.na(data[[param]]))) {
    data[[paste0(param, "_normalized")]] <- NA
  } else {
    data[[paste0(param, "_normalized")]] <- 1 - ((data[[param]] - min(data[[param]], na.rm = TRUE)) / (max(data[[param]], na.rm = TRUE) - min(data[[param]], na.rm = TRUE)))
  }
}

# View the normalized dataset
print(data)

# Save the normalized dataset to a new CSV file
write.csv(data, "normalized_scen.csv", row.names = FALSE)
