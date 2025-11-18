# Load the required data into a data frame
data <- data.frame(
  Alternatives = c('UWTP-BAU', 'UWTP-25%', 'UWTP-50%', 'UWTP-75%', 'GDM-BAU', 'GDM-25%', 'GDM-50%', 'GDM-75%', 'CWTP-BAU', 'CWTP-25%', 'CWTP-50%', 'CWTP-75%'),
  pH = c(8.25, 8.06, 7.73, 7.32, 6.80, 6.74, 6.64, 6.36, 7.09, 6.98, 6.82, 6.45),
  DO = c(6.89, 8.37, 9.04, 10.04, 7.17, 8.62, 9.76, 10.89, 9.20, 10.35, 11.25, 10.84),
  Temp = c(28.31, 28.18, 27.89, 27.62, 26.35, 25.84, 25.64, 25.58, 21.85, 21.29, 21.04, 21.82),
  TSS = c(71.40, 60.20, 53.62, 47.47, 28.78, 23.93, 18.29, 11.44, 8.74, 6.89, 5.82, 8.02),
  BOD = c(3.96, 2.91, 2.67, 2.24, 2.33, 2.95, 1.90, 1.85, 1.06, 0.89, 0.82, 1.07),
  PO4  = c(0.27, 0.13, 0.11, 0.09, 7.62, 5.27, 3.93, 2.91, 0.05, 0.15, 0.03, 0.08),
  NH3 = c(0.22, 0.14, 0.09, 0.07, 13.01, 8.93, 6.38, 4.43, 4.34, 2.99, 2.41, 1.60),
  NO3 = c(0.37, 0.25, 0.16, 0.12, 25.02, 19.63, 13.75, 9.53, 10.72, 7.23, 5.03, 2.88),
  Chloride = c(263.91, 222.21, 194.26, 174.20, 120.00, 59.50, 39.00, 18.50, 10.45, 7.98, 6.89, 26.38),
  FC = c(145.57, 76.42, 62.95, 48.18, 10050.00, 7695.00, 2690.00, 435.00, 5346.48, 4245.79, 1523.57, 806.91),
  Biol = c(81776.16, 56861.08, 43067.16, 35075.02, NA, NA, NA, NA, NA, NA, NA, NA),
  EC = c(NA, NA, NA, NA, 863.27, 682.62, 534.00, 402.94, NA, NA, NA, NA),
  TDS = c(NA, NA, NA, NA, 433.28, 335.73, 278.98, 219.36, NA, NA, NA, NA),
  Salinity = c(NA, NA, NA, NA, 388.76, 296.86, 237.42, 213.84, NA, NA, NA, NA),
  Alkalinity = c(NA, NA, NA, NA, 196.42, 166.88, 138.71, 119.68, NA, NA, NA, NA)
)

# Removing the first column (Alternatives) for normalization
criteria_data <- data[,-1]

# Normalize function, where higher values are better
normalize_higher <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Normalize function, where lower values are better
normalize_lower <- function(x) {
  1 - (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Assume the following:
# Higher is better: DO, NO3 (beneficial nutrients), pH
# Lower is better: Temp, TSS, BOD, PO4, NH3, Chloride, FC (less pollution or better water quality)

# Applying normalization
criteria_data$DO <- ifelse(is.na(criteria_data$DO), NA, normalize_higher(criteria_data$DO))
criteria_data$NO3 <- ifelse(is.na(criteria_data$NO3), NA, normalize_higher(criteria_data$NO3))
criteria_data$pH <- ifelse(is.na(criteria_data$pH), NA, normalize_higher(criteria_data$pH))
criteria_data$Biol <- ifelse(is.na(criteria_data$Biol), NA, normalize_higher(criteria_data$Biol))
criteria_data$Temp <- ifelse(is.na(criteria_data$Temp), NA, normalize_lower(criteria_data$Temp))
criteria_data$TSS <- ifelse(is.na(criteria_data$TSS), NA, normalize_lower(criteria_data$TSS))
criteria_data$BOD <- ifelse(is.na(criteria_data$BOD), NA, normalize_lower(criteria_data$BOD))
criteria_data$PO4 <- ifelse(is.na(criteria_data$PO4), NA, normalize_lower(criteria_data$PO4))
criteria_data$NH3 <- ifelse(is.na(criteria_data$NH3), NA, normalize_lower(criteria_data$NH3))
criteria_data$Chloride <- ifelse(is.na(criteria_data$Chloride), NA, normalize_lower(criteria_data$Chloride))
criteria_data$FC <- ifelse(is.na(criteria_data$FC), NA, normalize_lower(criteria_data$FC))
criteria_data$EC <- ifelse(is.na(criteria_data$EC), NA, normalize_lower(criteria_data$EC))
criteria_data$TDS <- ifelse(is.na(criteria_data$TDS), NA, normalize_lower(criteria_data$TDS))
criteria_data$Salinity <- ifelse(is.na(criteria_data$Salinity), NA, normalize_lower(criteria_data$Salinity))
criteria_data$Alkalinity <- ifelse(is.na(criteria_data$Alkalinity), NA, normalize_lower(criteria_data$Alkainity))



# Adding back the Alternatives
normalized_data <- cbind(data[1], criteria_data)

# Print the normalized data
print(normalized_data)
View(normalized_data)
