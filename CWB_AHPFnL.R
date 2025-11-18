setwd("/Users/zal/Desktop/grad school shit/Thesis Data/Laguna Lake/data/R_Scripts_DA")

# Enhanced function to convert weights to an AHP matrix
convert_to_ahp <- function(weights) {
  # Replace zeros with a small value (e.g., 0.1) to avoid division by zero
  weights[weights == 0] <- 0.1
  
  # Create the AHP matrix using outer product to simplify pairwise comparisons
  ahp_matrix <- outer(weights, weights, FUN = "/")
  
  # Ensure the diagonal elements are 1 since each criterion is equally important to itself
  diag(ahp_matrix) <- 1
  
  return(ahp_matrix)
}

# Define the criteria (not directly used in the script but good for reference)
criteria <- c("pH", "Dox", "Temp", "TSS", "BOD", "PO4", "NH3", "FC", "Biol","Turbidity", "Chloride", "NO3", "TC")

# Example raw data with participants P1 to P9
raw_data <- data.frame(
  P1 = c(6, 6, 1, 8, 6, 8, 5, 11, 15, 15, 6, 8, 5),
  P2 = c(0, 6, 0, 3, 5, 5, 0, 3, 25, 25, 25, 0, 3), 
  P3 = c(5, 10, 5, 10, 10, 0, 5, 15, 10, 0, 10, 5, 15),
  P4 = c(2, 2, 2, 2, 7, 8, 8, 15, 15, 8, 8, 8, 8),
  P5 = c(3, 8, 5, 8, 13, 10, 10, 15, 4, 5, 3, 7, 9),
  P6 = c(22, 0, 0, 11, 11, 0, 0, 22, 0, 0, 0, 23, 11),
  P7 = c(0, 3, 0, 35, 1, 2, 3, 5, 39, 3, 1, 3, 5),
  P8 = c(5, 5, 3, 11, 12, 4, 9, 10, 8, 7, 12, 9, 5),
  P9 = c(5, 11, 3, 7, 12, 5, 5, 12, 13, 3, 9, 5, 10),
  P10 = c(8, 10, 1, 9, 2, 2, 6, 13, 11, 8, 10, 8, 12)
)

View(raw_data)

# Normalize the data so that the sum of each participant's weights equals 1
normalized_data <- sweep(raw_data, 2, colSums(raw_data), FUN = "/")

# Create the AHP pairwise comparison matrices for each participant
ahp_matrices <- lapply(normalized_data, convert_to_ahp)

# ahp_matrices will be a list of matrices, one for each participant
print(ahp_matrices)

# Generate AHP matrix for P1
ahp_matrix_P1 <- convert_to_ahp(normalized_data$P1)

# View the AHP matrix for P1
print(ahp_matrix_P1)


# View the AHP matrix for the first participant
print(ahp_matrices[[1]])


# Loop through and print AHP matrices for all participants
for (i in 1:length(ahp_matrices)) {
  cat("AHP Matrix for Participant", names(ahp_matrices)[i], ":\n")
  print(ahp_matrices[[i]])
  cat("\n") # Add a new line for better readability
}


install.packages("openxlsx")
install.packages("ggplot2")


library(openxlsx)

# Function to export AHP matrices to an Excel file
export_ahp_matrices_to_excel <- function(ahp_matrices, filename = "AHP_Matrices_CWBFnL.xlsx") {
  wb <- createWorkbook()
  
  for (i in 1:length(ahp_matrices)) {
    addWorksheet(wb, sheetName = paste("Participant", names(ahp_matrices)[i]))
    writeData(wb, sheet = paste("Participant", names(ahp_matrices)[i]), x = ahp_matrices[[i]])
  }
  
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat("AHP matrices have been saved to", filename)
}

# Export AHP matrices to an Excel file
export_ahp_matrices_to_excel(ahp_matrices)


# Example visualization of the first participant's AHP matrix
heatmap(ahp_matrices[[1]], Rowv = NA, Colv = NA, col = heat.colors(256), scale = "none", margins = c(5,5))

library(ggplot2)

# Convert the matrix to a long format for ggplot2
ahp_matrix_long <- reshape2::melt(ahp_matrices[[1]])

# Plot
ggplot(ahp_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(x = "Criteria", y = "Criteria", title = "AHP Matrix Visualization", fill = "Importance") +
  coord_fixed()




library(knitr)
library(kableExtra)

# Assuming ahp_matrices[[1]] is your AHP matrix for the first participant
ahp_matrix <- ahp_matrices[[1]]

# Convert the matrix to a data frame for better handling
ahp_df <- as.data.frame(ahp_matrix)

# Adding row names as a new column for clarity
ahp_df$Criteria = rownames(ahp_df)
rownames(ahp_df) <- NULL

# Create a basic table using kable and apply some general styling with kableExtra
kable_styled <- kable(ahp_df, "html", booktabs = TRUE, caption = "AHP Matrix") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Print the styled table
kable_styled


# Install and load the required package
if (!require('eigenpr')) {
  install.packages('eigenpr', dependencies=TRUE, repos='http://cran.r-project.org')
}
library(eigenpr)

# A function to calculate the priority vector and consistency ratio
calculate_priority_vector <- function(ahp_matrix) {
  # Calculate eigenvalues and eigenvectors
  eigen_result <- eigen(ahp_matrix)
  
  # Find the index of the largest real eigenvalue
  real_eigenvalues <- Re(eigen_result$values)
  max_index <- which.max(real_eigenvalues)
  
  # Extract the corresponding eigenvector and normalize it
  priority_vector <- eigen_result$vectors[, max_index]
  priority_vector <- priority_vector / sum(priority_vector)
  
  # Calculate the largest eigenvalue
  max_eigenvalue <- real_eigenvalues[max_index]
  
  # Number of criteria
  n <- nrow(ahp_matrix)
  
  # Consistency Index (CI)
  CI <- (max_eigenvalue - n) / (n - 1)
  
  # Random Index (RI) for n = 1 to 15 (for n > 15, it's better to use statistical approximation)
  RI <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51, 1.48, 1.56, 1.57, 1.59)
  if (n <= 15) {
    CR <- CI / RI[n]
  } else {
    CR <- CI / (1.98 * (n-2) / sqrt(n))
  }
  
  # Ensure priority vector has only real numbers
  priority_vector <- Re(priority_vector)
  
  return(list(priority_vector = priority_vector, CR = CR, max_eigenvalue = max_eigenvalue))
}

# Apply the function to each participant's AHP matrix
priority_vectors_and_CRs <- lapply(ahp_matrices, calculate_priority_vector)

# Check the results for participant P1
priority_vectors_and_CRs[['P10']]
