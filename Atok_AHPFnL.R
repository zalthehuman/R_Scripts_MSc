#Atok Data

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
criteria <- c("pH", "Dox", "Temp", "TSS", "BOD", "PO4", "NH3", "FC", "Chloride", "NO3")

# Example raw data with participants P1 to P9
raw_data <- data.frame(
  P1 = c(8, 8, 6, 15, 8, 8, 6, 20, 10, 11),
  P2 = c(0, 0, 0, 5, 25, 25, 5, 5, 30, 5), 
  P3 = c(0, 20, 20, 20, 10, 0, 0, 0, 0, 30),
  P4 = c(6, 6, 6, 6, 8, 8, 15, 20, 10, 15),
  P5 = c(5, 10, 5, 15, 15, 8, 13, 11, 5, 13),
  P6 = c(10, 0, 10, 20, 0, 0, 10, 20, 10, 20),
  P7 = c(5, 10, 0, 5, 5, 5, 10, 40, 10, 10),
  P8 = c(13, 10, 12, 20, 6, 15, 10, 5, 1, 8),
  P9 = c(13, 6, 13, 17, 10, 5, 7, 12, 10, 7),
  P9 = c(13, 6, 13, 17, 10, 5, 7, 12, 10, 7),
  P10 = c(9, 12, 11, 12, 5, 4, 8, 12, 15, 12)
)

View(raw_data)

# Normalize the data so that the sum of each participant's weights equals 1
normalized_data <- sweep(raw_data, 2, colSums(raw_data), FUN = "/")

# Create the AHP pairwise comparison matrices for each participant
ahp_matrices <- lapply(normalized_data, convert_to_ahp)

# ahp_matrices will be a list of matrices, one for each participant

# Generate AHP matrix for P1
ahp_matrix_P1 <- convert_to_ahp(normalized_data$P1)

# View the AHP matrix for P1
print(ahp_matrix_P1)


# View the AHP matrix for the first participant
print(ahp_matrices)


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
export_ahp_matrices_to_excel <- function(ahp_matrices, filename = "AHP_Matrices_AtokFNL.xlsx") {
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



if (!require('remotes')) {
  install.packages('remotes', repos='http://cran.r-project.org')
}
remotes::install_github("davidski/eigenpr")
