# Load necessary libraries
library(readr)
library(cluster)
library(factoextra)
library(dplyr)

data <- read.csv("clusterStn1_ssn.csv")

# Perform k-means clustering
set.seed(123)  # For reproducibility
k <- 2  # Number of clusters (wet and dry)
kmeans_result <- kmeans(data[, 1:11], centers = k)  # Exclude the "season" column

# Add cluster labels to the dataset
data$cluster <- as.factor(kmeans_result$cluster)

# Create a function to label clusters as "wet" or "dry" based on the season
label_cluster <- function(data) {
  cluster_labels <- data %>%
    group_by(cluster, season) %>%
    summarise(count = n()) %>%
    group_by(cluster) %>%
    slice_max(count) %>%
    pull(season)
  
  data$cluster_label <- ifelse(data$cluster == 1, cluster_labels[1], cluster_labels[2])
  return(data)
}

# Apply the labeling function to the dataset
labeled_data <- label_cluster(data)

# Print the labeled dataset
print(labeled_data)

# Visualize the clusters using PCA
pca_result <- prcomp(data[, 1:11], scale. = TRUE)
fviz_pca_ind(pca_result, geom = "point", habillage = labeled_data$cluster_label,
             palette = c("#2E9FDF", "#FC4E07"), addEllipses = TRUE)


