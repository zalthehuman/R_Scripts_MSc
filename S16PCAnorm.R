setwd('/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/grad school shit/Thesis Data/Laguna Lake/data/R_Scripts_DA/')

# Load the water quality data into R as a data frame
water_data <- read.csv("imputedStn16.csv")

library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)

str(water_data)
water_data <- sapply(water_data, as.numeric)

# Check the structure of your data
str(water_data_subset)
sapply(water_data, function(x) sum(is.na(x)))

# Subset the data to include only the relevant variables
water_data_subset <- water_data[, c("pH", "Dox", "Temp", "TSS", "BOD", "PO4", "NH3", "FC", "Chloride", "NO3", "TC", "Biological")]


# Check for missing values
sum(is.na(water_data))
sum(is.na(water_data_subset_imputed))


# Check distribution of each parameter with histograms
ggplot(water_data, aes(x = pH)) + 
  geom_histogram(bins = 12, fill = "royalblue1", color = "white") + 
  labs(x = "pH", y = "Frequency")

ggplot(water_data, aes(x = NO3)) + 
  geom_histogram(bins = 12, fill = "steelblue2", color = "white") + 
  labs(x = "NO3", y = "Frequency")

ggplot(water_data, aes(x = BOD)) + 
  geom_histogram(bins = 8, fill = "lightgreen", color = "white") + 
  labs(x = "BOD", y = "Frequency")

ggplot(water_data, aes(x = Dox)) + 
  geom_histogram(bins = 12, fill = "salmon", color = "white") + 
  labs(x = "Dox", y = "Frequency")

ggplot(water_data, aes(x = Temp)) + 
  geom_histogram(bins = 9, fill = "orange", color = "white") + 
  labs(x = "Temp", y = "Frequency")

ggplot(water_data, aes(x = TSS)) + 
  geom_histogram(bins = 8, fill = "pink", color = "white") + 
  labs(x = "TSS", y = "Frequency")

ggplot(water_data, aes(x = Chloride)) + 
  geom_histogram(bins = 9, fill = "violet", color = "white") + 
  labs(x = "Chloride", y = "Frequency")

ggplot(water_data, aes(x = TC)) + 
  geom_histogram(bins = 12, fill = "hotpink", color = "white") + 
  labs(x = "Total Coliforms", y = "Frequency")

ggplot(water_data, aes(x = FC)) + 
  geom_histogram(bins = 9, fill = "yellow", color = "white") + 
  labs(x = "Fecal Coliforms", y = "Frequency")

ggplot(water_data, aes(x = Biological)) + 
  geom_histogram(bins = 9, fill = "brown", color = "white") + 
  labs(x = "Biological", y = "Frequency")



# Perform PCA on the water quality data
pca_result <- prcomp(water_data, scale = TRUE)

write.csv(water_data_subset_imputed, file = "/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/grad school shit/Thesis Data/Laguna Lake/data/ImputedWQS16_ND.csv")


pca_result

# Examine the results of the PCA
summary(pca_result)
Write_csv(pca_result$rotation, file = "/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/grad school shit/Thesis Data/Laguna Lake/data/newfile.csv")

# Plot the scree plot to show the variance explained by each principal component
screeplot(pca_result, type = "lines", main = "Scree Plot")

# Create a biplot to visualize the relationship between the variables and the principal components
biplot(pca_result, cex = 0.8)

# Create a plot of the scores for the first two principal components
scores <- as.data.frame(pca_result$x)  # Convert the scores matrix to a data frame
ggplot(scores, aes(x = PC1, y = PC2, color = )) + geom_point() + labs(title = "Scores Plot", x = "PC1", y = "PC2", color = "Quarter")


# Create a plot of the scores for the first two principal components
ggplot(scores, aes(x = PC1, y = PC2) +
         geom_point() +
         labs(title = "Scores Plot", x = "PC1", y = "PC2")
       
       
cor(x= water_data, y= NULL, method = "spearman")
cor(x= water_data, y= NULL, method = "pearson")
       
       
# Plot the correlation matrix as a heatmap
ggplot(water_data_subset_imputed_scaled = reshape2::melt(correlation_matrix)) +
geom_tile(aes(x = Var1, y = Var2, fill = value)) +
scale_fill_gradient2(low = "#377eb8", mid = "white", high = "#e41a1c", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
ggtitle("Correlation Matrix")
       
       
# Create a correlation matrix of the original variables and the principal components
cor_matrix <- cor(water_data, pca_result$x)
ggcorrplot(cor_matrix, type = "lower", outline.color = "white", colors = c("#6D9EC1", "white", "#E46726"))
       
library(ggcorrplot)
       
       