setwd('/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/grad school shit/Thesis Data/Laguna Lake/data/R_Scripts_DA/')

# Load the water quality data into R as a data frame
water_data <- read.csv("ImputedStn1_ND.csv")

library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)

# Check the structure of your data
str(water_data_subset)
sapply(water_data, function(x) sum(is.na(x)))

# Subset the data to include only the relevant variables
water_data_subset <- water_data[, c("pH", "Dox", "Temp", "TSS", "BOD", "PO4", "NH3", "FC", "Chloride", "NO3", "TC")]


water_data_subset$NO3 = as.numeric(water_data_subset$NO3)
water_data_subset$BOD = as.numeric(water_data_subset$BOD)
water_data_subset$TC = as.numeric(water_data_subset$TC)
water_data_subset$FC = as.numeric(water_data_subset$FC)
water_data_subset$Chloride = as.numeric(water_data_subset$Chloride)

# Impute missing values using the median
water_data_subset_imputed <- water_data_subset %>%
  mutate(across(.cols = everything(), .fns = ~ifelse(is.na(.), median(., na.rm = TRUE), .))) 

# Check for missing values
sum(is.na(water_data_subset))
sum(is.na(water_data_subset_imputed))

# Check summary statistics for each parameter
write.csv(summary(water_data_subset_imputed[, 1:10]), file = '/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/summary.csv')

# Check distribution of each parameter with histograms
ggplot(water_data_subset_imputed, aes(x = pH)) + 
  geom_histogram(bins = 12, fill = "royalblue1", color = "white") + 
  labs(x = "pH", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = NO3)) + 
  geom_histogram(bins = 6, fill = "steelblue2", color = "white") + 
  labs(x = "NO3", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = BOD)) + 
  geom_histogram(bins = 8, fill = "lightgreen", color = "white") + 
  labs(x = "BOD", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = Dox)) + 
  geom_histogram(bins = 12, fill = "salmon", color = "white") + 
  labs(x = "Dox", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = Temp)) + 
  geom_histogram(bins = 9, fill = "orange", color = "white") + 
  labs(x = "Temp", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = TSS)) + 
  geom_histogram(bins = 8, fill = "pink", color = "white") + 
  labs(x = "TSS", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = Chloride)) + 
  geom_histogram(bins = 9, fill = "violet", color = "white") + 
  labs(x = "Chloride", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = TC)) + 
  geom_histogram(bins = 12, fill = "hotpink", color = "white") + 
  labs(x = "Total Coliforms", y = "Frequency")

ggplot(water_data_subset_imputed, aes(x = FC)) + 
  geom_histogram(bins = 8, fill = "yellow", color = "white") + 
  labs(x = "Fecal Coliforms", y = "Frequency")


# Log transform the data
water_data_subset_imputed_log <- water_data_subset_imputed %>%
  mutate(across(.cols = pH:TC, .fns = log))

# Check distribution of each parameter with histograms
ggplot(water_data_subset_imputed_log, aes(x = pH)) + 
  geom_histogram(bins = 12, fill = "royalblue1", color = "white") + 
  labs(x = "pH", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = NO3)) + 
  geom_histogram(bins = 6, fill = "steelblue2", color = "white") + 
  labs(x = "NO3", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = BOD)) + 
  geom_histogram(bins = 8, fill = "lightgreen", color = "white") + 
  labs(x = "BOD", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = Dox)) + 
  geom_histogram(bins = 12, fill = "salmon", color = "white") + 
  labs(x = "Dox", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = Temp)) + 
  geom_histogram(bins = 9, fill = "orange", color = "white") + 
  labs(x = "Temp", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = TSS)) + 
  geom_histogram(bins = 8, fill = "pink", color = "white") + 
  labs(x = "TSS", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = Chloride)) + 
  geom_histogram(bins = 9, fill = "violet", color = "white") + 
  labs(x = "Chloride", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = TC)) + 
  geom_histogram(bins = 12, fill = "hotpink", color = "white") + 
  labs(x = "Total Coliforms", y = "Frequency")

ggplot(water_data_subset_imputed_log, aes(x = FC)) + 
  geom_histogram(bins = 8, fill = "yellow", color = "white") + 
  labs(x = "Fecal Coliforms", y = "Frequency")

# Log transform the data
water_data_subset_imputed_log <- water_data_subset_imputed %>%
  mutate(across(.cols = pH:TC, .fns = log))

# Scale and center the data
water_data_subset_imputed_log_scaled <- scale(water_data_subset_imputed_log[, -1], center = TRUE, scale = TRUE)


# Perform PCA on the water quality data
pca_result <- prcomp(water_data, scale = TRUE)

write.csv(water_data_subset_imputed, file = "/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/grad school shit/Thesis Data/Laguna Lake/data/ImputedWQS1_ND.csv")


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


?screeplot
?cor


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



