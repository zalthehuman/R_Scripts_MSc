setwd('/Users/zal/Dropbox/My PC (DESKTOP-PGE24LL)/Desktop/grad school shit/Thesis Data/Laguna Lake/data/R_Scripts_DA')

setwd("/Users/zal/Desktop/grad school shit/Thesis Data/Laguna Lake/data/R_Scripts_DA")

# Load the packages
library(missMDA)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Load the dataset
mydata <- read.csv("ImputedStn1.csv", header = TRUE)
str(mydata)

# Load the dataset
mydata <- read.csv("imputedStn1_comp.csv", header = TRUE)
str(mydata)



# Convert all other variables to numeric format
mydata <- sapply(mydata, as.numeric)

#log transform the data
mydata <- log(mydata)
View(mydata)


# Scale the data
mydata_scaled <- scale(mydata) # Exclude first column with row names


#=========================================================================================
# Impute missing values using missMDA
imputed_data <- imputePCA(mydata_scaled[, -1], method = "Regularized", numcp = 5, threshold = 1e-4)
View(imputed_data)

write.csv(imputed_data, file = "imputedStn1.csv", row.names = FALSE)

#=========================================================================================

# Perform PCA
pca <- PCA(mydata[,-2], graph = FALSE)
pca


summary(pca)

str(pca)
eig.val <- get_eigenvalue(pca)
eig.val


# Extract the results
pca_var <- get_pca_var(pca)
pca_ind <- get_pca_ind(pca)

# Plot the results
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             col.names = names(mydata)[-1] # Add parameter names
)

fviz_pca_ind(pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             col.var = names(mydata)[-1], # Add parameter names
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(pca)


# Plot variables with PC3 and PC4
fviz_pca_var(pca, axes = c(3, 4), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Plot individuals with PC3 and PC4
fviz_pca_ind(pca, axes = c(3, 4), col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



fviz_pca_ind(pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800", 
             col.var = names(mydata)[-1], # Add parameter names
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

pca_var
head(pca_var$coord)

head(pca_var$coord, 4)
pca_var$contrib

library(corrplot)

#> corrplot 0.84 loaded
corrplot(pca_var$cos2, is.corr = FALSE)
corrplot(pca_var$contrib, is.corr = FALSE)
corrplot(pca_var$cor, is.corr = FALSE)

res.desc <- dimdesc(pca, axes = c(1,2), proba = 0.05)
res.desc$Dim.1



res.pca <- PCA(mydata, graph = FALSE)
head(res.pca)

eigen(pca)
#=========================================================================================

## Principal Component Analysis Results for variables
##  ===================================================
##   Name       Description                                    
## 1 "$coord"   "Coordinates for the variables"                
## 2 "$cor"     "Correlations between variables and dimensions"
## 3 "$cos2"    "Cos2 for the variables"                       
## 4 "$contrib" "contributions of the variables"


# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 10)


# Perform k-means clustering on the first 2 principal components
set.seed(123)
kmeans_pca <- kmeans(pca$ind$coord[,1:2], centers = 3, nstart = 25)
kmeans_pca

# Plot the results
fviz_cluster(kmeans_pca, data = pca$ind$coord[,1:2],
             geom = "point", pointsize = 2,
             ellipse.type = "t", ggtheme = theme_minimal())

# Add cluster assignments to the PCA results
pca_ind$cluster <- kmeans_pca$cluster

# Plot the PCA results colored by cluster assignment
fviz_cluster(kmeans_pca, data = pca_ind$coord[,1:2], 
             geom = "point", pointsize = 2, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal())

fviz_contrib(kmeans_pca$cluster, pca_var, axes = c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             col.names = names(mydata)[-1] # Add parameter names
)




pca$var$contrib
pca$call$row.w

res.hcpc <- HCPC(pca, nb.clust = 3, consol = TRUE, min= 3, max = 10, graph = TRUE)
#dividing the clusters into 3 in order to see which var go together

res.hcpc$desc.axes$quanti
res.hcpc$desc.ind$para


# Description by variables
res.hcpc$desc.var$test.chi2

res.hcpc$desc.var$
  
summary(res.hcpc)

res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category

help("plot.HCPC")
plot.HCPC(res.hcpc, choice = "3D.map", ind.names = F)

ggplot(pca, aes(Dim.1, Dim.2))+ geom_point(aes(col = clust)) + theme_bw()

#see which params are in each clusters 
cluster = data.frame(res.hcpc$data.clust)
library(dplyr)
head(cluster)

cluster

cluster %>% group_by(clust) %>% summarize(Total_Params = n ())

cluster = cluster %>% arrange(by = clust)
cluster = [,c('Params', 'clust')]

plot.HCPC(res.hcpc, axes = 1:2)


df = data.frame(res.hcpc$call$X)
head = df
df

write.csv(df, file = "clusterStn1_10.16.csv", row.names = FALSE)

write.csv(cluster, file = "clusterStn1.csv", row.names = FALSE)
plot(cluster)
