# Example 2: Comparing Different Linkage Methods

# Load necessary libraries
library(ggplot2)
library(cluster)

# Sample data
set.seed(42)
data <- matrix(rnorm(100), nrow=10)

# Perform hierarchical clustering using different linkage methods
hc_single <- hclust(dist(data), method = 'single')
hc_complete <- hclust(dist(data), method = 'complete')
hc_ward <- hclust(dist(data), method = 'ward.D')

# Create dendrograms
par(mfrow=c(1,3))
plot(hc_single, main='Single Linkage')
plot(hc_complete, main='Complete Linkage')
plot(hc_ward, main='Ward Linkage')

# Visualize clusters for each method
clusters_single <- cutree(hc_single, k=3)
clusters_complete <- cutree(hc_complete, k=3)
clusters_ward <- cutree(hc_ward, k=3)

# Function to visualize clusters
plot_clusters <- function(data, clusters, title) {
  df <- data.frame(data, cluster = as.factor(clusters))
  ggplot(df, aes(x=X1, y=X2, color=cluster)) +
    geom_point(size=3) +
    ggtitle(title) +
    theme_minimal()
}

# Plot clusters for each linkage method
par(mfrow=c(1,3))
plot_clusters(data, clusters_single, 'Single Linkage Clusters')
plot_clusters(data, clusters_complete, 'Complete Linkage Clusters')
plot_clusters(data, clusters_ward, 'Ward Linkage Clusters')