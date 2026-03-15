# haufenR

This package implements several clustering methods in R with an emphasis on understandable code written using basic R functions.
The project is designed to provide simple, reproducible examples of common clustering pipelines.
The main functions cover: k-means, k-medoids, spectral clustering, hierarchical clustering, DBSCAN, and OPTICS (plus helper utilities and tests).

## Installation

From the project root:

```r
devtools::load_all()
```

## Quick start

Assume that `X` is a numeric data matrix with observations in rows.

```r

# k-means

set.seed(123)
X <- matrix(rnorm(40), ncol = 2)
res_mn <- k_means(X, K = 3)
table(res_mn$clusters)
plot(X, col = res_mn$clusters, pch = 19, cex = 2, main = "k-means Cluster")
points(res_mn$centers, col = 1:3, pch = 8, cex = 2)



# k-medoids
set.seed(123)
X <- rbind(
  matrix(rnorm(50*2, mean = 0), ncol = 2),
  matrix(rnorm(50*2, mean = 3), ncol = 2),
  matrix(rnorm(50*2, mean = 6), ncol = 2)
)
res <- k_medoids(X, k = 3)
res$medoids
table(res$clusters)
plot(X, col = res$clusters, pch = 19)



# Spectral Clustering

res_sc <- spectral_clustering(
  X,
  k = 2,
  affinity = "rbf",
  sigma = 0.6,
  laplacian = "sym",
  seed = 123
)

head(res_sc$clusters)



# Hierarchical Clustering

set.seed(123)
x <- matrix(rnorm(40), ncol = 2)
res_hc <- hc_hierarchical_clustering(x, k = 3, linkage = "average")
table(res_hc$clusters)
plot(res_hc)



# DBSCAN

x <- rbind(
  c(0.00, 0.00),
  c(0.10, 0.05),
  c(-0.05, 0.10),
  c(0.05, -0.10),
  c(2.00, 0.00),
  c(2.10, 0.05),
  c(1.95, 0.10),
  c(2.05, -0.10),
  c(1.00, 1.50)
)

res_db <- dbscan(x, eps = 0.25, minPts = 3)

print(res_db)
plot(res_db, x)



# OPTICS

x <- rbind(
  c(0.00, 0.00),
  c(0.10, 0.05),
  c(-0.05, 0.10),
  c(0.05, -0.10),
  c(2.00, 0.00),
  c(2.10, 0.05),
  c(1.95, 0.10),
  c(2.05, -0.10),
  c(1.00, 1.50)
)

res_opt <- optics(x, eps = 0.30, minPts = 3)

print(res_opt)
plot(res_opt)

```

## Documentation

Use `?k_means`, `?k_medoids`, `?hc_hierarchical_clustering`, `?spectral_clustering`, `?dbscan` and `?optics` for help pages.  
See `vignettes/` for longer usage examples, including hierarchical clustering.

## Team

Course project by: Oleksandr Pavlov, Vladyslav Kyriienko, Ivan Stroikov, Aleksandra Kosanic, Meryem Keskin.