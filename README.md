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
...



# k-medoids
...



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

Use `?hc_hierarchical_clustering`, `?spectral_clustering`, `?dbscan` and `?optics` for help pages.  
See `vignettes/` for longer usage examples, including hierarchical clustering.

## Team

Course project by: Oleksandr Pavlov, Vladyslav Kyriienko, Ivan Stroikov, Aleksandra Kosanic, Meryem Keskin.