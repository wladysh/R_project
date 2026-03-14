# haufenR

This package implements several clustering methods in R with an emphasis on understandable code written using basic R functions.
The project is designed to provide simple, reproducible examples of common clustering pipelines.
The main functions cover: k-means, k-medoids, spectral clustering, DBSCAN, and OPTICS (plus helper utilities and tests).

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

```r
res_sc <- spectral_clustering(
  X,
  k = 2,
  affinity = "rbf",
  sigma = 0.6,
  laplacian = "sym",
  seed = 123
)

head(res_sc$clusters)
```

# DBSCAN

```r
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
```

# OPTICS

```r
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

```

## Documentation

Use `?dbscan`, `?optics` for help pages. 
See `vignettes/` for longer usage examples.

## Team

Course project by: Oleksandr Pavlov, Vladyslav Kyriienko, Ivan Stroikov, Aleksandra Kosanic, Meryem Keskin.