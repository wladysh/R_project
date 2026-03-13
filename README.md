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

# spectral clustering
res_sc <- spectral_clustering(
  X,
  k = 2,
  affinity = "rbf",
  sigma = 0.6,
  laplacian = "sym",
  seed = 123
)

head(res_sc$clusters)

# DBSCAN
...

# OPTICS
...

```

## Documentation

Use `?<>`, `?<>`, ... for help pages.  
See `vignettes/` for longer usage examples.

## Team

Course project by: Oleksandr Pavlov, Vladyslav Kyriienko, Ivan Stroikov, Aleksandra Kosanic, Meryem Keskin.