Installation
------------

``` r
# install.packages("remotes")
remotes::install_github("hsonne/findblobs")
```

Plot an Integer Matrix
----------------------

``` r
findblobs::plot_integer_matrix(matrix(nrow = 5, byrow = TRUE, c(
  2, 2, 2, 2, 2,
  2, 0, 1, 0, 2,
  2, 1, 1, 1, 2,
  2, 0, 1, 0, 2,
  2, 2, 2, 2, 2
)))
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

Create Random Blobs
-------------------

``` r
blob_matrix <- findblobs::place_random_blobs(
  n_blobs = 5, min_fields = 3, max_fields = 10
)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)
