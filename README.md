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

    ## positions:
    ##      [,1] [,2]
    ## [1,]    2   15
    ## [2,]    1   14
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## positions:
    ##      [,1] [,2]
    ## [1,]    2   15
    ## [2,]    1   14
    ## [3,]    3   15
    ## [4,]    1   15
    ## [5,]    2   14
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## [2,]    2   15
    ## positions:
    ##      [,1] [,2]
    ## [1,]    2   15
    ## [2,]    1   14
    ## [3,]    3   15
    ## [4,]    1   15
    ## [5,]    2   14
    ## [6,]    3   14
    ## [7,]    2   13
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## [2,]    2   15
    ## [3,]    2   14
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## [2,]    2   15
    ## [3,]    2   14
    ## [4,]    3   14
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## [10,]    1   13
    ## [11,]    2   12
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## [2,]    2   15
    ## [3,]    2   14
    ## [4,]    3   14
    ## [5,]    2   13
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## [10,]    1   13
    ## [11,]    2   12
    ## [12,]    1   12
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## [2,]    2   15
    ## [3,]    2   14
    ## [4,]    3   14
    ## [5,]    2   13
    ## [6,]    1   13
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## [10,]    1   13
    ## [11,]    2   12
    ## [12,]    1   12
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## [2,]    2   15
    ## [3,]    2   14
    ## [4,]    3   14
    ## [5,]    2   13
    ## [6,]    1   13
    ## [7,]    1   14
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## [10,]    1   13
    ## [11,]    2   12
    ## [12,]    1   12
    ## [13,]    3   12
    ## [14,]    2   11
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    1   15
    ## [2,]    2   15
    ## [3,]    2   14
    ## [4,]    3   14
    ## [5,]    2   13
    ## [6,]    1   13
    ## [7,]    1   14
    ## [8,]    2   12
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## [10,]    1   13
    ## [11,]    2   12
    ## [12,]    1   12
    ## [13,]    3   12
    ## [14,]    2   11
    ## [15,]    4   12
    ## [16,]    3   11
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    1   15
    ##  [2,]    2   15
    ##  [3,]    2   14
    ##  [4,]    3   14
    ##  [5,]    2   13
    ##  [6,]    1   13
    ##  [7,]    1   14
    ##  [8,]    2   12
    ##  [9,]    3   12
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## [10,]    1   13
    ## [11,]    2   12
    ## [12,]    1   12
    ## [13,]    3   12
    ## [14,]    2   11
    ## [15,]    4   12
    ## [16,]    3   11
    ## [17,]    1   11
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    1   15
    ##  [2,]    2   15
    ##  [3,]    2   14
    ##  [4,]    3   14
    ##  [5,]    2   13
    ##  [6,]    1   13
    ##  [7,]    1   14
    ##  [8,]    2   12
    ##  [9,]    3   12
    ## [10,]    1   12
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    2   15
    ##  [2,]    1   14
    ##  [3,]    3   15
    ##  [4,]    1   15
    ##  [5,]    2   14
    ##  [6,]    3   14
    ##  [7,]    2   13
    ##  [8,]    4   14
    ##  [9,]    3   13
    ## [10,]    1   13
    ## [11,]    2   12
    ## [12,]    1   12
    ## [13,]    3   12
    ## [14,]    2   11
    ## [15,]    4   12
    ## [16,]    3   11
    ## [17,]    1   11
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    1   15
    ##  [2,]    2   15
    ##  [3,]    2   14
    ##  [4,]    3   14
    ##  [5,]    2   13
    ##  [6,]    1   13
    ##  [7,]    1   14
    ##  [8,]    2   12
    ##  [9,]    3   12
    ## [10,]    1   12
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10   14
    ## [2,]    8   14
    ## [3,]    9   15
    ## [4,]    9   13
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10   14
    ## [2,]    8   14
    ## [3,]    9   15
    ## [4,]    9   13
    ## [5,]    9   14
    ## [6,]    7   14
    ## [7,]    8   15
    ## [8,]    8   13
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## positions:
    ##       [,1] [,2]
    ##  [1,]   10   14
    ##  [2,]    8   14
    ##  [3,]    9   15
    ##  [4,]    9   13
    ##  [5,]    9   14
    ##  [6,]    7   14
    ##  [7,]    8   15
    ##  [8,]    8   13
    ##  [9,]   10   13
    ## [10,]    9   12
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## [3,]    9   13
    ## positions:
    ##       [,1] [,2]
    ##  [1,]   10   14
    ##  [2,]    8   14
    ##  [3,]    9   15
    ##  [4,]    9   13
    ##  [5,]    9   14
    ##  [6,]    7   14
    ##  [7,]    8   15
    ##  [8,]    8   13
    ##  [9,]   10   13
    ## [10,]    9   12
    ## [11,]   10   12
    ## [12,]    8   12
    ## [13,]    9   11
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## [3,]    9   13
    ## [4,]    9   12
    ## positions:
    ##       [,1] [,2]
    ##  [1,]   10   14
    ##  [2,]    8   14
    ##  [3,]    9   15
    ##  [4,]    9   13
    ##  [5,]    9   14
    ##  [6,]    7   14
    ##  [7,]    8   15
    ##  [8,]    8   13
    ##  [9,]   10   13
    ## [10,]    9   12
    ## [11,]   10   12
    ## [12,]    8   12
    ## [13,]    9   11
    ## [14,]   10   11
    ## [15,]    8   11
    ## [16,]    9   10
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## [3,]    9   13
    ## [4,]    9   12
    ## [5,]    9   11
    ## positions:
    ##       [,1] [,2]
    ##  [1,]   10   14
    ##  [2,]    8   14
    ##  [3,]    9   15
    ##  [4,]    9   13
    ##  [5,]    9   14
    ##  [6,]    7   14
    ##  [7,]    8   15
    ##  [8,]    8   13
    ##  [9,]   10   13
    ## [10,]    9   12
    ## [11,]   10   12
    ## [12,]    8   12
    ## [13,]    9   11
    ## [14,]   10   11
    ## [15,]    8   11
    ## [16,]    9   10
    ## [17,]   10   15
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## [3,]    9   13
    ## [4,]    9   12
    ## [5,]    9   11
    ## [6,]    9   15
    ## positions:
    ##       [,1] [,2]
    ##  [1,]   10   14
    ##  [2,]    8   14
    ##  [3,]    9   15
    ##  [4,]    9   13
    ##  [5,]    9   14
    ##  [6,]    7   14
    ##  [7,]    8   15
    ##  [8,]    8   13
    ##  [9,]   10   13
    ## [10,]    9   12
    ## [11,]   10   12
    ## [12,]    8   12
    ## [13,]    9   11
    ## [14,]   10   11
    ## [15,]    8   11
    ## [16,]    9   10
    ## [17,]   10   15
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## [3,]    9   13
    ## [4,]    9   12
    ## [5,]    9   11
    ## [6,]    9   15
    ## [7,]   10   12
    ## positions:
    ##       [,1] [,2]
    ##  [1,]   10   14
    ##  [2,]    8   14
    ##  [3,]    9   15
    ##  [4,]    9   13
    ##  [5,]    9   14
    ##  [6,]    7   14
    ##  [7,]    8   15
    ##  [8,]    8   13
    ##  [9,]   10   13
    ## [10,]    9   12
    ## [11,]   10   12
    ## [12,]    8   12
    ## [13,]    9   11
    ## [14,]   10   11
    ## [15,]    8   11
    ## [16,]    9   10
    ## [17,]   10   15
    ## [18,]    7   12
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## [3,]    9   13
    ## [4,]    9   12
    ## [5,]    9   11
    ## [6,]    9   15
    ## [7,]   10   12
    ## [8,]    8   12
    ## positions:
    ##       [,1] [,2]
    ##  [1,]   10   14
    ##  [2,]    8   14
    ##  [3,]    9   15
    ##  [4,]    9   13
    ##  [5,]    9   14
    ##  [6,]    7   14
    ##  [7,]    8   15
    ##  [8,]    8   13
    ##  [9,]   10   13
    ## [10,]    9   12
    ## [11,]   10   12
    ## [12,]    8   12
    ## [13,]    9   11
    ## [14,]   10   11
    ## [15,]    8   11
    ## [16,]    9   10
    ## [17,]   10   15
    ## [18,]    7   12
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    9   14
    ## [2,]    8   14
    ## [3,]    9   13
    ## [4,]    9   12
    ## [5,]    9   11
    ## [6,]    9   15
    ## [7,]   10   12
    ## [8,]    8   12
    ## positions:
    ##      [,1] [,2]
    ## [1,]    7    2
    ## [2,]    5    2
    ## [3,]    6    3
    ## [4,]    6    1
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## positions:
    ##      [,1] [,2]
    ## [1,]    7    2
    ## [2,]    5    2
    ## [3,]    6    3
    ## [4,]    6    1
    ## [5,]    7    1
    ## [6,]    5    1
    ## [7,]    6    2
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## [2,]    6    1
    ## positions:
    ##      [,1] [,2]
    ## [1,]    7    2
    ## [2,]    5    2
    ## [3,]    6    3
    ## [4,]    6    1
    ## [5,]    7    1
    ## [6,]    5    1
    ## [7,]    6    2
    ## [8,]    4    1
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## [2,]    6    1
    ## [3,]    5    1
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## [2,]    6    1
    ## [3,]    5    1
    ## [4,]    7    2
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## [11,]    3    1
    ## [12,]    4    2
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## [2,]    6    1
    ## [3,]    5    1
    ## [4,]    7    2
    ## [5,]    4    1
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## [11,]    3    1
    ## [12,]    4    2
    ## [13,]    8    1
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## [2,]    6    1
    ## [3,]    5    1
    ## [4,]    7    2
    ## [5,]    4    1
    ## [6,]    7    1
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## [11,]    3    1
    ## [12,]    4    2
    ## [13,]    8    1
    ## [14,]    9    1
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## [2,]    6    1
    ## [3,]    5    1
    ## [4,]    7    2
    ## [5,]    4    1
    ## [6,]    7    1
    ## [7,]    8    1
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## [11,]    3    1
    ## [12,]    4    2
    ## [13,]    8    1
    ## [14,]    9    1
    ## [15,]    8    3
    ## [16,]    7    4
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    6    2
    ## [2,]    6    1
    ## [3,]    5    1
    ## [4,]    7    2
    ## [5,]    4    1
    ## [6,]    7    1
    ## [7,]    8    1
    ## [8,]    7    3
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## [11,]    3    1
    ## [12,]    4    2
    ## [13,]    8    1
    ## [14,]    9    1
    ## [15,]    8    3
    ## [16,]    7    4
    ## [17,]    9    3
    ## [18,]    8    4
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    6    2
    ##  [2,]    6    1
    ##  [3,]    5    1
    ##  [4,]    7    2
    ##  [5,]    4    1
    ##  [6,]    7    1
    ##  [7,]    8    1
    ##  [8,]    7    3
    ##  [9,]    8    3
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## [11,]    3    1
    ## [12,]    4    2
    ## [13,]    8    1
    ## [14,]    9    1
    ## [15,]    8    3
    ## [16,]    7    4
    ## [17,]    9    3
    ## [18,]    8    4
    ## [19,]    6    4
    ## [20,]    7    5
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    6    2
    ##  [2,]    6    1
    ##  [3,]    5    1
    ##  [4,]    7    2
    ##  [5,]    4    1
    ##  [6,]    7    1
    ##  [7,]    8    1
    ##  [8,]    7    3
    ##  [9,]    8    3
    ## [10,]    7    4
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    7    2
    ##  [2,]    5    2
    ##  [3,]    6    3
    ##  [4,]    6    1
    ##  [5,]    7    1
    ##  [6,]    5    1
    ##  [7,]    6    2
    ##  [8,]    4    1
    ##  [9,]    8    2
    ## [10,]    7    3
    ## [11,]    3    1
    ## [12,]    4    2
    ## [13,]    8    1
    ## [14,]    9    1
    ## [15,]    8    3
    ## [16,]    7    4
    ## [17,]    9    3
    ## [18,]    8    4
    ## [19,]    6    4
    ## [20,]    7    5
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    6    2
    ##  [2,]    6    1
    ##  [3,]    5    1
    ##  [4,]    7    2
    ##  [5,]    4    1
    ##  [6,]    7    1
    ##  [7,]    8    1
    ##  [8,]    7    3
    ##  [9,]    8    3
    ## [10,]    7    4
    ## positions:
    ##      [,1] [,2]
    ## [1,]    8    6
    ## [2,]    6    6
    ## [3,]    7    7
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## positions:
    ##      [,1] [,2]
    ## [1,]    8    6
    ## [2,]    6    6
    ## [3,]    7    7
    ## [4,]    8    7
    ## [5,]    6    7
    ## [6,]    7    8
    ## [7,]    7    6
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## [2,]    7    7
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    8    7
    ##  [5,]    6    7
    ##  [6,]    7    8
    ##  [7,]    7    6
    ##  [8,]    9    6
    ##  [9,]    8    5
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## [2,]    7    7
    ## [3,]    8    6
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    8    7
    ##  [5,]    6    7
    ##  [6,]    7    8
    ##  [7,]    7    6
    ##  [8,]    9    6
    ##  [9,]    8    5
    ## [10,]   10    6
    ## [11,]    9    7
    ## [12,]    9    5
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## [2,]    7    7
    ## [3,]    8    6
    ## [4,]    9    6
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    8    7
    ##  [5,]    6    7
    ##  [6,]    7    8
    ##  [7,]    7    6
    ##  [8,]    9    6
    ##  [9,]    8    5
    ## [10,]   10    6
    ## [11,]    9    7
    ## [12,]    9    5
    ## [13,]    5    6
    ## [14,]    6    5
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## [2,]    7    7
    ## [3,]    8    6
    ## [4,]    9    6
    ## [5,]    6    6
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    8    7
    ##  [5,]    6    7
    ##  [6,]    7    8
    ##  [7,]    7    6
    ##  [8,]    9    6
    ##  [9,]    8    5
    ## [10,]   10    6
    ## [11,]    9    7
    ## [12,]    9    5
    ## [13,]    5    6
    ## [14,]    6    5
    ## [15,]    5    7
    ## [16,]    6    8
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## [2,]    7    7
    ## [3,]    8    6
    ## [4,]    9    6
    ## [5,]    6    6
    ## [6,]    6    7
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    8    7
    ##  [5,]    6    7
    ##  [6,]    7    8
    ##  [7,]    7    6
    ##  [8,]    9    6
    ##  [9,]    8    5
    ## [10,]   10    6
    ## [11,]    9    7
    ## [12,]    9    5
    ## [13,]    5    6
    ## [14,]    6    5
    ## [15,]    5    7
    ## [16,]    6    8
    ## [17,]    5    8
    ## [18,]    6    9
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## [2,]    7    7
    ## [3,]    8    6
    ## [4,]    9    6
    ## [5,]    6    6
    ## [6,]    6    7
    ## [7,]    6    8
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    8    7
    ##  [5,]    6    7
    ##  [6,]    7    8
    ##  [7,]    7    6
    ##  [8,]    9    6
    ##  [9,]    8    5
    ## [10,]   10    6
    ## [11,]    9    7
    ## [12,]    9    5
    ## [13,]    5    6
    ## [14,]    6    5
    ## [15,]    5    7
    ## [16,]    6    8
    ## [17,]    5    8
    ## [18,]    6    9
    ## [19,]    4    6
    ## [20,]    5    5
    ## exclude:
    ##      [,1] [,2]
    ## [1,]    7    6
    ## [2,]    7    7
    ## [3,]    8    6
    ## [4,]    9    6
    ## [5,]    6    6
    ## [6,]    6    7
    ## [7,]    6    8
    ## [8,]    5    6
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    8    7
    ##  [5,]    6    7
    ##  [6,]    7    8
    ##  [7,]    7    6
    ##  [8,]    9    6
    ##  [9,]    8    5
    ## [10,]   10    6
    ## [11,]    9    7
    ## [12,]    9    5
    ## [13,]    5    6
    ## [14,]    6    5
    ## [15,]    5    7
    ## [16,]    6    8
    ## [17,]    5    8
    ## [18,]    6    9
    ## [19,]    4    6
    ## [20,]    5    5
    ## [21,]   10    7
    ## [22,]    9    8
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    7    6
    ##  [2,]    7    7
    ##  [3,]    8    6
    ##  [4,]    9    6
    ##  [5,]    6    6
    ##  [6,]    6    7
    ##  [7,]    6    8
    ##  [8,]    5    6
    ##  [9,]    9    7
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    8    6
    ##  [2,]    6    6
    ##  [3,]    7    7
    ##  [4,]    7    5
    ##  [5,]    8    7
    ##  [6,]    6    7
    ##  [7,]    7    8
    ##  [8,]    7    6
    ##  [9,]    9    6
    ## [10,]    8    5
    ## [11,]   10    6
    ## [12,]    9    7
    ## [13,]    9    5
    ## [14,]    5    6
    ## [15,]    6    5
    ## [16,]    5    7
    ## [17,]    6    8
    ## [18,]    5    8
    ## [19,]    6    9
    ## [20,]    4    6
    ## [21,]    5    5
    ## [22,]   10    7
    ## [23,]    9    8
    ## exclude:
    ##       [,1] [,2]
    ##  [1,]    7    6
    ##  [2,]    7    7
    ##  [3,]    8    6
    ##  [4,]    9    6
    ##  [5,]    6    6
    ##  [6,]    6    7
    ##  [7,]    6    8
    ##  [8,]    5    6
    ##  [9,]    9    7
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10    2
    ## exclude:
    ##      [,1] [,2]
    ## [1,]   10    1
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10    2
    ## [2,]    9    2
    ## [3,]   10    3
    ## [4,]   10    1
    ## exclude:
    ##      [,1] [,2]
    ## [1,]   10    1
    ## [2,]   10    2
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10    2
    ## [2,]    9    2
    ## [3,]   10    3
    ## [4,]   10    1
    ## [5,]   10    4
    ## exclude:
    ##      [,1] [,2]
    ## [1,]   10    1
    ## [2,]   10    2
    ## [3,]   10    3
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10    2
    ## [2,]    9    2
    ## [3,]   10    3
    ## [4,]   10    1
    ## [5,]   10    4
    ## exclude:
    ##      [,1] [,2]
    ## [1,]   10    1
    ## [2,]   10    2
    ## [3,]   10    3
    ## [4,]    9    2
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10    2
    ## [2,]    9    2
    ## [3,]   10    3
    ## [4,]   10    1
    ## [5,]   10    4
    ## [6,]    9    4
    ## [7,]   10    5
    ## exclude:
    ##      [,1] [,2]
    ## [1,]   10    1
    ## [2,]   10    2
    ## [3,]   10    3
    ## [4,]    9    2
    ## [5,]   10    4
    ## positions:
    ##      [,1] [,2]
    ## [1,]   10    2
    ## [2,]    9    2
    ## [3,]   10    3
    ## [4,]   10    1
    ## [5,]   10    4
    ## [6,]    9    4
    ## [7,]   10    5
    ## exclude:
    ##      [,1] [,2]
    ## [1,]   10    1
    ## [2,]   10    2
    ## [3,]   10    3
    ## [4,]    9    2
    ## [5,]   10    4
    ## [6,]   10    5
    ## positions:
    ##       [,1] [,2]
    ##  [1,]    9    1
    ##  [2,]   10    2
    ##  [3,]    9    2
    ##  [4,]   10    3
    ##  [5,]   10    1
    ##  [6,]    9    3
    ##  [7,]   10    4
    ##  [8,]    8    2
    ##  [9,]    9    4
    ## [10,]   10    5
    ## [11,]    9    5
    ## [12,]   10    6
    ## exclude:
    ##      [,1] [,2]
    ## [1,]   10    1
    ## [2,]   10    2
    ## [3,]   10    3
    ## [4,]    9    2
    ## [5,]   10    4
    ## [6,]   10    5

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)
