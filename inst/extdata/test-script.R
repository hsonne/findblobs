# Create random blobs ----------------------------------------------------------
if (FALSE)
{
  empty_matrix <- matrix(0L, nrow = 6, ncol = 10)

  set.seed(42)
  # Tetris
  m <- findblobs:::place_random_blobs(m = empty_matrix, n_blobs = 10,
                                      min_fields = 4)

  # Pentomino
  m <- findblobs:::place_random_blobs(m = empty_matrix, n_blobs = 10,
                                      min_fields = 5)
}

# Test the recognition of blobs ------------------------------------------------
if (FALSE)
{
  M1 <- matrix(byrow = TRUE, ncol = 3, c(
    "", "", "x",
    "", "x", "x",
    "x", "x", "x"
  ))

  M2 <- matrix(ncol = 4, byrow = TRUE, c(
    F, T, T, F,
    T, T, F, F,
    T, F, T, T,
    F, T, T, F
  ))

  par(mfrow = c(10, 10), mar = c(0, 0, 0, 0))

  for (i in 1:100) {
    (content <- matrix(sample(c("x", ""), size = 25, replace = TRUE), ncol = 5))
    plot_integer_matrix(x <- get_blobs(content == "x"))
  }

  matrix_dim <- c(200, 200)

  for (i in 1:1) {

    message("Test run ", i)

    M <- "x" == matrix(
      nrow = matrix_dim[1],
      sample(c("_", "x"), prod(matrix_dim), replace = TRUE)
    )

    blobs <- get_blobs(M)
  }

  plot_integer_matrix(blobs, 30)
}

if (FALSE)
{
  bak <- groups
  head(bak)
  (groups <- bak[c(1,2,5)])
  (group <- bak[[4]])
  (new_groups <- bak[c(4,6)])

  merge_one_into(groups, group)
  merge_n_into(new_groups, groups)
  merge_two(bak[[1]], bak[[3]])
  merge_groups(bak[c(1,3)])
  merged_groups <- merge_groups(bak)
  any(sapply(merged_groups[-1], function(x) any(merged_groups[[1]] %in% x)))
}

if (FALSE)
{
  x <- get_column_blobs(M)
  y <- t(get_column_blobs(t(M), offset = max(column_blobs)))
  is_set <- x > 0
  groups <- unique(remove_singles(unname(split(x[is_set], y[is_set]))))

  g2 <- order_by_first(merge_groups_2(groups))
  g3 <- order_by_first(merge_groups_3(groups))
  g4 <- order_by_first(merge_groups_4(groups))
  g5 <- order_by_first(merge_groups_5(groups))

  identical(g2, g5)
  compare_group_lists(g2, g5)
}
