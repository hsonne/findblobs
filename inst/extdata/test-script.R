# Create random blobs ----------------------------------------------------------
if (FALSE)
{
  empty_matrix <- matrix(0L, nrow = 6, ncol = 10)

  set.seed(42)
  # Tetris
  m <- findblobs::place_random_blobs(
    m = empty_matrix, n_blobs = 10, min_fields = 4
  )

  findblobs::plot_integer_matrix(findblobs:::get_blobs(m != 0))
  m

  # Pentomino
  m <- findblobs:::place_random_blobs(
    m = empty_matrix, n_blobs = 5, min_fields = 5
  )
}

# Test the recognition of blobs ------------------------------------------------
if (FALSE)
{
  # Define test matrices
  (M1 <- matrix(byrow = TRUE, ncol = 3, c(
    "_", "_", "x",
    "_", "x", "x",
    "x", "x", "x"
  )))

  (M2 <- matrix(ncol = 4, byrow = TRUE, c(
    "_", "x", "x", "_",
    "x", "x", "_", "_",
    "x", "_", "x", "x",
    "_", "x", "x", "_"
  )))

  # Define output matrix
  mfrow <- c(7, 7)
  par(mfrow = mfrow, mar = c(0, 0, 0, 0))

  # Create 100-times blobs from random matrices
  for (i in seq_len(prod(mfrow))) {
    blob_matrix <- findblobs:::get_blobs(
      M = (findblobs::random_matrix(c(5, 5)) == "x")
    )
    findblobs:::plot_integer_matrix(blob_matrix)
  }

  for (i in 1:3) {
    message("Test run ", i)
    blobs <- findblobs:::get_blobs(
      M = (findblobs::random_matrix(c(250, 250)) == "x"),
      methods = 6:7
    )
  }

  plot_integer_matrix(blobs)
}

# Test low level functions -----------------------------------------------------
if (FALSE)
{
  M <- findblobs:::place_random_blobs(
    matrix(0L, nrow = 50, ncol = 80),
    n_blobs = 10, min_fields = 30, max_fields = 50
  )

  m <- (M > 0)

  x <- findblobs:::get_column_blobs(m)
  y <- t(findblobs:::get_column_blobs(t(m), offset = max(x)))

  is_set <- x > 0
  all_groups <- unique(findblobs:::remove_singles(unname(split(x[is_set], y[is_set]))))

  head(all_groups)

  (groups <- all_groups[c(1, 2, 5)])
  (group <- all_groups[[4]])
  (new_groups <- all_groups[c(4, 6)])

  findblobs:::merge_one_into(groups, group)
  findblobs:::merge_n_into(new_groups, groups)
  findblobs:::merge_two(all_groups[[1]], all_groups[[2]])
  findblobs:::merge_groups(all_groups[c(1, 3)])

  merged_groups <- findblobs:::merge_groups(all_groups)

  any(sapply(merged_groups[-1], function(x) any(merged_groups[[1]] %in% x)))
}

if (FALSE)
{
  groups <- all_groups

  g4 <- findblobs:::order_by_first(findblobs:::merge_groups_4(groups))
  g5 <- findblobs:::order_by_first(findblobs:::merge_groups_5(groups))
  g6 <- findblobs:::order_by_first(findblobs:::merge_groups_6(groups))
  g7 <- findblobs:::order_by_first(findblobs:::merge_groups_7(groups))

  identical(g4, g5)
  identical(g4, g6)
  identical(g4, g7)

  findblobs:::compare_group_lists(g4, g5)
}
