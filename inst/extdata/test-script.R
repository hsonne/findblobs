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
      M = (findblobs::random_matrix(c(150, 150)) == "x"),
      methods = 6:9
    )
  }

  plot_integer_matrix(blobs)
}

# Test low level functions -----------------------------------------------------
if (FALSE)
{
  M <- findblobs:::place_random_blobs(
    matrix(0L, nrow = 500, ncol = 800),
    n_blobs = 10, min_fields = 500, max_fields = 1000
  )

  M <- findblobs::random_matrix(c(10, 10))

  m <- (M > 0)

  x <- findblobs:::get_column_blobs(m)
  y <- t(findblobs:::get_column_blobs(t(m), offset = max(x)))

  is_set <- x > 0

  all_groups <- unname(split(x[is_set], y[is_set]))
  all_groups <- unique(findblobs:::remove_singles(all_groups))

  findblobs::plot_integer_matrix(findblobs:::get_blobs(m))

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

# Test merge_groups_*() --------------------------------------------------------
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

# Test merge_groups_9 ----------------------------------------------------------
if (FALSE)
{
  m <- (findblobs::random_matrix(c(400, 400)) > 0)
  x <- findblobs:::get_column_blobs(m)
  y <- t(findblobs:::get_column_blobs(t(m), offset = max(x)))
  is_set <- x > 0
  all_groups <- unname(split(x[is_set], y[is_set]))
  all_groups <- unique(findblobs:::remove_singles(all_groups))

  (groups <- all_groups)

  length(groups)

  result <- merge_groups_9(groups)

  length(result)

  stopifnot(all(table(unlist(result)) == 1))
}
