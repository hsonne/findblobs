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
    blob_matrix <- findblobs:::get_blobs(M = (random_matrix(c(5, 5)) == "x"))
    findblobs:::plot_integer_matrix(blob_matrix)
  }

  for (i in 1:1) {
    message("Test run ", i)
    blobs <- findblobs:::get_blobs(M = (random_matrix(c(350, 350)) == "x"))
  }

  plot_integer_matrix(blobs)
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

# random_matrix ----------------------------------------------------------------
random_matrix <- function(matrix_dim = c(10, 10))
{
  values <- sample(c("_", "x"), prod(matrix_dim), replace = TRUE)

  matrix(values, nrow = matrix_dim[1])
}
