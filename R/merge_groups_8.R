if (FALSE)
{
  m <- (findblobs:::random_matrix(c(10, 10)) > 0)
  x <- findblobs:::get_column_blobs(m)
  y <- t(findblobs:::get_column_blobs(t(m), offset = max(x)))
  is_set <- x > 0
  all_groups <- unname(split(x[is_set], y[is_set]))
  all_groups <- unique(findblobs:::remove_singles(all_groups))
  groups <- all_groups

  merge_groups_8(groups)
}

# merge_groups_8 ---------------------------------------------------------------
merge_groups_8 <- function(groups)
{
  L <- groups_to_link_matrix(groups)
  row_state <- rep(0L, nrow(L))
  state_seen <- 1L
  state_used <- 2L

  row <- 1
  row_state[row] <- state_used
  cols <- groups[[row]]

  while(! is.na(row)) {

    rows <- which(
      row_state == 0 & (L[, cols, drop = FALSE] > 0),
      arr.ind = TRUE
    )[, 1]

    if (length(rows)) {

      row_state[rows] <- state_seen
      cols <- unique(unlist(groups[rows]))
      groups[[row]] <- sort(unique(groups[[row]], cols))

    } else {

      row <- which(row_state == 0)[1]
      if (! is.na(row)) {
        row_state[row] <- state_used
        cols <- unique(unlist(groups[row]))
      }
    }
  }

  kwb.utils::excludeNULL(groups[row_state == state_used])
}
