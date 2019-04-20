# merge_groups_9 ---------------------------------------------------------------
merge_groups_9 <- function(groups)
{
  L <- groups_to_link_matrix(groups)

  rows_seen <- logical(nrow(L))
  result <- list()
  row <- 1
  new_row <- TRUE

  while (! is.na(row)) {

    if (new_row) {

      new_row <- FALSE
      cols <- groups[[row]]
      rows_seen[row] <- TRUE
    }

    is_linked <- ! rows_seen & L[, cols, drop = FALSE] > 0

    if (any(is_linked)) {

      rows <- unique(which(is_linked, arr.ind = TRUE)[, 1])
      rows_seen[rows] <- 1
      cols <- unique(unlist(groups[rows]))
      L[row, cols] <- 1

    } else {

      result[[length(result) + 1]] <- which(L[row, ] == 1)
      row <- which(! rows_seen)[1]
      new_row <- TRUE
    }
  }

  result
}

# mark_rows --------------------------------------------------------------------
mark_rows <- function(m, rows = integer())
{
  m[rows, ][m[rows, ] == 0] <- "-"
  as.data.frame(m)
}

# mark_cols --------------------------------------------------------------------
mark_cols <- function(m, cols = integer())
{
  m[, cols][m[, cols] == 0] <- "-"
  as.data.frame(m)
}
