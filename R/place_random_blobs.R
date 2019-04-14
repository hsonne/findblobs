# place_random_blobs -----------------------------------------------------------

#' Create Random Blobs in a Matrix
#'
#' @param m matrix of integer in which to place blobs. Default: 10 x 15-Matrix
#' @param n_blobs number of blobs to create. Default: 5
#' @param min_fields minimum number of fields a blob should consist of
#' @param max_fields maximum number of fields a blob should consist of. Defaults
#'   to \code{min_fields}
#' @param do_plot if \code{TRUE} (the default), the result matrix is plotted
#' @return matrix in which fields belonging to the same blob have the same
#'   integer number and zeros indicate empty fields
#' @export
#' @examples
#' set.seed(42)
#' place_random_blobs(n_blobs = 5, min_fields = 5, max_fields = 10)
#'
place_random_blobs <- function(
  m = matrix(0L, nrow = 10, ncol = 15),
  n_blobs = 5, min_fields = 3, max_fields = min_fields, do_plot = TRUE
)
{
  for (id in seq_len(n_blobs)) {

    n_fields <- if (min_fields == max_fields) {
      min_fields
    } else {
      sample(min_fields:max_fields, 1)
    }

    message("Creating blob ", id, "/", n_blobs, " with ", n_fields, " fields")

    m <- set_random_blob(m, n_fields, id)
  }

  # Plot the matrix
  if (do_plot) {
    plot_integer_matrix(m)
  }

  m
}

# set_random_blob --------------------------------------------------------------
set_random_blob <- function(m, n_fields, id = 1)
{
  max_row <- nrow(m)
  max_col <- ncol(m)
  field_counter <- 0

  # Matrix to lookup number of element at position [row, column]
  pos_to_index <- matrix(seq_len(max_row * max_col), nrow = max_row)

  # Initialise a matrix to hold the blob positions
  blob_pos <- matrix(NA_integer_, nrow = n_fields, ncol = 2)

  # Get empty positions in matrix m
  possible_pos <- which(m == 0, arr.ind = TRUE)

  while (nrow(possible_pos) && field_counter < n_fields) {

    # Number of possible positions
    n_possible <- nrow(possible_pos)

    # Select a possible position, randomly if there is more than one
    pos <- if (n_possible == 1) {
      possible_pos[1, ]
    } else {
      possible_pos[sample(n_possible, 1), ]
    }

    # Add the position to the blob
    field_counter <- field_counter + 1
    blob_pos[field_counter, ] <- pos

    # Determine all possible neighbour positions
    positions_0 <- do.call(rbind, lapply(seq_len(field_counter), function(i) {
      x <- blob_pos[i, 1]
      y <- blob_pos[i, 2]
      rbind(c(x + 1, y), c(x - 1, y), c(x, y + 1), c(x, y - 1))
    }))

    # Remove positions that are out of the matrix
    row_ok <- kwb.utils::inRange(positions_0[, 1], 1, max_row)
    col_ok <- kwb.utils::inRange(positions_0[, 2], 1, max_col)
    positions_1 <- positions_0[row_ok & col_ok, ]

    # Remove positions that are not free in the matrix
    positions_2 <- positions_1[m[positions_1] == 0, , drop = FALSE]

    # Remove positions that are part of the current blob
    taken <- pos_to_index[blob_pos[seq_len(field_counter), , drop = FALSE]]
    wanted <- pos_to_index[positions_2]
    positions_3 <- positions_2[! wanted %in% taken, , drop = FALSE]

    # Update the list of possible positions
    possible_pos <- positions_3

  } # end of while()

  # Set the blob positions in the matrix
  m[blob_pos] <- id

  m
}
