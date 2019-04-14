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
  #kwb.utils::assignArgumentDefaults(findblobs::place_random_blobs)
  #kwb.utils::assignPackageObjects("findblobs")

  for (id in seq_len(n_blobs)) {

    n_fields <- if (min_fields == max_fields) {
      min_fields
    } else {
      sample(min_fields:max_fields, 1)
    }

    message("Creating blob ", id, "/", n_blobs, " with ", n_fields, " fields")

    m <- set_random_blob(m, n_fields, id)
  }

  # Free the "forbidden" fields around the blobs (indicated by negative ids)
  m[m < 0] <- 0

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
    positions_0 <- positions_around(blob_pos[1:field_counter, , drop = FALSE])

    # Remove positions that are out of the matrix
    positions_1 <- exclude_out_of_range(positions_0, max_row, max_col)

    # Remove positions that are not free in the matrix
    positions_2 <- positions_1[m[positions_1] == 0, , drop = FALSE]

    # Remove positions that are part of the current blob
    positions_3 <- exclude_positions(
      positions_2, blob_pos[1:field_counter, , drop = FALSE]
    )

    # Update the list of possible positions
    possible_pos <- positions_3

  } # end of while()

  # Exclude empty positions, if any
  blob_pos <- blob_pos[seq_len(field_counter), , drop = FALSE]

  if (nrow(blob_pos)) {

    # Set the blob positions in the matrix
    m[blob_pos] <- id

    # Set the negative id in "forbidden" fields "around" the blob
    forbidden_pos <- positions_around(blob_pos)
    forbidden_pos <- exclude_out_of_range(forbidden_pos, max_row, max_col)
    forbidden_pos <- exclude_positions(forbidden_pos, blob_pos)

    m[forbidden_pos] <- -id
  }

  m
}

# positions_around -------------------------------------------------------------
positions_around <- function(blob_pos)
{
  unique(do.call(rbind, lapply(seq_len(nrow(blob_pos)), function(i) {
    x <- blob_pos[i, 1]
    y <- blob_pos[i, 2]
    rbind(c(x + 1, y), c(x - 1, y), c(x, y + 1), c(x, y - 1))
  })))
}

# exclude_out_of_range ---------------------------------------------------------
exclude_out_of_range <- function(positions, max_row, max_col)
{
  row_ok <- kwb.utils::inRange(positions[, 1], 1, max_row)
  col_ok <- kwb.utils::inRange(positions[, 2], 1, max_col)

  positions[row_ok & col_ok, ]
}

# exclude_positions ------------------------------------------------------------
exclude_positions <- function(positions, exclude = NULL)
{
  if (is.null(exclude)) {
    return(positions)
  }

  if (any(is.na(positions))) {
    stop("There are NA in positions in exclude_positions()!")
  }

  if (any(is.na(exclude))) {
    stop("There are NA in exclude in exclude_positions()!")
  }

  # Maximum row and column index
  max_row <- max(c(positions[, 1], exclude[, 1]))
  max_col <- max(c(positions[, 2], exclude[, 2]))

  # Matrix to lookup number of element at position [row, column]
  pos_to_index <- matrix(seq_len(max_row * max_col), nrow = max_row)

  wanted <- pos_to_index[positions]
  taken <- pos_to_index[exclude]

  positions[! wanted %in% taken, , drop = FALSE]
}
