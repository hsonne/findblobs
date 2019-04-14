# get_column_blobs -------------------------------------------------------------

#' Find Connected Cells along Matrix Columns
#'
#' @param m a matrix of logical
#' @param offset integer to be added to the numbers indicating connected cells
#' @return matrix of integer having the same dimension as \code{m}. Cells being
#'   \code{FALSE} in \code{m} are \code{0} in the output. Cells being
#'   \code{TRUE} in \code{m} are a positive integer number in the output.
#'   Connected cells within same columns share the same number.
#' @examples
#' column_blobs <- findblobs:::get_column_blobs(
#'   matrix(ncol = 3, byrow = TRUE, c(
#'     FALSE, TRUE, FALSE,
#'     TRUE,  TRUE, FALSE,
#'     FALSE, FALSE, TRUE,
#'     TRUE,  TRUE,  TRUE,
#'     TRUE,  FALSE, TRUE
#'   ))
#' )
#'
#' findblobs::plot_integer_matrix(column_blobs)
#'
get_column_blobs <- function(m, offset = 0L)
{
  stopifnot(is.matrix(m), is.logical(m))

  n <- nrow(m)

  m_top <- m[-n, ]
  m_bottom <- m[-1, ]

  which_start <- which(rbind(m[1, ], m_bottom & ! m_top))
  which_end <- which(rbind(m_top & ! m_bottom, m[n, ]))

  ids <- seq_along(which_start)

  indices <- unlist(lapply(ids, function(i) seq(which_start[i], which_end[i])))

  blobs <- matrix(0L, nrow = nrow(m), ncol = ncol(m))
  blobs[indices] <- rep(ids + offset, which_end - which_start + 1)

  blobs
}
