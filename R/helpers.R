# compare_group_lists ----------------------------------------------------------
compare_group_lists <- function(gl1, gl2)
{
  stopifnot(length(gl1) == length(gl2))

  equals <- sapply(seq_along(gl1), function(i) {
    identical(gl1[[i]], gl2[[i]])
  })

  if (any(! equals)) {
    kwb.utils::printIf(TRUE, which(! equals))
    kwb.utils::printIf(TRUE, gl1[! equals])
    kwb.utils::printIf(TRUE, gl2[! equals])
  }
}

# order_by_first ---------------------------------------------------------------

#' Order List of Vectors by First Vector Elements
#'
#' @param x list of vectors
#' @examples
#' findblobs:::order_by_first(list(
#'   c(3, 4),
#'   c(1, 5, 6),
#'   c(2, 1)
#' ))
#'
order_by_first <- function(x)
{
  stopifnot(is.list(x))

  x[order(sapply(x, "[[", 1))]
}

# random_matrix ----------------------------------------------------------------

#' Create Matrix with Randomly "Filled" Fields
#'
#' @param matrix_dim numeric vector of length two giving the number of rows and
#'   columns, respectively of the matrix
#' @export
#'
random_matrix <- function(matrix_dim = c(10, 10))
{
  values <- sample(c("_", "x"), prod(matrix_dim), replace = TRUE)

  matrix(values, nrow = matrix_dim[1])
}
