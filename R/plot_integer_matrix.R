# plot_integer_matrix ----------------------------------------------------------

#' Plot Matrix of Integer as Coloured Squares
#'
#' @param x matrix of integer
#' @export
#' @examples
#' plot_integer_matrix(matrix(nrow = 5, byrow = TRUE, c(
#'   2, 2, 2, 2, 2,
#'   2, 0, 1, 0, 2,
#'   2, 1, 1, 1, 2,
#'   2, 0, 1, 0, 2,
#'   2, 2, 2, 2, 2
#' )))
#'
plot_integer_matrix <- function(x)
{
  old_par <- par(mar = c(1, 1, 1, 1))
  on.exit(par(old_par))

  ids <- setdiff(sort(unique(c(x))), 0)
  colours <- stats::setNames(grDevices::rainbow(length(ids)), ids)

  graphics::plot(
    NULL, xlim = c(0, ncol(x)), ylim = c(nrow(x), 0),
    asp = 1, axes = FALSE, xlab = "", ylab = ""
  )

  coords <- which(x != 0, arr.ind = TRUE)

  graphics::rect(
    xleft = coords[, "col"] - 1,
    ybottom = coords[, "row"],
    xright = coords[, "col"],
    ytop = coords[, "row"] - 1,
    col = colours[as.character(x[coords])],
    border = NA
  )

  graphics::rect(0, 0, ncol(x), nrow(x))
}
