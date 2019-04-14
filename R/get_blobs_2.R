if (FALSE)
{
  x <- get_column_blobs(M)
  y <- t(get_column_blobs(t(M), offset = max(column_blobs)))
  is_set <- x > 0
  groups <- unique(remove_singles(unname(split(x[is_set], y[is_set]))))

  g2 <- order_by_first(merge_groups_2(groups))
  g3 <- order_by_first(merge_groups_3(groups))
  g4 <- order_by_first(merge_groups_4(groups))
  g5 <- order_by_first(merge_all_groups(groups))

  identical(g2, g5)
  compare_group_lists(g2, g5)
}

# order_by_first ---------------------------------------------------------------
order_by_first <- function(x)
{
  stopifnot(is.list(x))

  x[order(sapply(x, "[[", 1))]
}

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
