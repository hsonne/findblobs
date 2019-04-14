# merge_groups_6 ---------------------------------------------------------------
merge_groups_6 <- function(groups, n_start_recursive = 60, dbg = FALSE)
{
  #groups <- kwb.utils::excludeNULL(groups)

  if (length(groups) < n_start_recursive) {
    return(merge_groups_4(groups))
  }

  n_elements <- lengths(groups)
  if (! all(n_elements > 0)) {
    kwb.utils::printIf(TRUE, groups[n_elements == 0])
    stop()
  }

  ranges <- do.call(rbind, lapply(groups, range))

  middle_id <- max(ranges) %/% 2

  is_left <- ranges[, 2] < middle_id
  is_right <- ranges[, 1] > middle_id
  is_middle <- ! (is_left | is_right)

  kwb.utils::printIf(dbg, c(sum(is_left), sum(is_middle), sum(is_right)))

  groups_left <- if (any(is_left)) {
    kwb.utils::excludeNULL(merge_groups_4(groups[is_left]))
  }

  groups_middle <- if (any(is_middle)) {
    kwb.utils::excludeNULL(merge_groups_4(groups[is_middle]))
  }

  groups_right <- if (any(is_right)) {
    kwb.utils::excludeNULL(merge_groups_4(groups[is_right]))
  }

  kwb.utils::printIf(dbg, c(
    length(groups_left), length(groups_middle), length(groups_right)
  ))

  new_groups <- c(groups_left, groups_middle, groups_right)

  if (identical(new_groups, groups)) {
    merge_groups_4(groups = new_groups)
  } else {
    merge_groups_6(groups = new_groups)
  }
}
