# merge_groups_5 ---------------------------------------------------------------
merge_groups_5 <- function(groups)
{
  stopifnot(is.list(groups))
  # stopifnot(all(! sapply(groups, is.unsorted)))

  #bak <- groups
  n <- length(groups)

  if (n == 1) {
    return(groups)
  }

  if (n == 2) {
    return(do.call(merge_two, groups))
  }

  first_two <- c(1, 2)

  merge_n_into(
    new_groups = groups[- first_two],
    groups = do.call(merge_two, groups[first_two])
  )

  # sort(sapply(groups, "[", 1))
  # i <- 1
  # (partners <- which(sapply(groups[-i], function(g) any(groups[[i]] %in% g))))
  # indices <- 2 * seq_len(n %/% 2) - 1
  # #i <- indices[1]
  # do.call(c, lapply(indices, function(i) {
  #   merge_groups(groups[c(i, i + 1)])
  # }))
  # if (n %% 2 == 1) {
  # } else {
  # }
  # stop("length > 3 not yet implemented")
  # mid <- n %/% 2
  # groups_1 <- merge_groups(groups[1:mid])
  # groups_2 <- merge_groups(groups[(mid + 1):length(groups)])
}

# merge_two --------------------------------------------------------------------
merge_two <- function(a, b, dbg = FALSE)
{
  kwb.utils::printIf(dbg, a)
  kwb.utils::printIf(dbg, b)

  if (any(a %in% b)) {
    #sort(unique(c(a, b)))
    list(sort(unique(c(a, b))))
  } else {
    list(a, b)
  }
}

# merge_n_into -----------------------------------------------------------------
merge_n_into <- function(new_groups, groups)
{
  stopifnot(is.list(new_groups))
  stopifnot(is.list(groups))

  Reduce(merge_one_into, new_groups, init = groups)
}

# merge_one_into ---------------------------------------------------------------
merge_one_into <- function(groups, group)
{
  stopifnot(is.list(groups))
  stopifnot(is.numeric(group))

  which_join <- which(sapply(groups, function(x) any(group %in% x)))

  n <- length(which_join)

  if (n == 0) {
    c(groups, list(group))
  } else {
    groups[[which_join[1]]] <- sort(unique(c(unlist(groups[which_join]), group)))
    if (n > 1) {
      groups[- which_join[-1]]
    } else {
      groups
    }
  }
}
