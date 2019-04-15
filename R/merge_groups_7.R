# merge_groups_7 ---------------------------------------------------------------
merge_groups_7 <- function(groups)
{
  L <- groups_to_link_matrix(groups)

  handled <- integer(length(groups))

  merged <- lapply(seq_along(groups), function(i) {

    if (! handled[i]) {

      handled[i] <<- 1
      elements <- groups[[i]]
      (g <- elements)

      while (length(g)) {

        # Indices of "connected" groups
        gi <- unique(which(L[, g, drop = FALSE] == 1L, arr.ind = TRUE)[, 1])

        # Other groups than the current one?
        if (length(gi) > 1) {

          gi <- setdiff(gi, i)
          handled[gi] <<- 1
          g <- setdiff(unique(unlist(groups[gi])), elements)
          elements <- unique(c(g, elements))

        } else {

          g <- NULL
        }
      }

      sort(elements)
    } # else NULL (if already handled)
  })

  kwb.utils::excludeNULL(unique(merged))
}

# groups_to_link_matrix --------------------------------------------------------
groups_to_link_matrix <- function(groups)
{
  unlisted <- unlist(groups)
  result <- matrix(0L, nrow = length(groups), ncol = max(unlisted))
  result[cbind(rep(seq_along(groups), lengths(groups)), unlisted)] <- 1
  result
}
