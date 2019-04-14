# merge_groups_3 ---------------------------------------------------------------
merge_groups_3 <- function(groups)
{
  is_queued <- rep(TRUE, length(groups))

  fmt <- "Length(queue): %8d.\n"
  #cat(sprintf(fmt, 0))
  backspace <- paste(rep("\b", nchar(fmt) + 5), collapse = "")

  while (sum(is_queued)) {

    #cat(paste0(backspace, sprintf(fmt, sum(is_queued))))

    i <- which(is_queued)[1]

    is_queued[i] <- FALSE

    (group <- groups[[i]])

    if (! is.null(group)) {

      (intersects <- sapply(groups, function(x) any(x %in% group)))
      (indices <- setdiff(which(intersects), i))

      if (length(indices)) {
        groups[[i]] <- unique(c(group, unlist(groups[indices])))
        groups[indices] <- lapply(indices, function(ii) NULL)
        is_queued[i] <- TRUE
        is_queued[indices] <- FALSE
      }
    }
  }

  lapply(kwb.utils::excludeNULL(groups), sort)
}
