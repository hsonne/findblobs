# merge_groups_2 ---------------------------------------------------------------
merge_groups_2 <- function(groups)
{
  queue <- seq_along(groups)

  fmt <- "Length(queue): %8d.\n"
  #cat(sprintf(fmt, 0))
  backspace <- paste(rep("\b", nchar(fmt) + 5), collapse = "")

  while (length(queue)) {

    i <- queue[1]
    queue <- queue[-1]

    #cat(paste0(backspace, sprintf(fmt, length(queue))))

    (group <- groups[[i]])

    if (! is.null(group)) {

      (intersects <- sapply(groups, function(x) any(x %in% group)))
      (indices <- setdiff(which(intersects), i))

      if (length(indices)) {
        groups[[i]] <- sort(unique(c(group, unlist(groups[indices]))))
        groups[indices] <- lapply(indices, function(i) NULL)
        queue <- c(queue, i)
      }
    }
  }

  kwb.utils::excludeNULL(groups)
}
