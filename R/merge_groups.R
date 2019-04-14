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

# merge_groups_4 ---------------------------------------------------------------
merge_groups_4 <- function(groups)
{
  id_frequency <- table(unlist(groups))

  (single_ids <- as.integer(names(id_frequency[id_frequency == 1])))

  if (length(single_ids) == 0) {
    return(merge_groups_3(groups))
  }

  i <- which(sapply(groups, function(g) all(g %in% single_ids)))

  if (length(i) == 0) {
    return(merge_groups_3(groups))
  }

  #message("\nPutting aside ", length(i), " out of ", length(groups),
  #        " groups that cannot be merged.")

  new_groups <- if (length(groups) < 50) {
    merge_groups_2(groups = groups[-i])
  } else {
    mid <- length(groups) %/% 2
    first <- 1:mid
    g1 <- merge_groups_4(groups[first])
    g2 <- merge_groups_4(groups[-first])
    merge_groups_2(c(g1, g2))
  }

  c(groups[i], new_groups)
}

