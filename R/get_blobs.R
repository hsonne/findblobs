if (FALSE)
{
  M1 <- matrix(byrow = TRUE, ncol = 3, c(
    "", "", "x",
    "", "x", "x",
    "x", "x", "x"
  ))

  M2 <- matrix(ncol = 4, byrow = TRUE, c(
    F, T, T, F,
    T, T, F, F,
    T, F, T, T,
    F, T, T, F
  ))
  
  par(mfrow = c(10, 10), mar = c(0, 0, 0, 0))
  
  for (i in 1:100) {
    (content <- matrix(sample(c("x", ""), size = 25, replace = TRUE), ncol = 5))
    plot_integer_matrix(x <- get_blobs(content == "x"))
  }
  
  matrix_dim <- c(200, 200)

  for (i in 1:1) {
    
    message("Test run ", i)
    
    M <- "x" == matrix(
      nrow = matrix_dim[1], 
      sample(c("_", "x"), prod(matrix_dim), replace = TRUE)
    )
    
    blobs <- get_blobs(M)
  }  
  
  plot_integer_matrix(blobs, 30)
}

# get_blobs --------------------------------------------------------------------
get_blobs <- function(M)
{
  column_blobs <- kwb.utils::catAndRun(
    "Getting column blobs", 
    get_column_blobs(M)
  )
  
  row_blobs <- kwb.utils::catAndRun(
    "Getting row blobs", 
    t(get_column_blobs(t(M), offset = max(column_blobs)))
  )

  #x = column_blobs; y = row_blobs

  groups_2 <- kwb.utils::catAndRun(
    "Merging group info with method 2", 
    find_groups(x = column_blobs, y = row_blobs, method = 2)
  )

  groups_3 <- kwb.utils::catAndRun(
    "Merging group info with method 3", 
    find_groups(x = column_blobs, y = row_blobs, method = 3)
  )

  groups_4 <- kwb.utils::catAndRun(
    "Merging group info with method 4", 
    find_groups(x = column_blobs, y = row_blobs, method = 4)
  )

  groups_5 <- kwb.utils::catAndRun(
    "Merging group info with method 5", 
    find_groups(x = column_blobs, y = row_blobs, method = 5)
  )

  dbg <- FALSE
  
  blobs_2 <- kwb.utils::catAndRun(dbg = dbg, "Applying group info", {
    apply_group_info(column_blobs, groups_2)
  })

  blobs_3 <- kwb.utils::catAndRun(dbg = dbg, "Applying group info", {
    apply_group_info(column_blobs, groups_3)
  })

  blobs_4 <- kwb.utils::catAndRun(dbg = dbg, "Applying group info", {
    apply_group_info(column_blobs, groups_4)
  })

  blobs_5 <- kwb.utils::catAndRun(dbg = dbg, "Applying group info", {
    apply_group_info(column_blobs, groups_5)
  })

  stopifnot(kwb.utils::allAreIdentical(list(blobs_2, blobs_3, blobs_4, blobs_5)))
  
  blobs_2
}

# find_groups ----------------------------------------------------------------
find_groups <- function(x, y, method = 2)
{
  is_set <- x > 0

  groups <- unique(remove_singles(unname(split(x[is_set], y[is_set]))))

  if (method == 2) {
    merge_groups_2(groups)
  } else if (method == 3) {
    merge_groups_3(groups)
  } else if (method == 4) {
    merge_groups_4(groups)
  } else if (method == 5) {
    merge_all_groups(groups)
  } else {
    stop("Unknown method: ", method)
  }
}

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

# apply_group_info -------------------------------------------------------------
apply_group_info <- function(column_blobs, groups)
{
  for (group in unique(groups)) {
    column_blobs[column_blobs %in% group] <- group[1]
  }
  
  column_blobs
}

# remove_singles ---------------------------------------------------------------
remove_singles <- function(x)
{
  stopifnot(is.list(x))

  is_single <- lengths(x) < 2
  
  if (any(is_single)) {
    #message("Removing ", sum(is_single), " singles.")
    x[! is_single]
  } else {
    x
  }
}

# get_column_blobs -------------------------------------------------------------
get_column_blobs <- function(m, offset = 0)
{
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

# plot_integer_matrix ----------------------------------------------------------
plot_integer_matrix <- function(x, n_cols = 4)
{
  #x <- blobs
  ids <- setdiff(sort(unique(c(x))), 0)
  colours <- stats::setNames(rainbow(length(ids)), ids)
  
  plot(
    NULL, xlim = c(0, ncol(x)), ylim = c(nrow(x), 0),
    asp = 1, axes = FALSE, xlab = "", ylab = ""
  )
  
  coords <- which(x != 0, arr.ind = TRUE)
  
  rect(
    xleft = coords[, "col"] - 1,
    ybottom = coords[, "row"],
    xright = coords[, "col"],
    ytop = coords[, "row"] - 1,
    col = colours[as.character(x[coords])],
    border = NA
  )
  
  rect(0, 0, ncol(x), nrow(x))
}
