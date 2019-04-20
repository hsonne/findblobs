# get_blobs --------------------------------------------------------------------
get_blobs <- function(M, methods = 6)
{
  #kwb.utils::assignPackageObjects("findblobs")

  column_blobs <- kwb.utils::catAndRun(
    "Getting column blobs",
    get_column_blobs(M)
  )

  row_blobs <- kwb.utils::catAndRun(
    "Getting row blobs",
    t(get_column_blobs(t(M), offset = max(column_blobs)))
  )

  is_set <- column_blobs > 0

  # Compose information about linked row/column blobs
  groups <- unique(remove_singles(unname(split(
    column_blobs[is_set], row_blobs[is_set]
  ))))

  # Try different methods of merging the group information
  groups_list <- lapply(methods, function(method) {
    kwb.utils::catAndRun(
      paste("Merging group info with method", method),
      merge_groups(groups, method = method)
    )
  })

  dbg <- FALSE

  blobs_list <- lapply(groups_list, function(groups) {
    kwb.utils::catAndRun(dbg = dbg, "Applying group info", {
      apply_group_info(column_blobs, groups)
    })
  })

  if (length(methods) > 1) {
    stopifnot(kwb.utils::allAreIdentical(blobs_list))
  }

  blobs_list[[1]]
}

# merge_groups -----------------------------------------------------------------
merge_groups <- function(groups, method = 4)
{
  if (method == 4) {
    merge_groups_4(groups)
  } else if (method == 5) {
    merge_groups_5(groups)
  } else if (method == 6) {
    merge_groups_6(groups)
  } else if (method == 7) {
    merge_groups_7(groups)
  } else if (method == 8) {
    merge_groups_8(groups)
  } else if (method == 9) {
    merge_groups_9(groups)
  } else {
    stop("Unknown method: ", method)
  }
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

# apply_group_info -------------------------------------------------------------
apply_group_info <- function(column_blobs, groups)
{
  for (group in unique(groups)) {
    column_blobs[column_blobs %in% group] <- group[1]
  }

  column_blobs
}
