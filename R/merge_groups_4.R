# merge_groups_4 ---------------------------------------------------------------
merge_groups_4 <- function(groups)
{
  id_frequency <- table(unlist(groups))

  (single_ids <- as.integer(names(id_frequency[id_frequency == 1])))

  if (length(single_ids) == 0) {
    return(merge_groups_5(groups))
  }

  i <- which(sapply(groups, function(g) all(g %in% single_ids)))

  if (length(i) == 0) {
    return(merge_groups_5(groups))
  }

  #message("\nPutting aside ", length(i), " out of ", length(groups),
  #        " groups that cannot be merged.")

  c(groups[i], merge_groups_5(groups = groups[-i]))
}
