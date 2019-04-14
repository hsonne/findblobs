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
