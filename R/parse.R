# new version of pmf.R???

#' @param mapping aesthetics mapping, a list of expressions (used to be quosures)
parse_aes <- function(mapping){

  flat_mapping <- flatten_aes(mapping)
  prob_aes <- filter(flat_mapping)





}


filter_prob_aes <- function(mapping){
  prob_aes <- c("width", "height", "area")
  mapping_names <- names(mapping)

  idx <- numeric()
  for (i in seq_along(mapping_names)){
    for (aes in prob_aes){

      if (grepl(aes, x = mapping_names[i])){
        idx <- c(idx, i)
      }
    }

  }

  mapping[idx]

}

#' @param mapping: list $x = [], $width = []
#' @return flattened mapping: $x1 = , $width1 = , $width2 =
flatten_aes <- function(mapping){
  new_mapping <- unlist(mapping)
  aes_names <- names(mapping)
  new_names <- character()
  for (i in seq_along(mapping)){
    for (j in seq_along(mapping[[i]])){
      new_names <- c(new_names, paste(aes_names[[i]], j, sep = ""))
    }
  }
  names(new_mapping) <- new_names
  # browser()
  print(new_mapping)
  new_mapping

}
