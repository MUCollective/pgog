# new version of pmf.R???

#' @param mapping aesthetics mapping, a list of expressions (used to be quosures)
parse_aes <- function(mapping){

  flat_mapping <- flatten_aes(mapping)
  prob_aes <- filter_prob_aes(flat_mapping)

  # initialize the common conditionals
  all_conds <- list()
  for (i in seq_along(prob_aes)){
    all_conds[[i]] <- parse_pmf(prob_aes[[i]])$conditionals
  }
  conds <- Reduce(intersect, all_conds)

  # save em to a matrix for easy indexing
  mtx <- aes_to_mtx(prob_aes)
  # sort by conditionals length

  mtx

}

aes_to_mtx <- function(mapping){

  mtx_w <- length(mapping)
  mtx_h <- 3 # marginal, cond, aes name

  mtx <- matrix(list(rep(NULL, mtx_h * mtx_w)), mtx_h, mtx_w)

  aes_names <- names(mapping)

  for (j in seq_len(mtx_w)){
    m <- parse_pmf(mapping[[j]])$marginals
    if (!is.list(m)){
      m <- list(m)
    }
    mtx[1, j][[1]] <- m

    c <- parse_pmf(mapping[[j]])$conditionals
    if (! is.list(c)){
      c <- list(c)
    }
    mtx[2, j][[1]] <- c
    mtx[3, j] <- aes_names[j]
  }

  # more meaningful names

  row.names(mtx) <- c("marginals", "conditionals", "aes")

  mtx
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
  # print(new_mapping)
  new_mapping

}
