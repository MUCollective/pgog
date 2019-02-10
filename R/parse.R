
#' parses and checks aesthetics mapping
#' @param mapping aesthetics mapping, a list of expressions (used to be quosures)
#' @export
parse_aes <- function(mapping){

  flat_mapping <- flatten_aes(mapping)
  prob_aes_names <- c("width", "height", "area")
  prob_aes <- filter_prob_aes(prob_aes_names, flat_mapping)
  coord_aes <- filter_prob_aes(c("x", "y"), flat_mapping)

  browser()
  # initialize the common conditionals

  # (no longer necessary tho)
  # all_conds <- list()
  # for (i in seq_along(prob_aes)){
  #   all_conds[[i]] <- parse_pmf(prob_aes[[i]])$conditionals
  # }
  # conds <- Reduce(intersect, all_conds)

  # save em to a matrix for easy indexing
  prob_mtx <- aes_to_mtx(prob_aes)
  # sort by conditionals length
  cond_lengths <- sapply(prob_mtx$conditionals, length)
  prob_mtx <- prob_mtx[order(cond_lengths), ]
  # check if the conditionals and marginals multiply into a single pmf
  stopifnot(mtx_check(prob_mtx))



  NULL

}

#' Helper func: Goes thru the matrix to check if it's a legit factorization of a pmf
#' @param mtx rows: pmf terms, cols: marginals, conditionals, aesthetics names
#'
mtx_check <- function(m){

  legit <- TRUE

  for (i in seq_len(nrow(m))){
    cond <- m[i, 2][[1]]
    marg <- m[i, 1][[1]]

    if (i < nrow(m)){
      next_cond <- m[i + 1, 2][[1]]
      combi <- c(cond, marg)
      legit <- setequal(next_cond, combi)
    }
  }
  legit
}




#' Helper function
#'
#' @param mapping
#'
#' @return a dataframe of aes:variables
#'
aes_to_mtx <- function(mapping){

  mtx_nrow <- length(mapping)
  mtx_ncol <- 3 # marginal, cond, aes name

  mtx <- matrix(list(NULL), nrow = mtx_nrow, ncol = mtx_ncol)
  # mtx <- data.frame(mtx)

  aes_names <- names(mapping)

  for (i in seq_len(mtx_nrow)){
    m <- parse_pmf(mapping[[i]])$marginals
    if (!is.list(m)){
      m <- list(m)
    }
    mtx[i, 1][[1]] <- m

    c <- parse_pmf(mapping[[i]])$conditionals
    if (! is.list(c)){
      if (! is.null(c)){
        c <- list(c)
      }
    }

    if (! is.null(c)){ # this leaves NULL in matrix, which is what we want???
      mtx[i, 2][[1]] <- c
    }

    mtx[i, 3] <- aes_names[i]
  }

  # more meaningful names

  colnames(mtx) <- c("marginals", "conditionals", "aes")

  data.frame(mtx)
}



filter_prob_aes <- function(aes_names, mapping){
  # prob_aes <- c("width", "height", "area")
  mapping_names <- names(mapping)

  idx <- numeric()
  for (i in seq_along(mapping_names)){
    for (aes in aes_names){
      if (grepl(aes, x = mapping_names[i])){
        idx <- c(idx, i)
      }
    }
  }

  mapping[idx]
}

#' helper function
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
