
#' parses and checks aesthetics mapping
#' @param mapping aesthetics mapping, a list of expressions (used to be
#' quosures)
#' @export
parse_aes <- function(mapping){

  # 1. get a list of prob and coord aesthetics
  flat_mapping <- flatten_aes(mapping)
  prob_aes_names <- c("width", "height", "area")
  prob_aes <- filter_prob_aes(prob_aes_names, flat_mapping)
  coord_aes <- filter_prob_aes(c("x", "y"), flat_mapping)

  # save em to a matrix for easy indexing
  # also checks aes mapping (1D, 2D)
  prob_mtx <- aes_to_mtx(prob_aes)

  # 2. sort by conditionals length
  cond_lengths <- sapply(prob_mtx$conditionals, length)
  # TODO: cond_lengths works with P(A) where cond = NULL, but not with
  # P(1|A)
  prob_mtx <- prob_mtx[order(cond_lengths), ]
  # check if the conditionals and marginals multiply into a single pmf
  stopifnot(mtx_check(prob_mtx))

  # 3. completes conditionals like P(1|A)
  prob_mtx <- complete_conditionals(prob_mtx)

  # 4. check and combine with coord aesthetics

  mtx <- add_coord_aes(prob_mtx, coord_aes)

  # 5. number the levels
  mtx$level <- seq_len(nrow(mtx))

  pprint(mtx)


  mtx

}

#' Helper func
#' @param prob_mtx
#' @param coord_mtx
#'
#' @return
#'
add_coord_aes <- function(prob_mtx, coord_aes){


  for (i in seq_along(coord_aes)){
    aes <- names(coord_aes)[[i]]
    pvar <- as.character(coord_aes[[i]])

    for (j in seq_len(nrow(prob_mtx))){
      # P(B|A) or P(1|A)
      if (as.character(prob_mtx$marginals[[j]][[1]]) == pvar ||
          as.character(prob_mtx$conditionals[[j]][[1]]) == pvar){
        supplied_aes <- prob_mtx$aes[[j]]

        # fix NULL <- P(1|A)
        if (is.list(supplied_aes)){
          if (is.null(supplied_aes[[1]])){
            if (aes == "x"){
              supplied_aes <- "height"
            } else {
              supplied_aes <- "width"
            }
          }
        }

        prob_mtx$aes[[j]] <- paste(aes, ".", supplied_aes, sep = "")
        break
      }


    }
  }

  # check for underspecified, i.e. there's still NULL aesthetics mapping
  stopifnot(!sum(sapply(flatten(prob_mtx$aes),is.null)))

  prob_mtx

}




#' Helper function: add in the implied P(1|A), etc
#'
#' @param prob_mtx probability matrix as specified
#'
#' @return the matrix with probability aesthetics, including
#'
#' @examples
complete_conditionals <- function(prob_mtx){

  # if first cond != null (or first marg != 1)
  extra_conds <- prob_mtx$conditionals[[1]]
  if (! is.null(extra_conds)){
    for (cond in extra_conds){
      prob_mtx <- rbind(rep(0, 3), prob_mtx)
      prob_mtx$marginals[[1]] <- list(expr(1))
      prob_mtx$conditionals[[1]] <- list(cond)
      prob_mtx$aes[[1]] <- list(NULL)
    }
  }

  prob_mtx

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

  # browser()
  mtx_nrow <- length(mapping)
  mtx_ncol <- 3 # marginal, cond, aes name

  mtx <- matrix(list(NULL), nrow = mtx_nrow, ncol = mtx_ncol)
  # mtx <- data.frame(mtx)

  aes_names <- names(mapping)

  for (i in seq_len(mtx_nrow)){

    # marginal
    m <- parse_pmf(mapping[[i]])$marginals
    if (!is.list(m)){
      m <- list(m)
    }
    mtx[i, 1][[1]] <- m

    # conditional
    c <- parse_pmf(mapping[[i]])$conditionals
    if (! is.list(c)){
      if (! is.null(c)){
        c <- list(c)
      }
    }

    if (! is.null(c)){ # this leaves NULL in matrix, which is what we want???
      mtx[i, 2][[1]] <- c
    }

    # aesthetics
    mtx[i, 3] <- aes_names[i]

    # check mapping for width <- P(A, B)
    stopifnot(check_aes(m, aes_names[i]))
  }

  # more meaningful names
  colnames(mtx) <- c("marginals", "conditionals", "aes")
  data.frame(mtx)
}

check_aes <- function(marg, aes){
  # dimension is right
  aes_dimension <- 1
  if (aes == "area"){
    aes_dimension <- 2
  }
  aes_dimension == length(marg)
}


filter_prob_aes <- function(aes_names, mapping){
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

#' helper function: number those aesthetics since
#' @param mapping: list $x = c(A), $width = c(P(B|A), P(A))
#' @return flattened mapping: $width1 = , $width2 =
flatten_aes <- function(mapping){

  # browser()

  new_mapping <- unlist(mapping)
  aes_names <- names(mapping)
  new_names <- character()
  for (i in seq_along(mapping)){
    for (j in seq_along(mapping[[i]])){
      # new_names <- c(new_names, paste(aes_names[[i]], j, sep = ""))
      new_names <- c(new_names, aes_names[[i]])
    }
  }
  names(new_mapping) <- new_names
  # print(new_mapping)
  new_mapping

}
