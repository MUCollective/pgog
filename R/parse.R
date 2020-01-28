
#' parses and checks aesthetics mapping
#' @param mapping aesthetics mapping, a list of expressions (used to be
#' quosures)
#' @return the aesthetics matrix
#' @export
parse_aes <- function(mapping){

  #browser()
  # 1. get a list of prob and coord aesthetics
  #output list (for example, height = c(P(cyl|gear)), x = c(gear), list with length 2, 2 elements height and x
  #x get gear out of the c(). Within height, 2 elements, P and cyl|gear, then inside cyl|gear, three elements, cyl, |, and gear
  # In the meantime, get the rid of env, only need the object part, primiarily just get rid of the environment, flat_mapping is a list of length 2
  # , and cannot access the information inside the second index)
  flat_mapping <- flatten_aes(mapping)$mapping

  prob_aes_names <- c("width", "height", "area")
  # prob_aes_names <- c("width", "height", "area", "alpha", "color", "colour", "fill")

  # match the aes names first, extract the pre-specifiecd information we need c("width", "height", "area")
  # extract that part from flat_mapping
  prob_aes <- filter_prob_aes(prob_aes_names, flat_mapping)

  # similar as the previous one
  coord_aes <- filter_prob_aes(c("x", "y", "alpha", "color", "colour", "fill"), flat_mapping)

  elements_checker(prob_aes,coord_aes)

  # save them to a matrix for easy indexing
  # also checks aes mapping (1D, 2D)
  # change the list the matrix, convert each elements to the column instead of 2 dim list
  # add marginal
  prob_mtx <- aes_to_mtx(prob_aes)

  #browser()

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
  ## add level to the matrix

  # pprint(mtx)


  mtx
  # return the matrix

}

#' Helper: get all the elements in the height and combine into one vector, compare two sets
#' all elements in coord_aes should be included in the prob_aes
#' @param prob_aes, coord_aes
#' @return error message or nothing
#'
elements_checker = function(prob_aes, coord_aes) {
  #browser()
  list_coord_aes = c()
  list_prob_aes = c()
  for (i in seq_along(coord_aes)){
    list_coord_aes = c(list_coord_aes, coord_aes[[i]])
  }
  prob_aes_tmp = prob_aes
  for(j in seq_along(prob_aes_tmp)){
    prob_aes_tmp[[j]][[1]] = NULL
    if(length(prob_aes_tmp[[1]][[1]]) != 1 && prob_aes_tmp[[j]][[1]][[1]] == "|"){
      prob_aes_tmp[[j]][[1]][[1]] = NULL
      for (k in seq_along(unlist(prob_aes_tmp))) {
        list_prob_aes = c(list_prob_aes,unlist(prob_aes_tmp)[[k]])
      }
    } else {
      for (m in seq_along(prob_aes_tmp[[j]]))
      list_prob_aes = c(list_prob_aes,prob_aes_tmp[[j]][[m]])
    }
  }
  list_coord_aes = unique(list_coord_aes)
  list_prob_aes = unique(list_prob_aes)
  if (all(list_coord_aes %in% list_prob_aes)==FALSE){
    stop('Parser failed due to miss matching')
  }
}


#' Helper: returns the names of all random variables contained in a parsed
#' mapping.
#'
#' @param mapping mapping parsed by `parse_aes`
#'
#' @return vector of strings containing random variable names
#'
get_all_rv <- function(mapping){
  #browser()

  # margs <- flatten(mapping$marginals)
  # # filter out the P(1|A) conds
  # alles <- get_margs(mapping)
  # # find P(1|A) conds
  # cond_inx <- sapply(margs, function(i) i == 1)
  # alles <- c(alles, rev(flatten(mapping$conditionals[cond_inx])))

  c(get_margs(mapping), get_conds(mapping))

}

#' TODO
#' @param mapping probability mapping TODO
#' @return TODO: all marginals in the mapping
#'
get_margs <- function(mapping){
  #browser()
  margs <- flatten(mapping$marginals)
  alles <- rev(margs[sapply(margs, function(i) i !=1)])
  alles
}

#' TODO
#' @param mapping probability mapping TODO
#' @return TODO: all conditionals in the mapping
#'
get_conds <- function(mapping){
  #browser()
  margs <- flatten(mapping$marginals)
  cond_inx <- sapply(margs, function(i) i == 1)
  rev(flatten(mapping$conditionals[cond_inx]))

}



add_coord_aes <- function(prob_mtx, coord_aes){
  #browser()
  for (i in seq_along(coord_aes)){
    aes <- names(coord_aes)[[i]]
    pvar <- as.character(coord_aes[[i]])

    for (j in seq_len(nrow(prob_mtx))){
      # P(B|A) or P(1|A) ????
      marg <- as.character(prob_mtx$marginals[[j]][[1]])
      cond <- as.character(prob_mtx$conditionals[[j]][[1]])

      if (marg == pvar){
        supplied_aes <- prob_mtx$aes[[j]]
        prob_mtx$aes[[j]] <- paste(aes, ".", supplied_aes, sep = "")
        break
      } else if (((length(cond) > 0) && cond == pvar)){
        supplied_aes <- prob_mtx$aes[[j]]
        # #browser()
        if (is.list(supplied_aes))
          if (is.null(supplied_aes[[1]])){
            supplied_aes <- "cond"
            if (aes == "x" | aes == "y"){
              prob_mtx$aes[[j]] <- paste(aes, ".", supplied_aes, sep = "")
            } else {
              prob_mtx$aes[[j]] <- supplied_aes
            }
          } else {
            prob_mtx$aes[[j]] <- paste(aes, ".", supplied_aes, sep = "")
          }
        break
      }
    }
  }

  # check for underspecified, i.e. there's still NULL aesthetics mapping
  n_nulls <- sum(sapply(flatten(prob_mtx$aes),is.null))
  stopifnot(n_nulls == 0)

  prob_mtx

}




#' Helper function: add in the implied P(1|A), etc
#'
#' @param prob_mtx probability matrix as specified
#'
#' @return the matrix with probability aesthetics, including
#'
#'
complete_conditionals <- function(prob_mtx){
  #browser()
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
#' @param m rows: pmf terms, cols: marginals, conditionals, aesthetics names
#'
mtx_check <- function(m){

  #browser()
  for (i in seq_len(nrow(m))){
    cond <- m[i, 2][[1]]
    marg <- m[i, 1][[1]]

    if (i < nrow(m)){
      next_cond <- m[i + 1, 2][[1]]
      combi <- c(cond, marg)
      if (!setequal(next_cond, combi)){
        return(FALSE)
      }
    }
  }
  TRUE
}


#' Helper function
#'
#' @param mapping TODO: aesthetics mapping?
#'
#' @return a dataframe of aes:variables
#'
aes_to_mtx <- function(mapping){

  #browser()

  mtx_nrow <- length(mapping)
  mtx_ncol <- 3 # marginal, cond, aes name

  mtx <- matrix(list(NULL), nrow = mtx_nrow, ncol = mtx_ncol)
  # mtx <- data.frame(mtx)

  aes_names <- names(mapping)

  for (i in seq_len(mtx_nrow)){

    # marginal
    # m <- as.character(i)
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
  #browser()
  # dimension is right
  aes_dimension <- 1
  if (aes == "area"){
    aes_dimension <- 2
  }
  aes_dimension == length(marg)
}


filter_prob_aes <- function(aes_names, mapping){
  #browser()
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

  #browser()

  # parse out the list inside the little quosure
  for (i in seq_along(mapping)){
    # mapping[[1]]
    # <quosure>
    #   expr: ^A
    #   env:  0x11c0b6af8
    pair <- mapping[[i]]
    # mapping_expr get the expression part, and the length is how many expressions it contains,
    #e.g c(gear), contains one vecotr c() and data part gear
    mapping_expr <- quo_get_expr(pair)
    mapping_env <- quo_get_env(pair)
    if (length(mapping_expr) > 1){
      if (as.character(mapping_expr[[1]]) == "c"){
        list_of_Ps <- c()
        for (j in seq_along(mapping_expr)){
          if (j != 1){ # skip the c()
            # TODO: turn it back to a quosure?
            list_of_Ps <- c(mapping_expr[[j]],list_of_Ps)
          }
        }
        # quo_set_expr(mapping[[i]]) <- mapping_expr[2:length(mapping_expr)]
        mapping[[i]] <- list_of_Ps
      } else {
        # could be `factor(cyl)`
        mapping[[i]] <- list(mapping_expr[[2]])
      }
    } else {
      mapping[[i]] <- list(mapping_expr)
    }
  }


  new_mapping <- unlist(mapping)
  aes_names <- names(mapping)
  new_names <- character()

  for (i in seq_along(mapping)){
    for (j in seq_along(mapping[[i]])){
      new_names <- c(new_names, aes_names[[i]])
    }
  }
  names(new_mapping) <- new_names
  list(mapping=new_mapping, env = mapping_env)

}


