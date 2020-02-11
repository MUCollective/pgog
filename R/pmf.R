#' parses a single pmf
#' @param p a single expression like P(A, B | C, D)
#' @return a list of marginals and conditionals
parse_pmf <- function(p){
  l <- as.character(p)
  l <- l[2:length(l)]
  char_l <- as.character(l)
  char_l <- paste(char_l, collapse = "+")
  p_expr <- parse(text = char_l)[[1]]
  sym_l <- get_conditional_recur(p_expr)
  is_conditional = FALSE

  if (length(sym_l) == 1){
    return(list(marginals = sym_l))
  }

  if (sym_l[[1]] == as.symbol("|")) {
     sym_l <- sym_l[2:length(sym_l)] # remove `|`
     is_conditional = TRUE
  }

  marg_idx = -1
  conds_idx = -1
  lhs = TRUE
  i = 1

  while (sym_l[[i]] == as.symbol("+")) {
    i = i + 1
  }

  marg_idx = (i):(i + i - 1)

  if(is_conditional){
    i = tail(marg_idx, n = 1) + 1
    conds_count = 1
    while(sym_l[[i]] == as.symbol("+")){
      i = i + 1
      conds_count = conds_count + 1
    }
    conds_idx = i:length(sym_l)
    list(marginals = sym_l[marg_idx] , conditionals = sym_l[conds_idx])

  } else {
    stopifnot(marg_idx != -1)
    list(marginals = sym_l[marg_idx])
  }
}


#' each pmf is an expression like `P(A, B| C)`
#' @param length a list of pmfs assigned to the length aesthetics
#' @param area a list of pmfs assigned to the area aesthetics
#' @return marginals and conditionals
parse_pmfs <- function(one_d = NULL, two_d = NULL){
  # number of terms in the aesthetics
  res <- list()
  res$length = length(one_d) + length(two_d)
  res$marginals <- list()
  res$conditionals <- list()


  if (!is.null(one_d)){
    all <- one_d
    if (!is.null(two_d)){
      all <- paste(all, two_d)
    }
  } else {
    stopifnot(!is.null(two_d))
    all <- two_d
  }


  for (i in 1:length(all)){
    res$marginals <- c(res$marginal, parse_pmf(all[[i]])$marginals)

    cond <- parse_pmf(all[[i]])$conditionals
    if (! is.null(cond)){
      res$conditionals <- c(res$conditionals,cond)
    }
  }


  combine_pmfs(res)


}

pprint_pmf <- function(res){


  s <- paste("", paste(res$marginals, collapse = "+"), sep = "")
  if (length(res$conditionals) != 0){
    s <- paste(s, "|", paste(res$conditionals, collapse = "+"), sep = "")
  }

  s
}

#' tries to merge a list of pmfs
#' @param res a list of expressions like A, B, C
#' @return a list of
#' @importFrom dplyr intersect
combine_pmfs <- function(res){

  common_rv <- intersect(res$marginals, res$conditionals)

  stopifnot(res$length >1 & length(common_rv)  != 0) #TODO: >2 pmfs?

  # #browser()
  # res$marginals <- c(res$marginals, common_rv) # this is wrong
  rv_idx <- which(res$conditionals %in% common_rv )
  res$conditionals <- res$conditionals[-rv_idx]
  res$conditionals <- unique(res$conditionals)

  pprint_pmf(res)
}
