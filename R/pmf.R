#' @param p a single expression like P(A, B | C, D)
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
parse_pmfs <- function(length, area){


}


#' tries to merge a list of pmfs
#' @return a list of
combine_pmfs <- function(marginals, conditionals){

}
