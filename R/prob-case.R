#' @return three types of density functions {joint, fill, count}
#'
#' 1. marg = A, cond=NULL, joint (`density` or `scale` in `compute_densities()`)
#' 2. marg = B, cond=A (continuous), equiv to `count` column and `position = fill`
#' 3. marg = A, cond=B (discrete), equiv to `count` column
#' 4. area plot cases
#' 5. other edge cases, prob throw em out
assign_prob_case <- function(data, prob.struct){

  marg_var <- sapply(get_margs(prob.struct), as.character)
  if (!(is.list(marg_var) & length(marg_var) == 0)) {
    marg_var <- paste0("p.", marg_var)
  } else {
    marg_var <- c()
  }

  cond_var <- sapply(get_conds(prob.struct), as.character)
  if (!(is.list(cond_var) & length(cond_var) == 0)){
    cond_var <- paste0("p.", cond_var)
  } else {
    cond_var <- c()
  }


  # check if these variables are continuous
  all_rvs <- unique(c(marg_var, cond_var))
  is_continuous <- function(i) ! is.factor(i)
  rv_levels <- sapply(as.tibble(data[, all_rvs]), is_continuous)
  has_too_many_levels <- sum(rv_levels)


  browser()



  # scenario 1, joint
  if (is_empty(cond_var)){
    return("joint")
  } else {
    # scenario 2 and 3
    # is the cond continuous?
    # criteria, is this variable a factor?
    using_factor <- TRUE

    # look up the variable in data

    if (using_factor){
      if (is.factor(cond)){ # discrete B
        return("count")
      } else { # continous A
        return("fill")
      }
    } else {
      stop("TODO")
    }
  }
}

# https://github.com/MUCollective/pgog/issues/76

