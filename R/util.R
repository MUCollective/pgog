
# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
#' @references https://github.com/tidyverse/ggplot2/blob/c84d9a075280d374892e5a3e0e25dd0ba246caad/R/performance.R
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}






# From ggplot

# Name ggplot grid object
# Convenience function to name grid objects
#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}


# From http://adv-r.had.co.nz/Expressions.html
make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)

  eval(call("function", args, body), env)
}


# ========== parse those P(B, .), P(B|.) expressions
get_conditional_recur <- function(x){
  if (is.atomic(x) || is.symbol(x) || is.expression(x)){
    x
  } else if (is.call(x) || is.pairlist(x)){
    unlist(lapply(x, get_conditional_recur))
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

get_joint <- function(e){
  l <- get_conditional_recur(e)
  l[[length(l) - 1]]
}

# walks the ast to find what variable it's conditioned on
# P(|_), find B
get_conditional <- function(e){
  l <- get_conditional_recur(e)
  # █─expr
  # └─█─P
  # └─█─`|`
  # ├─A
  # └─B
  # Want A, so the second last in the list
  if ("|" %in% sapply(l, as.character)){ #if P(B|A)
    l[[length(l) - 1]]
  } else { # if P(A)
    l[[length(l)]]
  }
}

is_joint <- function(e){
  l <- get_conditional_recur(e)

  length(l) > 2 && (!is_conditional(e))
}

is_conditional <- function(e){

  l <- get_conditional_recur(e)
  "|" %in% sapply(l, as.character)
}

mod_position <- function(aes_names){
  if ("x" %in% aes_names | "y" %in% aes_names){
    position_array(aes_names = aes_names)
  } else {
    "identity"
  }
}



# for position-icon

adjust <- function(ratio, idx, width=5){
  idx <- as.integer(idx) - 1
  seq(-0.5 * ratio, 0.5 * ratio, length.out = width)[idx %% width + 1]
}

# determines the width and height of icon array
factor_w_h <- function(n){
  factor_test <- as.integer(sqrt(n))
  while (n %% factor_test != 0) {
    factor_test <- factor_test - 1
  }
  data.frame(w = max(factor_test, n/factor_test),
             h = min(factor_test, n/factor_test))
}



## TODO: write functionality
## TODO: input/output format
#' @importFrom rlang env
#' @importFrom rlang caller_env
P <- function(x) {
    e <- rlang::env(
      caller_env(),
      # override | in this environment
      # https://adv-r.hadley.nz/meta.html
      `|` = function(a, b) {
        len <- length(a)
        cond_unnormalized <- a[as.logical(b)]

        # secretly rescale so that check_aesthetics(evaled, n) won't complain
        tbl <- table(cond_unnormalized)
        counts <- sum(tbl)
        factor <- len %/% counts

        # TODO: this does not consider three or more variables
        # TODO: this returns a char vec, not the original data type
        res <- c(rep(names(tbl)[1], times = factor * tbl[1]), rep(names(tbl)[2], times = len - factor * tbl[1]))
        # attributes(res)$is_conditional <- TRUE
        # browser()
        res
      }
    )
    # browser()

    args <- enexpr(x)
    is_conditional <- "|" %in% as.character(args)
    res <- eval(args, e)
    attributes(res)$is_conditional <- is_conditional
    # browser()
    res

}

discrete <- function (x) x

#' @importFrom ggplot2 stat
P1 <- function(x){
  expr(stat(width * density))
}

#' oops copied from P()
RandomVar <- P



# From geom_point.R
translate_shape_string <- function(shape_string) {
  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }

  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25
  )

  shape_match <- charmatch(shape_string, names(pch_table))

  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)

    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(5, n_bad)])

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    }

    stop(
      "Can't find shape name:",
      collapsed_names,
      more_problems,
      call. = FALSE
    )
  }

  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)

    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_table))),
      integer(1)
    )

    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    }

    stop(
      "Shape names must be unambiguous:",
      collapsed_names,
      more_problems,
      call. = FALSE
    )
  }

  unname(pch_table[shape_match])
}

#' prints pretty version of the aesthetics mapping
pprint <- function(aes_mtx){


  m <- matrix(nrow = nrow(aes_mtx), ncol = ncol(aes_mtx))


  # cat("\n")
  # cat(paste(c("marg", "cond", "aes", "level"), collapse = "\t"))
  # cat("\n")

  for (i in seq_len(nrow(aes_mtx))){
    for (j in seq_len(ncol(aes_mtx))){
      m[i, j] <- paste(aes_mtx[i,j][[1]], collapse = ",")

      # cat("\t")
      # for (k in seq_len(length(aes_mtx[i,j]))){
      # cat(paste(aes_mtx[i,j][[k]]))
      # cat("\t")
      # }
    }
    # cat("\n")
  }
  colnames(m) <- c("marg", "cond", "aes", "level")
  print(data.frame(m))
}


# ======== Advanced R ========

# expr_type <- function(x) {
#   if (rlang::is_syntactic_literal(x)) {
#     "constant"
#   } else if (is.symbol(x)) {
#     "symbol"
#   } else if (is.call(x)) {
#     "call"
#   } else if (is.pairlist(x)) {
#     "pairlist"
#   } else {
#     typeof(x)
#   }
# }
#
# switch_expr <- function(x, ...) {
#   switch(expr_type(x),
#          ...,
#          stop("Don't know how to handle type ", typeof(x), call. = FALSE)
#   )
# }
#

# cement(Good, time, name)
# #> [1] "Good time name"

# cement <- function(...) {
#   args <- ensyms(...)
#   paste(purrr::map(args, as_string), collapse = " ")
# }
#
#
# set_attr <- function(x, ...) {
#   browser()
#   attr <- rlang::list2(...)
#   attributes(x) <- attr
#   x
# }

