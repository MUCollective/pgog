# From ggplot

# Name ggplot grid object
# Convenience function to name grid objects
#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# swap_mapping <- function(mapping, aesthetics, new_aes, new_expr){
#
#   # change expr
#   old_quosure <- mapping[[(aesthetics)]]
#   old_expr <- quo_get_expr(old_quosure)
#   old_expr[[1]] <- new_expr
#   mapping[[(aesthetics)]] <- quo_set_expr( mapping[[(aesthetics)]], old_expr)
#
#   replaced <- replace(names(mapping), match(as.character(aesthetics), new_aes))
#   names(mapping) <- replaced
#
#   mapping
# }

# walks the ast to find what variable it's conditioned on
# P(_|B), find B
get_conditional <- function(e){
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

  l <- get_conditional_recur(e)
  l[[length(l)]]

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

