
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
        browser()

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
print.prob_chain <- function(aes_mtx){


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


#' @references https://github.com/tidyverse/ggplot2/blob/660aad2db2b3495ae0d8040915a40d247133ffc0/R/utilities.r
has_flipped_aes <- function(data, params = list(), main_is_orthogonal = NA,
                            range_is_orthogonal = NA, group_has_equal = FALSE,
                            ambiguous = FALSE, main_is_continuous = FALSE) {
  # Is orientation already encoded in data?
  if (!is.null(data$flipped_aes)) {
    not_na <- which(!is.na(data$flipped_aes))
    if (length(not_na) != 0) {
      return(data$flipped_aes[[not_na[1L]]])
    }
  }

  # Is orientation requested in the params
  if (!is.null(params$orientation) && !is.na(params$orientation)) {
    return(params$orientation == "y")
  }

  x <- data$x %||% params$x
  y <- data$y %||% params$y
  xmin <- data$xmin %||% params$xmin
  ymin <- data$ymin %||% params$ymin
  xmax <- data$xmax %||% params$xmax
  ymax <- data$ymax %||% params$ymax

  # Does a single x or y aesthetic corespond to a specific orientation
  if (!is.na(main_is_orthogonal) && xor(is.null(x), is.null(y))) {
    return(is.null(y) == main_is_orthogonal)
  }

  has_x <- !is.null(x)
  has_y <- !is.null(y)

  # Does a provided range indicate an orientation
  if (!is.na(range_is_orthogonal)) {
    if (!is.null(ymin) || !is.null(ymax)) {
      return(!range_is_orthogonal)
    }
    if (!is.null(xmin) || !is.null(xmax)) {
      return(range_is_orthogonal)
    }
  }

  # If ambiguous orientation = NA will give FALSE
  if (ambiguous && (is.null(params$orientation) || is.na(params$orientation))) {
    return(FALSE)
  }

  # Is there a single actual discrete position
  y_is_int <- is.integer(y)
  x_is_int <- is.integer(x)
  if (xor(y_is_int, x_is_int)) {
    return(y_is_int != main_is_continuous)
  }

  # Does each group have a single x or y value
  if (group_has_equal) {
    if (has_x) {
      if (length(x) == 1) return(FALSE)
      x_groups <- vapply(split(data$x, data$group), function(x) length(unique(x)), integer(1))
      if (all(x_groups == 1)) {
        return(FALSE)
      }
    }
    if (has_y) {
      if (length(y) == 1) return(TRUE)
      y_groups <- vapply(split(data$y, data$group), function(x) length(unique(x)), integer(1))
      if (all(y_groups == 1)) {
        return(TRUE)
      }
    }
  }

  # give up early
  if (!has_x && !has_y) {
    return(FALSE)
  }

  # Both true discrete. give up
  if (y_is_int && x_is_int) {
    return(FALSE)
  }
  # Is there a single discrete-like position
  y_is_int <- if (has_y) isTRUE(all.equal(y, round(y))) else FALSE
  x_is_int <- if (has_x) isTRUE(all.equal(x, round(x))) else FALSE
  if (xor(y_is_int, x_is_int)) {
    return(y_is_int != main_is_continuous)
  }
  # Is one of the axes a single value
  if (all(x == 1)) {
    return(main_is_continuous)
  }
  if (all(y == 1)) {
    return(!main_is_continuous)
  }
  # If both are discrete like, which have most 0 or 1-spaced values
  y_diff <- diff(sort(y))
  x_diff <- diff(sort(x))

  if (y_is_int && x_is_int) {
    return((sum(x_diff <= 1) < sum(y_diff <= 1)) != main_is_continuous)
  }

  y_diff <- y_diff[y_diff != 0]
  x_diff <- x_diff[x_diff != 0]

  # If none are discrete is either regularly spaced
  y_is_regular <- if (has_y && length(y_diff) != 0) all(suppressWarnings((y_diff / min(y_diff)) %% 1) < .Machine$double.eps) else FALSE
  x_is_regular <- if (has_x && length(x_diff) != 0) all(suppressWarnings((x_diff / min(x_diff)) %% 1) < .Machine$double.eps) else FALSE
  if (xor(y_is_regular, x_is_regular)) {
    return(y_is_regular != main_is_continuous)
  }

  # default to no
  FALSE
}

flip_data <- function(data, flip = NULL) {
  flip <- flip %||% data$flipped_aes[1] %||% FALSE
  if (flip) {
    names(data) <- switch_orientation(names(data))
  }
  data
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

#' @references https://github.com/tidyverse/ggplot2/blob/660aad2db2b3495ae0d8040915a40d247133ffc0/R/compat-plyr.R
#' Apply function to unique subsets of a data.frame
#'
#' This function is akin to `plyr::ddply`. It takes a single data.frame,
#' splits it by the unique combinations of the columns given in `by`, apply a
#' function to each split, and then reassembles the results into a sigle
#' data.frame again.
#'
#' @param df A data.frame
#' @param by A character vector of column names to split by
#' @param fun A function to apply to each split
#' @param ... Further arguments to `fun`
#' @param drop Should unused factor levels in the columns given in `by` be
#' dropped.
#'
#' @return A data.frame if the result of `fun` does not include the columns
#' given in `by` these will be prepended to the result.
#'
#' @keywords internal
#' @noRd
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  ids <- id(grouping_cols, drop = drop)
  group_rows <- split(seq_len(nrow(df)), ids)
  fallback_order <- unique(c(by, names(df)))
  rbind_dfs(lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    res <- fun(cur_data, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(new_data_frame())
    vars <- lapply(setNames(by, by), function(col) .subset2(cur_data, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(new_data_frame(unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    new_data_frame(res[intersect(c(fallback_order, names(res)), names(res))])
  }))
}



rbind_dfs <- function(dfs) {
  out <- list()
  columns <- unique(unlist(lapply(dfs, names)))
  nrows <- vapply(dfs, .row_names_info, integer(1), type = 2L)
  total <- sum(nrows)
  if (length(columns) == 0) return(new_data_frame(list(), total))
  allocated <- rep(FALSE, length(columns))
  names(allocated) <- columns
  col_levels <- list()
  for (df in dfs) {
    new_columns <- intersect(names(df), columns[!allocated])
    for (col in new_columns) {
      if (is.factor(df[[col]])) {
        all_factors <- all(vapply(dfs, function(df) {
          val <- .subset2(df, col)
          is.null(val) || is.factor(val)
        }, logical(1)))
        if (all_factors) {
          col_levels[[col]] <- unique(unlist(lapply(dfs, function(df) levels(.subset2(df, col)))))
        }
        out[[col]] <- rep(NA_character_, total)
      } else {
        out[[col]] <- rep(.subset2(df, col)[1][NA], total)
      }
    }
    allocated[new_columns] <- TRUE
    if (all(allocated)) break
  }
  is_date <- lapply(out, inherits, 'Date')
  is_time <- lapply(out, inherits, 'POSIXct')
  pos <- c(cumsum(nrows) - nrows + 1)
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    rng <- seq(pos[i], length.out = nrows[i])
    for (col in names(df)) {
      date_col <- inherits(df[[col]], 'Date')
      time_col <- inherits(df[[col]], 'POSIXct')
      if (is_date[[col]] && !date_col) {
        out[[col]][rng] <- as.Date(
          unclass(df[[col]]),
          origin = ggplot_global$date_origin
        )
      } else if (is_time[[col]] && !time_col) {
        out[[col]][rng] <- as.POSIXct(
          unclass(df[[col]]),
          origin = ggplot_global$time_origin
        )
      } else if (date_col || time_col || inherits(df[[col]], 'factor')) {
        out[[col]][rng] <- as.character(df[[col]])
      } else {
        out[[col]][rng] <- df[[col]]
      }
    }
  }
  for (col in names(col_levels)) {
    out[[col]] <- factor(out[[col]], levels = col_levels[[col]])
  }
  attributes(out) <- list(
    class = "data.frame",
    names = names(out),
    row.names = .set_row_names(total)
  )
  out
}

#' @references https://github.com/tidyverse/ggplot2/blob/660aad2db2b3495ae0d8040915a40d247133ffc0/R/performance.R
df_rows <- function(x, i) {
  new_data_frame(lapply(x, `[`, i = i))
}
