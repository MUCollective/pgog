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
P <- function(x) {
  require(rlang)
  e <- rlang::env(
    caller_env(),
    # override | in this environment
    # https://adv-r.hadley.nz/meta.html
    `|` = function(a, b) {
      # browser()
      len <- length(a)
      cond_unnormalized <- a[as.logical(b)]

      # secretly rescale so that check_aesthetics(evaled, n) won't complain
      tbl <- table(cond_unnormalized)
      counts <- sum(tbl)
      factor <- len %/% counts
      c(rep(names(tbl)[1], times = factor * tbl[1]), rep(names(tbl)[2], times = len - factor * tbl[1]))
    }
  )
  # browser()

  args <- enexpr(x)
  eval(args, e)
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

