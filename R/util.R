factor_w_h <- function(n){
  factor_test <- as.integer(sqrt(n))
  while (n %% factor_test != 0) {
    factor_test <- factor_test - 1
  }
  data.frame(w = max(factor_test, n/factor_test),
             h = min(factor_test, n/factor_test))
}




P <- function(...) {
  # eval_tidy(enexpr(x))

  e <- env(
    caller_env(),
    # TODO: override | in this environment
    `|` = function(a, b ) {
      a + b # TODO
    })

  args <- enexprs(...)
  # args <- ensyms(...)
  # eval(args, e)
  # browser()
  args

}
