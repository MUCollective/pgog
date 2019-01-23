context("parse")
library(rlang)


test_that("flatten_aes", {
  mapping <- list()
  mapping$width <- list(expr(P(A)), expr(P(B|A)))
  mapping$x <- list(expr(A))


  flatten_aes(mapping)

  expect_output(flatten_aes(mapping), "") # TODO

})
