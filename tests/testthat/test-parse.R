context("parse")
library(rlang)



mapping <- list()
mapping$width <- list(expr(P(B|A)), expr(P(A)), expr(P(C|A)))
mapping$x <- list(expr(A))

# test_that("flatten_aes", {
#
#   expect_output(flatten_aes(mapping), "") # TODO
#
# })


test_that("parse_aes", {
  expect_output(parse_aes(mapping), "")

})
