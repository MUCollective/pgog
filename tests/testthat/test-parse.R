context("parse")
library(rlang)



mapping <- list()
mapping$width <- list(expr(P(A)), expr(P(B|A)))
mapping$x <- list(expr(A))

# test_that("flatten_aes", {
#
#   expect_output(flatten_aes(mapping), "") # TODO
#
# })


test_that("parse_aes", {
  expect_output(parse_aes(mapping), "")

})
