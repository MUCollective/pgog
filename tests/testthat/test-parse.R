context("parse")
library(rlang)


# legit mapping

m1 <- list()
m1$width <- list(expr(P(B|A)), expr(P(A)), expr(P(C|A,B)))
m1$x <- list(expr(A))

# bad mapping
m2 <- m1
m2$width <- list(expr(P(A)), expr(P(C)))


# conditinoal pmf
m3 <- list()
m3$area <- list(expr(P(C, B|A, D)))
m3$x <- list(expr(A))
m3$y <- list(expr(D))

# test_that("parse_aes succeed", {
#
#   expect_null(parse_aes(m1))
#
# })
#
#
# test_that("parse_aes fail", {
#   expect_error(parse_aes(m2))
#
# })



test_that("parse_aes conditional pmf", {
  expect_null(parse_aes(m3))
})
