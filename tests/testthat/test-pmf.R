context("test-pmf")
library(purrr)

test_that("P(B|A) P(A) P(C|A, B)", {
  exprs <- c(expr(P(B|A)), expr(P(A)), expr(P(C|A, B)))

  res <- parse_pmfs(one_d = exprs)

  expect_equal(res, "B+A+C")
})




test_that("P(C|A, B) P(A) P(B|A)", {
  exprs <- c(expr(P(B|A)), expr(P(A)), expr(P(C|A, B)))

  res <- parse_pmfs(one_d = exprs)

  expect_equal(res, "B+A+C")
})


test_that("P(A) P(C)", {
  exprs <- c(expr(P(A)), expr(P(C)))

  expect_error(parse_pmfs(one_d = exprs))
})
