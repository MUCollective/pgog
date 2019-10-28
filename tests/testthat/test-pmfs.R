context("parsing-multiple-pmfs")
library(purrr)
library(rlang)

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




test_that("P(A) P(B | A)", {
  exprs <- c(expr(P(A)), expr(P(B|A)))
  res <- parse_pmfs(one_d = exprs)

  expect_equal(res, "B+A")
})

test_that(" P(B | A) P(A)", {
  exprs <- rev(c(expr(P(A)), expr(P(B|A))))
  res <- parse_pmfs(one_d = exprs)

  expect_equal(res, "B+A")
})


# these should fail



test_that("P(B|A) P(A) P(A|C)", {
  exprs <- c(expr(P(B|A)), expr( P(A)), expr( P(A|C)))

  expect_error(parse_pmfs(one_d = exprs))
})
