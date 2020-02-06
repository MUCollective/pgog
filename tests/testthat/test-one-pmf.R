context("parsing-one-mpf")
library(purrr)
library(rlang)

# TODO: test things like "P(A|A)", "P(A, B)"
test_that("P(A,B)", {
  exprs <- expr(P(A,B))
  res <- parse_pmf(exprs)
  expect_equal(res, list(marginals = c(expr(A), expr(B))))
})

test_that("P(A,A)", {
  exprs <- expr(P(A,A))
  res <- parse_pmf(exprs)
  expect_equal(res, list(marginals = c(expr(A), expr(A))))
})

test_that("P(A)", {
  exprs <- expr(P(A))
  res <- parse_pmf(exprs)
  expect_equal(res, list(marginals = expr(A)))
})

test_that("P(A|B)", {
  exprs <- expr(P(A|B))
  res <- parse_pmf(exprs)
  expect_equal(res, list(marginals = list(expr(A)),conditionals = list(expr(B))))
})

