context("test-geom_primary_layout")
library(rlang)
library(tidyverse)

# m3 <- list()
# m3$height <- list(expr(P(C|A, D)))
# m3$x <- list(expr(A))
# m3$y <- list(expr(D))

# foo <- function(...){
#   rlang::expr(...)
# }


test_that("parsing things connect to ggplot", {

  res <- ggplot(mtcars) + geom_test(aes(height = c(P(C|A,D), P(A|D)), x = c(A,D)))
  expect_null()
})
