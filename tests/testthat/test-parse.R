context("parse.R")
library(rlang)

# legit mapping

# TODO: write more cases

# bad mapping
m1 = aes(height = c(P(cyl|gear)), x = c(gear))
m2 = aes(height = c(P(cyl)), x =c(gear))
m3 = aes(height = c(P(cyl|gear, carb)), x = c(gear), y=c(carb))
m4 = aes(x = c(mpg), height = c(P(cyl | mpg)), fill = factor(cyl))
prob_aes_names = c("width", "height", "area")
coord_aes_names = c("x", "y", "alpha", "color", "colour", "fill")
# TODO: write more cases


test_that("parse_aes pmf 1", {
  expect_named(parse_aes(m1),c("marginals","conditionals","aes","level"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 2", {
  expect_named(parse_aes(m4),c("marginals","conditionals","aes","level"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 3", {
  expect_named(parse_aes(m3),c("marginals","conditionals","aes","level"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 4", {
  expect_equal(flatten_aes(m1)$mapping[[1]],quote(gear),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 5", {
  expect_equal(flatten_aes(m1)$mapping[[2]],quote(P(cyl | gear)),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 6", {
  expect_equal(flatten_aes(m3)$mapping[[3]],quote(P(cyl|gear, carb)),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 7", {
  expect_equal(flatten_aes(m3)$mapping$height[2],quote((cyl | gear)()),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 8", {
  expect_equal(flatten_aes(m3)$mapping$height[[2]],quote(cyl | gear),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 9", {
  expect_equal(flatten_aes(m3)$mapping$height[[3]],quote(carb),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 10", {
  expect_equal(flatten_aes(m3)$mapping[[2]],quote(carb),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 11", {
  expect_equal(flatten_aes(m3)$mapping$height[[1]],quote(P),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 12", {
  expect_equal(flatten_aes(m3)$mapping$height[1],quote(P()),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("prob_aes 1", {
  expect_equal(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping)[[1]],quote(P(cyl | gear, carb)),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("prob_aes 2", {
  expect_equal(deparse(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping)$height),"P(cyl | gear, carb)",ignore.order = TRUE, ignore.case = TRUE)
})

test_that("prob_aes 3", {
  expect_equal(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping)$height[[2]],quote(cyl | gear),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("prob_aes 4", {
  expect_named(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping), "height",ignore.order = TRUE, ignore.case = TRUE)
})

test_that("coord_aes 1", {
  expect_named(filter_prob_aes(coord_aes_names,flatten_aes(m3)$mapping),c("x","y"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("coord_aes 2", {
  expect_equal(filter_prob_aes(coord_aes_names,flatten_aes(m3)$mapping)[[1]],quote(gear),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("aes_to_mtx 1", {
  expect_named(aes_to_mtx(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping)),c("marginals","conditionals","aes"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("aes_to_mtx 2", {
  expect_equal(aes_to_mtx(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping))[2],structure(list(conditionals = list(conditionals = list(quote(gear),quote(carb)))), row.names = 1L, class = "data.frame"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("aes_to_mtx 3", {
  expect_equal(aes_to_mtx(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping))[3],structure(list(aes = list(aes = "height")), row.names = 1L, class = "data.frame"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("aes_to_mtx 4", {
   expect_equal(aes_to_mtx(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping))[1],structure(list(marginals = list(marginals = list(quote(cyl)))), row.names = 1L, class = "data.frame"),ignore.order = TRUE, ignore.case = TRUE)
 })

test_that("complete_conditionals 1", {
  expect_named(complete_conditionals(aes_to_mtx(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping))),c("marginals","conditionals","aes"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("complete_conditionals 2", {
   expect_equal(complete_conditionals(aes_to_mtx(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping))),structure(list(marginals = list(marginals = list(1), marginals = list(1), marginals = list(quote(cyl))),
   conditionals = list(conditionals = list(quote(carb)), conditionals = list(quote(gear)), conditionals = list(quote(gear), quote(carb))), aes = list(aes = list(NULL), aes = list(NULL), aes = "height")), row.names = c(NA,-3L), class = "data.frame")
   ,ignore.order = TRUE, ignore.case = TRUE)
 })

 test_that("add_coord_aes 1", {
   expect_equal(add_coord_aes(complete_conditionals(aes_to_mtx(filter_prob_aes(prob_aes_names,flatten_aes(m3)$mapping))),filter_prob_aes(coord_aes_names,flatten_aes(m3)$mapping)),
   structure(list(marginals = list(marginals = list(quote(1)), marginals = list(quote(1)), marginals = list(quote(cyl))), conditionals = list(conditionals = list(quote(carb)),
   conditionals = list(quote(gear)), conditionals = list(quote(gear), quote(carb))), aes = list(aes = "y.cond", aes = "x.cond", aes = "height")), row.names = c(NA, -3L), class = "data.frame")
   ,ignore.order = TRUE, ignore.case = TRUE)
 })

test_that("elements_checker 1", {
  expect_error(elements_checker(filter_prob_aes(c("width", "height", "area"), flatten_aes(m2)$mapping), filter_prob_aes(c("x", "y", "alpha", "color", "colour", "fill"), flatten_aes(m2)$mapping)),
  "Parser failed due to miss matching", fixed=TRUE)
 })

test_that("elements_checker 2", {
  expect_error(elements_checker(filter_prob_aes(c("width", "height", "area"), flatten_aes(m3)$mapping),
                               filter_prob_aes(c("x", "y", "alpha", "color", "colour", "fill"), flatten_aes(m3)$mapping)),NA)
})






