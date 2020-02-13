context("squeezing the rectangle with bounds given")
#library(purrr)
#library(rlang)



test_that("squeeze a unit square on a unit bound", {
  bounds = data.frame(l = c(0),r = c(1),b = c(0), t = c(1))
  location = data.frame(l = c(0),r = c(1),b = c(0), t = c(1))
  result = squeeze(location, bounds)
  expect_equal(result$l, 0)
  expect_equal(result$r, 1)
  expect_equal(result$b, 0)
  expect_equal(result$t, 1)
})

test_that("squeeze a rectangle on a unit bound", {
  bounds = data.frame(l = c(0),r = c(1),b = c(0), t = c(1))
  location = data.frame(l = c(0),r = c(0.5),b = c(0), t = c(1))
  result = squeeze(location, bounds)
  expect_equal(result$l, 0)
  expect_equal(result$r, 0.5)
  expect_equal(result$b, 0)
  expect_equal(result$t, 1)
})

test_that("squeeze a rectangle on an non-unit bound", {
  bounds = data.frame(l = c(0),r = c(2),b = c(0), t = c(0.5))
  location = data.frame(l = c(0),r = c(0.5),b = c(0), t = c(1))
  result = squeeze(location, bounds)
  expect_equal(result$l, 0)
  expect_equal(result$r, 1)
  expect_equal(result$b, 0)
  expect_equal(result$t, 0.5)
})

test_that("squeeze multiple rectangle on an unit bound", {
  bounds = data.frame(l = c(0),r = c(1),b = c(0), t = c(1))
  location = data.frame(l = c(0,0.5),r = c(0.5,1),b = c(0,0), t = c(1,1))
  result = squeeze(location, bounds)
  expect_equal(result$l, c(0,0.5))
  expect_equal(result$r, c(0.5,1))
  expect_equal(result$b, c(0,0))
  expect_equal(result$t, c(1,1))
})

test_that("squeeze multiple rectangle on a non-unit bound", {
  bounds = data.frame(l = c(0),r = c(2),b = c(0), t = c(0.5))
  location = data.frame(l = c(0,0.5),r = c(0.5,1),b = c(0,0), t = c(1,1))
  result = squeeze(location, bounds)
  expect_equal(result$l, c(0,1))
  expect_equal(result$r, c(1,2))
  expect_equal(result$b, c(0,0))
  expect_equal(result$t, c(0.5, 0.5))
})

test_that("squeeze multiple stacked rectangle on a non-unit bound", {
  bounds = data.frame(l = c(0),r = c(2),b = c(0), t = c(0.5))
  location = data.frame(l = c(0,0.5,0,0.5),r = c(0.5,1,0.5,1),b = c(0,0,0.6,0.4), t = c(0.6,0.4,1,1))
  result = squeeze(location, bounds)
  expect_equal(result$l, c(0,1,0,1))
  expect_equal(result$r, c(1,2,1,2))
  expect_equal(result$b, c(0,0,0.3,0.2))
  expect_equal(result$t, c(0.3,0.2,0.5, 0.5))
})

test_that ("aes checker 1",{
  aes = c("x.height","width","y.cond","width.color")
  result = c()
  for(i in 1:length(aes)){
  result = c(result,aes_checker(aes[i]))
  }
  expect_equal(result,c("x.height","width","y.cond","width"))
})


test_that ("aes checker 2",{
  aes = c("x.height","width","y.cond","color")
  result = c()
  for(i in 1:length(aes)){
    result = c(result,aes_checker(aes[i]))
  }
  expect_equal(result,c("x.height","width","y.cond",NA))
})

test_that ("aes checker 3",{
  aes = c("x.height","width","y.cond","fill.width.color")
  result = c()
  for(i in 1:length(aes)){
    result = c(result,aes_checker(aes[i]))
  }
  expect_equal(result,c("x.height","width","y.cond","width"))
})

test_that ("aes lookup 1",{
  aes = c("x.height","width","y.cond","fill.width.color")
  result = c()
  for(i in 1:length(aes)){
    result = c(result,aes_lookup(aes[i]))
  }
  expect_equal(result,c(eval(hbar),eval(hspine),eval(vspine),eval(hspine)))
})
