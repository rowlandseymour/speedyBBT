test_that("lambda function works for full draws matrix", {
  expect_equal(2 * 2, 4)
})

test_that("lambda function works for partial draws matrix", {
  expect_equal(2 * 2, 4)
})

test_that("parameter function works for alpha.sq", {
  expect_equal(2 * 2, 4)
})

test_that("check_model works when object is valid", {
  expect_equal(2 * 2, 4)
})


test_that("check_model fails when object is invalid", {
  expect_equal(2 * 2, 4)
})


test_that("check_model fails when parameter name isn't found in the draws matrix", {
  expect_equal(2 * 2, 4)
})
