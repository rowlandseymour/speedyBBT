# Unit tests for comparisons_to_matrix function
test_that("comparisons_to_matrix constructs correct win matrix", {

  # Test case 1: Basic functionality
  comparisons <- data.frame("winner" = c(1, 3, 2, 2), "loser" = c(3, 1, 1, 3))
  expected_matrix <- matrix(c(0, 1, 1,
                              0, 0, 0,
                              1, 1, 0), nrow = 3, byrow = TRUE)

  result_matrix <- comparisons_to_matrix(3, comparisons)

  expect_equal(result_matrix, expected_matrix)

  # Test case 2: No comparisons
  comparisons_empty <- data.frame("winner" = numeric(0), "loser" = numeric(0))
  expected_matrix_empty <- matrix(0, 3, 3)

  result_matrix_empty <- comparisons_to_matrix(3, comparisons_empty)

  expect_equal(result_matrix_empty, expected_matrix_empty)

  # Test case 3: Single comparison
  comparisons_single <- data.frame("winner" = c(2), "loser" = c(1))
  expected_matrix_single <- matrix(c(0, 1, 0,
                                     0, 0, 0,
                                     0, 0, 0), nrow = 3, byrow = TRUE)

  result_matrix_single <- comparisons_to_matrix(3, comparisons_single)

  expect_equal(result_matrix_single, expected_matrix_single)
})



library(testthat)
library(Matrix)

# Unit tests for construct.design.matrix function
test_that("construct.design.matrix constructs correct design matrix", {

  # Test case 1: Design matrix with 2 objects
  expected_matrix <-  Matrix::sparseMatrix(i = c(1, 1),
                                    j = c(1, 2),
                                    x = c(1, -1),
                                    dims = c(1, 2))

  result_matrix <- construct.design.matrix(2)

  expect_equal(result_matrix, expected_matrix)

  # Test case 2: Design matrix with 3 objects
  expected_matrix_3 <-  Matrix::sparseMatrix(i = c(1, 1, 2, 2, 3, 3),
                                    j = c(1, 2, 1, 3, 2, 3),
                                    x = c(1, -1, 1, -1, 1, -1),
                                    dims = c(3, 3))

  result_matrix_3 <- construct.design.matrix(3)

  expect_equal(result_matrix_3, expected_matrix_3)

  # Test case 3: Design matrix with 4 objects
  expected_matrix_4 <-  Matrix::sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
                                    j = c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4),
                                    x = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
                                    dims = c(6, 4))

  result_matrix_4 <- construct.design.matrix(4)

  expect_equal(result_matrix_4, expected_matrix_4)
})



library(testthat)
library(Matrix)

# Unit tests for construct.design.matrix.both.ways function
test_that("construct.design.matrix.both.ways constructs correct design matrix", {

  # Test case 1: Design matrix with 2 objects
  expected_matrix_2 <- Matrix::sparseMatrix(i = c(1, 1, 2, 2),
                                    j = c(1, 2, 2, 1),
                                    x = c(1, -1, 1, -1),
                                    dims = c(2, 2))

  result_matrix_2 <- construct.design.matrix.both.ways(2)

  expect_equal(result_matrix_2, expected_matrix_2)

})





library(testthat)

# Unit tests for construct.generalised.design.matrix function
test_that("construct.generalised.design.matrix constructs correct design matrix", {

  # Test case 1: Simple case with linear covariates
  example.df <- data.frame("a" = c(1, 2, 3), "b" = c(2, 3, 1))
  example.formula <- ~ a + b


  player1 <- c(1, 3, 2)
  player2 <- c(3, 1, 1)

  expected_matrix <- matrix(c(-2,  1,
                               2, -1,
                              1,  1), ncol = 2, byrow = TRUE)

  result_matrix <- construct.generalised.design.matrix(player1, player2, example.formula, example.df)

  expect_equal(result_matrix, expected_matrix)

  # Test case 2: Including a quadratic term
  example.df <- data.frame("a" = c(1, 2, 3), "b" = c(2, 3, 1))
  example.formula <- ~ a + b + I(a^2)

  player1 <- c(1, 3)
  player2 <- c(3, 2)

  expected_matrix <- matrix(c(-2, 1, -8,
                              1, -2, 5), ncol = 3, byrow = TRUE)

  result_matrix <- construct.generalised.design.matrix(player1, player2, example.formula, example.df)

  expect_equal(result_matrix, expected_matrix)

  # Test case 3: Edge case with a single comparison
  example.df <- data.frame("a" = c(1, 2), "b" = c(2, 3))
  example.formula <- ~ a

  player1 <- c(1)
  player2 <- c(2)

  expected_matrix <- matrix(c(-1), ncol = 1)

  result_matrix <- construct.generalised.design.matrix(player1, player2, example.formula, example.df)

  expect_equal(result_matrix, expected_matrix)

  # Test case 4: Multiple comparisons with identical players
  example.df <- data.frame("a" = c(1, 1, 1), "b" = c(2, 2, 2))
  example.formula <- ~ a + b

  player1 <- c(1, 2, 3)
  player2 <- c(2, 3, 1)

  expected_matrix <- matrix(c(0, 0,
                              0, 0,
                              0, 0), ncol = 2, byrow = TRUE)

  result_matrix <- construct.generalised.design.matrix(player1, player2, example.formula, example.df)

  expect_equal(result_matrix, expected_matrix)
})


library(testthat)
library(Matrix)

# Unit tests for construct.design.matrix.by.comparison function
test_that("construct.design.matrix.by.comparison constructs correct design matrix", {

  # Test case 1: Simple case with 3 objects and 4 comparisons
  object1 <- c(1, 3, 2, 1)
  object2 <- c(3, 1, 1, 2)

  expected_matrix <- Matrix::sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4),
                                  j = c(1, 3, 3, 1, 2, 1, 1, 2),
                                  x = c(1, -1, 1, -1, 1, -1, 1, -1),
                                  dims = c(4, 3))

  result_matrix <- construct.design.matrix.by.comparison(object1, object2)

  expect_equal(result_matrix, expected_matrix)

  # Test case 2: Case with repeated comparisons
  object1 <- c(2, 2, 3, 3)
  object2 <- c(1, 1, 1, 1)

  expected_matrix <- Matrix::sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4),
                                  j = c(2, 1, 2, 1, 3, 1, 3, 1),
                                  x = c(1, -1, 1, -1, 1, -1, 1, -1),
                                  dims = c(4, 3))

  result_matrix <- construct.design.matrix.by.comparison(object1, object2)

  expect_equal(result_matrix, expected_matrix)

  # Test case 3: Edge case with a single comparison
  object1 <- c(1)
  object2 <- c(2)

  expected_matrix <- Matrix::sparseMatrix(i = c(1, 1),
                                  j = c(1, 2),
                                  x = c(1, -1),
                                  dims = c(1, 2))

  result_matrix <- construct.design.matrix.by.comparison(object1, object2)

  expect_equal(result_matrix, expected_matrix)

  # Test case 4: Case with identical objects in comparison (should yield a row of zeros)
  object1 <- c(1, 2, 3)
  object2 <- c(1, 2, 3)

  expected_matrix <- Matrix::sparseMatrix(i = c(1, 2, 3),
                                  j = c(1, 2, 3),
                                  x = c(0, 0, 0),
                                  dims = c(3, 3))

  result_matrix <- construct.design.matrix.by.comparison(object1, object2)

  expect_equal(result_matrix, expected_matrix)

  # Test case 5: Case with more than 3 objects
  object1 <- c(1, 4, 3, 2)
  object2 <- c(4, 1, 2, 3)

  expected_matrix <- Matrix::sparseMatrix(i = c(1, 1, 2, 2, 3, 3, 4, 4),
                                  j = c(1, 4, 4, 1, 3, 2, 2, 3),
                                  x = c(1, -1, 1, -1, 1, -1, 1, -1),
                                  dims = c(4, 4))

  result_matrix <- construct.design.matrix.by.comparison(object1, object2)

  expect_equal(result_matrix, expected_matrix)

})





