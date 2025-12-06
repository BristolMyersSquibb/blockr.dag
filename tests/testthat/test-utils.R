test_that("last returns the last element", {
  expect_equal(last(c(1, 2, 3)), 3)
  expect_equal(last(c("a", "b", "c")), "c")
  expect_equal(last(list(1, "a", TRUE)), TRUE)
  expect_equal(last(1), 1)
})

test_that("filter_null removes NULL elements", {
  expect_equal(filter_null(list(1, NULL, 3)), list(1, 3))
  expect_equal(filter_null(list(NULL, NULL)), list())
  expect_equal(filter_null(list(1, 2, 3)), list(1, 2, 3))
  expect_equal(filter_null(list()), list())
  expect_equal(filter_null(list(a = 1, b = NULL, c = 3)), list(a = 1, c = 3))
})

test_that("has_length checks if object has elements", {
  expect_true(has_length(c(1, 2, 3)))
  expect_true(has_length(1))
  expect_true(has_length("a"))
  expect_false(has_length(c()))
  expect_false(has_length(list()))
  expect_false(has_length(NULL))
})
