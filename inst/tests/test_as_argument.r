context("Test as_argument")

test_that("Argument transformation working as expected", {
  expect_equal(as_argument("p", TRUE), "-p")
  expect_equal(as_argument("-p", TRUE), "-p")
  expect_equal(as_argument("help", TRUE, mark = "--"), "--help")
  expect_equal(as_argument("--help", TRUE, mark = "--"), "--help")
  expect_equal(as_argument("op", c(1, 2), 2, mark = "--"), "--op 1 2")
  expect_equal(as_argument(NA, TRUE), "")
  expect_equal(as_argument("merged", FALSE), "")
  expect_equal(as_argument("op", NA) , "")
  expect_equal(as_argument("op", "123")  , "-op 123")
  expect_equal(as_argument("op", c(1, 2, 3)) , "-op 1 2 3")
  expect_equal(as_argument("op", list(c(1, 2, 3), c(4, 5, 6)), parrot = TRUE) , "-op 1 2 3 -op 4 5 6")
  expect_equal(as_argument("op", list(c(1, 2, 3), c(4, 5, 6)), parrot = FALSE) , "-op 1 2 3 4 5 6")
})

test_that("arguments are conform to expectations", {
  expect_error(as_argument("p", TRUE, 2))
  expect_error(as_argument("p", c(1, 2), 3))
  expect_error(as_argument(NA, expect = 1))
})
