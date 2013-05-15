context("flag validation")

test_that("parsing is ok", {
  expect_equal(valid_flag("op"), "-op")
  expect_equal(valid_flag("-op"), "-op")
  expect_equal(valid_flag(" -op "), "-op")
  expect_equal(valid_flag(" op   "), "-op")
  expect_equal(valid_flag("-op   "), "-op")
  # edge case
  expect_equal(valid_flag(" - o p   "), "-op")
  # long options
  expect_equal(valid_flag("h", mark = "--"), "--h")
})
