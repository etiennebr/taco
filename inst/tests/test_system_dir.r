context("specific system_dir command")

test_that("system_dir", {
  expect_equal(sort(system_dir(".", "ls", stdout = TRUE)), sort(list.files(".")))
  expect_equal(sort(system_dir("./", "ls", stdout = TRUE)), sort(list.files("./")))
  expect_warning(system_dir(".", "git commit"))
})
