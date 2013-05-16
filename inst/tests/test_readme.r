context("git command using system_dir")

test_that("readme example", {
  commit <- function(message = NA, all = TRUE, indir = getwd()) {
    arg <- paste( "commit",
      as_argument("a", all), 
      as_argument("m", message, mandatory = 1) # make message mandatory
    )
    system_dir(indir, "git", arg)
  }
  expect_error(commit())
})
