context("Tidying data with tidy_pol")


test_that("tidy_pol tidies a data set", {

  data <- tibble::tibble(
   'Ctrl-Ctrl' = 1:3,
   'M1-Ctrl'= 1:3,
   'Ctrl-LPS' = 4:6,
   'M1-LPS' = 11:13
   )
  tidy <- tibble::tibble(
    'primary' = rep(rep(c("Ctrl", "M1"), each=3), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=6),
    'response' = as.integer(c(1, 2, 3, 1, 2, 3, 4, 5, 6, 11, 12, 13))
  )

  expect_identical(tidy_pol(data), tidy)
})

test_that("tidy_pol produces error for non-numeric data", {
  not_numeric <- tibble::tibble(
    'Ctrl-Ctrl' = c("1", "2", "3"),
    'M1-Ctrl'= 1:3,
    'Ctrl-LPS' = 4:6,
    'M1-LPS' = 11:13
  )
  expect_error(tidy_pol(not_numeric), "Input data must be numeric")
})

test_that("tidy_pol produces error if separator missing", {
  no_sep <- tibble::tibble(
    'Ctrl' = 1:3,
    'M1-Ctrl'= 1:3,
    'Ctrl-LPS' = 4:6,
    'M1-LPS' = 11:13
  )
  expect_error(tidy_pol(no_sep), "Column names must contain separator")
})
