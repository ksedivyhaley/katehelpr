context("Tidying data")


test_that("tidy_pol tidies a data set", {

  data <- tibble::tibble(
   'Ctrl-Ctrl' = 1:3,
   'M1-Ctrl'= 1:3,
   'Ctrl-LPS' = 4:6,
   'M1-LPS' = 11:13
   )
  tidy <- tibble::tibble(
    'polarization' = rep(rep(c("Ctrl", "M1"), each=3), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=6),
    'response' = as.integer(c(1, 2, 3, 1, 2, 3, 4, 5, 6, 11, 12, 13))
  )

  expect_identical(tidy_pol(data), tidy)
})
