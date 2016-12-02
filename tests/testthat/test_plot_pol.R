context("Plotting data with plot_pol")

test_that("Correct input results in a plot", {

  data <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12),
    'sd' = c(1, 1, 1, 1),
    'p.value' = c(1, 0.0213, 1, 0.0010)
  )

  expect_is(plot_pol(data), "ggplot")
})

test_that("p.value and p.star labels work", {

  data <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12),
    'sd' = c(1, 1, 1, 1),
    'p.value' = c(1, 0.0213, 1, 0.0010)
  )

  expect_is(plot_pol(data, label = "p.value"), "ggplot")
  expect_is(plot_pol(data, label = "p.star"), "ggplot")
})

test_that("Produces warning if label not found", {
  data <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12),
    'sd' = c(1, 1, 1, 1)
  )

  expect_is(plot_pol(data, label = "p.value"), "ggplot") #is this OK?
  expect_warning(plot_pol(data, label = "p.value"), "Unrecognized or missing label")
})

test_that("Fails if required column missing", {
  data <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12)
  )

  expect_error(plot_pol(data), "df is missing column")
})

test_that("Can get plot from untidy data", {

  data <- tibble::tibble(
    'Ctrl-Ctrl' = 1:3,
    'M1-Ctrl'= 1:3,
    'Ctrl-LPS' = 4:6,
    'M1-LPS' = 11:13
  )

  p <- data %>%
    tidy_pol() %>%
    analyse_pol() %>%
    plot_pol()

  expect_is(p, "ggplot")

})
