context("Plotting data with plot_pol")

data <- tibble::tibble(
  'primary' = rep(c("Ctrl", "M1"), 2),
  'secondary'= rep(c("Ctrl", "LPS"), each=2),
  'mean' = c(2, 5, 5, 12),
  'sd' = c(1, 1, 1, 1),
  'p.value' = c(1, 0.0213, 1, 0.0010)
)

test_that("Correct input results in a plot", {

  p <- plot_pol(data, xlab="X", ylab="Y")

  expect_is(p, "ggplot")
  expect_is(p$facet, "FacetWrap")
  expect_equal(as.character(p$mapping$x), "primary")
  expect_equal(as.character(p$mapping$y), "mean")
  expect_equal(p$labels$x, "X")
  expect_equal(p$labels$y, "Y")
  expect_equal(p$labels$ymin, "mean - sd")
})

test_that("p.value and p.star labels work", {

  p_num <- plot_pol(data, label = "p.value")
  p_star <- plot_pol(data, label = "p.star")

  expect_is(p_num, "ggplot")
  expect_equal(p_num$labels$label,
               "paste0(\"p=\", round(p.value, 4))")

  expect_is(p_star, "ggplot")
  expect_equal(p_star$labels$label, "p.star")
})

test_that("Produces warning, no label if label not found", {

  data_no_p <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12),
    'sd' = c(1, 1, 1, 1)
  )

  p <- plot_pol(data_no_p, label = "p.value")

  expect_is(p, "ggplot")
  expect_equal(p$labels$label, NULL)
  expect_warning(plot_pol(data_no_p, label = "p.value"),
                 "Unrecognized or missing label")
})

test_that("Fails if required column missing", {

  data_error <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12)
  )

  expect_error(plot_pol(data_error), "df is missing column")
})
