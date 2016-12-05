context("Testing entire pipeline")

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

# Issues with system.file() cause test failure.

#test_that("Correct analysis performed",{

#  file1 <-  system.file("extdata", "test.csv", package = "katehelpr")

#  list.files(gsub("/test.csv", "/", file1))

#  p <- read.csv(file1) %>%
#    tidy_pol(sep="\\.") %>%
#    analyse_pol() %>%
#    plot_pol(ylab="Inflammation", label="p.star")

#  expect_is(p, "ggplot")
#})
