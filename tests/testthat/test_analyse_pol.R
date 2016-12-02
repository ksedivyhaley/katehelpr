context("Analysing data with analyse_pol")

test_that("Correct analysis performed",{

  analysed <- tibble::tibble(
    'primary' = rep(rep(c("Ctrl", "M1"), each=3), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=6),
    'response' = c(1, 2, 3, 4, 5, 6, 4, 5, 6, 11, 12, 13) ) %>%
    analyse_pol() %>%
    mutate(p.value = round(p.value, 4)) #otherwise can't get pvalue within margin

  expected <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12),
    'sd' = c(1, 1, 1, 1),
    'p.value' = c(1, 0.0213, 1, 0.0010)
  )
  expect_identical(analysed$primary, expected$primary)
  expect_identical(analysed$secondary, expected$secondary)
  expect_identical(analysed$p.value, expected$p.value)
})

test_that("NA handling works",{

  na_default <- tibble::tibble(
    'primary' = rep(rep(c("Ctrl", "M1"), each=3), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=6),
    'response' = c(1, 2, 3, 4, NA, 6, 4, 5, 6, 11, 12, 13) ) %>%
    analyse_pol() %>%
    mutate(sd = round(sd, 4),
      p.value = round(p.value, 4))

  expected_default <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, 5, 5, 12),
    'sd' = c(1, 1.4142, 1, 1),
    'p.value' = c(1, 0.1444, 1, 0.0010)
  )

  na_include <- tibble::tibble(
    'primary' = rep(rep(c("Ctrl", "M1"), each=3), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=6),
    'response' = c(1, 2, 3, 4, NA, 6, 4, 5, 6, 11, 12, 13) ) %>%
    analyse_pol(na.rm=FALSE) %>%
    mutate(sd = round(sd, 4),
           p.value = round(p.value, 4))

  expected_NA <- tibble::tibble(
    'primary' = rep(c("Ctrl", "M1"), 2),
    'secondary'= rep(c("Ctrl", "LPS"), each=2),
    'mean' = c(2, NA, 5, 12),
    'sd' = c(1, NA, 1, 1),
    'p.value' = c(1, 0.1444, 1, 0.0010)
  )

  expect_identical(na_default, expected_default)
  expect_identical(na_include, expected_NA)
})

test_that("Alternate reference can be set",{

  alt_ref <- tibble::tibble(
       'primary' = rep(rep(c("WT", "Mutant"), each=3), 2),
       'secondary'= rep(c("Diet1", "Diet2"), each=6),
       'response' = c(1, 2, 3, 4, 5, 6, 4, 5, 6, 11, 12, 13) ) %>%
    analyse_pol(reference = "WT") %>%
    mutate(p.value = round(p.value, 4))

  alt_ref_expected <- tibble::tibble(
    'primary' = rep(c("WT", "Mutant"), 2),
    'secondary'= rep(c("Diet1", "Diet2"), each=2),
    'mean' = c(2, 5, 5, 12),
    'sd' = c(1, 1, 1, 1),
    'p.value' = c(1, 0.0213, 1, 0.0010)
  )
  expect_identical(alt_ref, alt_ref_expected)
})

test_that("Produces error if required columns absent",{
  no_secondary <- tibble::tibble(
    'primary' = rep(rep(c("WT", "Mutant"), each=3), 2),
    'response' = c(1, 2, 3, 4, 5, 6, 4, 5, 6, 11, 12, 13)
  )
  expect_error(analyse_pol(no_secondary), "df is missing column")
})
